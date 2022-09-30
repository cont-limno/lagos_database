rm(list=ls())
library(tidyverse)
library(lubridate)
library(fs)
library(RPostgreSQL)
library(robustbase)

coalesce_join <- function(x, y, 
                          by = NULL, suffix = c(".x", ".y"), 
                          join = dplyr::full_join, ...) {
    joined <- join(x, y, by = by, suffix = suffix, ...)
    # names of desired output
    cols <- union(names(x), names(y))
    
    to_coalesce <- names(joined)[!names(joined) %in% cols]
    suffix_used <- suffix[ifelse(endsWith(to_coalesce, suffix[1]), 1, 2)]
    # remove suffixes and deduplicate
    to_coalesce <- unique(substr(
        to_coalesce, 
        1, 
        nchar(to_coalesce) - nchar(suffix_used)
    ))
    
    coalesced <- purrr::map_dfc(to_coalesce, ~dplyr::coalesce(
        joined[[paste0(.x, suffix[1])]], 
        joined[[paste0(.x, suffix[2])]]
    ))
    names(coalesced) <- to_coalesce
    
    dplyr::bind_cols(joined, coalesced)[cols]
}

ident.outlier <- function(x){
    # x <- temp$ResultMeasureValue
    if(length(x)>=3) {
        dat.gap <- data.frame(x = sort(unique(x),decreasing = TRUE))
        xrange <- quantile(x,probs=0.99,na.rm=T)
        xrange.9 <- quantile(x,probs=0.90,na.rm=T)
        dat.gap <- dat.gap %>% mutate(gap = c(0,diff(x)) *-1) %>% 
            arrange(desc(gap)) %>% 
            filter(x>xrange.9) %>%
            filter(gap > 1.5*xrange)
        # dat.gap$size[1] <- 1
        # dat.gap$size[2] <- dat.gap$gap[2]/dat.gap$gap[1]
        # dat.gap$size[3] <- dat.gap$gap[3]/dat.gap$gap[1]
        # dat.gap <- dat.gap %>% filter(size >0.75)
        # dat.gap <- dat.gap %>% filter(gap >1.5*xrange)
        if(nrow(dat.gap>=1)){
            thresh <- min(dat.gap$x)
        } else {
            thresh=max(x,na.rm=T)+1
        }
    } else {thresh=max(x,na.rm=T)+1}
    return(thresh)
}

ratio <- function(var_less,var_more){
    temp2 <- dat %>% filter(CharacteristicName == var_more | CharacteristicName == var_less) %>% 
        select(event_id,CharacteristicName,ResultMeasureValue,Obs_Id)
    print(unique(temp2$CharacteristicName))
    
    temp3 <- temp2 %>% 
        pivot_wider(names_from = CharacteristicName,values_from = ResultMeasureValue,id_cols = event_id) %>% 
        drop_na() %>% 
        mutate(ratio = (!!as.name(var_less))/(!!as.name(var_more))) %>% 
        filter(ratio >s.ratio) %>% 
        mutate(flag = "RATIO")
    
    temp2 <- temp2 %>% left_join(temp3 %>% select(event_id,flag)) %>% 
        filter(!is.na(flag))
    
    if(is.null(flagdata)) {
        flagdata <- temp2$Obs_Id
    } else {
        flagdata <- c(flagdata,temp2$Obs_Id)
    }
    return(flagdata)
}
#info for connecting to postgres DB

# dbDisconnect(con)
# drv <- dbDriver("PostgreSQL")
# con <- dbConnect(drv, dbname = "lagos_us_limno",
#                  host = '144.92.62.199', port = 5432,
#                  user = "postgres", password = 'SparklingRusty')

#read in the datafiles

data_dir <- "~/Lottig Dropbox/Noah Lottig/CL_LAGOSUS_exports/LAGOSUS_LIMNO/US_NEW/"
#list files
dat <- read_rds(file = paste0(data_dir,"limnous.rds"))
# temp <- read_rds("/Users/noahlottig/Lottig Dropbox/Noah Lottig/CL_LAGOSUS_exports/LAGOSUS_LIMNO/US_NEW/WQX_Final.rds")


#program specific changes
temp <- dat %>% filter(OrganizationIdentifier=="SDWRAP")
dat <- dat %>% filter(!Obs_Id %in% temp$Obs_Id) #per SD, delete all SDWRAP data
#remove hypersaline lakes
great.salt <- c(455519,455679,455700,455786,455881,455996,456134,456189,456262,456377,455356)
dat <- dat %>% filter(!lagoslakeid %in% great.salt)

temp <- dat %>% filter(CharacteristicName=="Conductivity") %>% 
    # filter(source_unit=="mho/cm") %>% 
    filter(source_unit=="mS/cm") %>% 
    filter(ResultMeasureValue>1000)
dat <- dat %>% filter(!Obs_Id %in% temp$Obs_Id)
temp <- dat %>% filter(CharacteristicName=="Conductivity") %>% 
    # filter(source_unit=="mho/cm") %>% 
    filter(source_unit=="mho/cm") 
dat <- dat %>% filter(!Obs_Id %in% temp$Obs_Id)
gc()


temp <- dat %>%
    filter(if_any(everything(), ~str_detect(tolower(.), "x-sec location horizontal")))

dat <- dat %>% filter(!Obs_Id %in% temp$Obs_Id)

#assign sampledate
dat <- dat %>% rename(sampledate = ActivityStartDate)

states_in_data<-unique(dat$State)
length(unique(dat$State))
#Add Activity data
activities <- read_csv("big_data/activities.csv", 
                       col_types = cols(ActivityStartDate = col_date(format = "%Y-%m-%d"), 
                                        ActivityRelativeDepthName = col_character()))
activities <- activities %>% distinct() %>% 
    rename(sampledate = ActivityStartDate)

activities <- activities %>% group_by(OrganizationIdentifier,ActivityIdentifier,sampledate) %>% 
    mutate(n=n()) %>% ungroup() %>% filter(n==1) %>% select(-n)

dat <- dat %>% left_join(activities)

temp <- dat %>% group_by(State) %>% 
    summarize(n=sum(!is.na(ActivityRelativeDepthName)))
rm(activities)
gc()
#check to make sure no weird sample fractions and activity types made it in
#make sure these get excluded 


#choose only samples where we can use the depth
unique(dat$ActivityDepthAltitudeReferencePointText)

depth_reference_locations <- unique(dat$ActivityDepthAltitudeReferencePointText)
types_to_keep <- c("SURFACE","WATER SURFACE","Surface","WATER SURF","Water Surface","From Surface","EPI","Land surface")
depth_reference_locations <- depth_reference_locations[which(!depth_reference_locations %in% types_to_keep)] 
depth_reference_locations
temp<- dat %>% select(Obs_Id,ActivityDepthAltitudeReferencePointText) %>%  filter(is.na(ActivityDepthAltitudeReferencePointText))
temp2 <- dat %>% select(Obs_Id,ActivityDepthAltitudeReferencePointText) %>% filter(ActivityDepthAltitudeReferencePointText %in% types_to_keep)
temp <- rbind(temp,temp2)
dat <- dat %>% filter(Obs_Id %in% temp$Obs_Id)
rm(temp)
rm(temp2)
gc()


#assign known depths that are specified
# dat <- dat %>% mutate_at(c('ActivityDepthHeightMeasure.MeasureValue',
#                            'ResultDepthHeightMeasure.MeasureValue',
#                            'ActivityBottomDepthHeightMeasure.MeasureValue',
#                            'ActivityTopDepthHeightMeasure.MeasureValue')
#                          ,as.numeric)
dat$source_sampledepth <- dat$ActivityDepthHeightMeasure.MeasureValue
temp <- dat %>% filter(is.na(source_sampledepth)) %>% 
    filter(!is.na(ResultDepthHeightMeasure.MeasureValue))
dat$source_sampledepth[which(dat$Obs_Id %in% temp$Obs_Id)] <- temp$ResultDepthHeightMeasure.MeasureValue

#transfer reported depth values that show up as parameters to depth of sample
temp<- dat %>% filter(source_parameter=="Depth") %>% 
    select(ResultMeasureValue,OrganizationFormalName,ActivityIdentifier,MonitoringLocationIdentifier,sampledate) %>% 
    distinct() %>% 
    group_by(OrganizationFormalName,ActivityIdentifier,MonitoringLocationIdentifier,sampledate) %>% 
    mutate(n=n()) %>% 
    ungroup() %>% 
    filter(n==1) %>% 
    select(-n) %>% 
    rename(reported_depth=ResultMeasureValue)

dat <- dat %>% left_join(temp)
dat <- dat %>% 
    mutate(source_sampledepth = ifelse(is.na(source_sampledepth),reported_depth,source_sampledepth))
dat <- dat %>% filter(CharacteristicName !="Depth")
rm(temp)
gc()

#extract profile data
unique(dat$source_parameter)
meter_vars <-c("Conductivity",
               "Specific conductance",
               "Dissolved oxygen (DO)",
               "Oxygen",
               "pH",
               "Salinity",
               "Turbidity",
               "Turbidity Field",
               "Depth")

dat_profile <- dat %>% 
    select(sampledate,MonitoringLocationIdentifier,OrganizationIdentifier,CharacteristicName,source_sampledepth,Obs_Id,source_parameter) %>% 
    filter(source_parameter %in% meter_vars)

dat_profile_keep <- dat_profile %>% 
    filter(!is.na(source_sampledepth)) %>% 
    group_by(sampledate,MonitoringLocationIdentifier,OrganizationIdentifier,CharacteristicName) %>% 
    arrange(source_sampledepth, .by_group = TRUE) %>% 
    slice(1)

dat_profile <- dat_profile %>% filter(!Obs_Id %in% dat_profile_keep$Obs_Id)
dat <- dat %>% filter(!Obs_Id %in% dat_profile$Obs_Id)
rm(dat_profile)
rm(dat_profile_keep)
gc()

dat$source_sampleposition <- NA
temp <- dat %>% filter(is.na(source_sampledepth)) %>% 
    filter(!is.na(ActivityBottomDepthHeightMeasure.MeasureValue))
dat$source_sampledepth[which(dat$Obs_Id %in% temp$Obs_Id)] <- temp$ActivityBottomDepthHeightMeasure.MeasureValue
temp <- dat %>% filter(is.na(source_sampledepth)) %>% 
    filter(!is.na(ActivityTopDepthHeightMeasure.MeasureValue))
dat$source_sampledepth[which(dat$Obs_Id %in% temp$Obs_Id)] <- temp$ActivityTopDepthHeightMeasure.MeasureValue
temp <- dat %>% filter(is.na(source_sampledepth))
rm(temp)

#exclude variables
gc()
# dat <- dat %>% filter(CharacteristicName !="Oxygen, dissolved")

#If program doesn't record sample depth but records epi sample...asign 0 source depth
good.depths <- unique(dat$ActivityRelativeDepthName)
bad_reldepths <- c("Near Bottom","Midwater","Bottom","BelowThermoclin","Above Halocline","Other","Below Halocline")
good.depths <- good.depths[which(!good.depths %in% bad_reldepths)]
good.depths
dat <- dat %>% filter(!ActivityRelativeDepthName %in% bad_reldepths)
unique(dat$ActivityRelativeDepthName)
rm(good.depths)
rm(bad_reldepths)
gc()
temp <- dat %>% filter(is.na(source_sampledepth)) %>% 
    filter(ActivityRelativeDepthName=="Surface" | 
                           ActivityRelativeDepthName == "Photic zone" |
                           ActivityRelativeDepthName == "AboveThermoclin")
unique(temp$ActivityRelativeDepthName)
dat$source_sampledepth[which(dat$Obs_Id %in% temp$Obs_Id)] <- 0
dat$source_sampleposition[which(dat$Obs_Id %in% temp$Obs_Id)] <- "INFERRED"
gc()

good.depths <- unique(dat$ActivityDepthAltitudeReferencePointText)[!is.na(unique(dat$ActivityDepthAltitudeReferencePointText))]
good.depths 

temp <- dat %>% filter(is.na(source_sampledepth)) %>% 
    filter(ActivityDepthAltitudeReferencePointText %in% good.depths)
dat$source_sampledepth[which(dat$Obs_Id %in% temp$Obs_Id)] <- 0
dat$source_sampleposition[which(dat$Obs_Id %in% temp$Obs_Id)] <- "INFERRED"


#assign depth type to samples with known assigned sample depths
dat$source_sampleposition <- ifelse(is.na(dat$source_sampledepth),"UNKNOWN",dat$source_sampleposition)
dat$source_sampleposition <- ifelse(is.na(dat$source_sampleposition),"SPECIFIED",dat$source_sampleposition)
unique(dat$source_sampleposition)
dat$lagos_sampledepth <- dat$source_sampledepth
dat$lagos_sampleposition <- dat$source_sampleposition

#assign secchi to depth of 0
dat$lagos_sampledepth[which(dat$CharacteristicName=="Secchi")] <- 0
dat$lagos_sampleposition[which(dat$CharacteristicName=="Secchi")] <- "SPECIFIED"


#remove repeated observations when reported values are different (still keep duplicates)
temp <- dat %>% filter(lagos_sampleposition=="UNKNOWN")
temp <- temp %>% group_by(sampledate,MonitoringLocationIdentifier,OrganizationIdentifier,CharacteristicName) %>% 
    mutate(n=length(unique(ResultMeasureValue))) %>% 
    ungroup() %>% 
    filter(n >1) %>% 
    select(sampledate,MonitoringLocationIdentifier,OrganizationIdentifier,CharacteristicName,Obs_Id,n,ResultMeasureValue) %>% 
    arrange(OrganizationIdentifier,MonitoringLocationIdentifier,CharacteristicName,sampledate)
dat <- dat %>% filter(!Obs_Id %in% temp$Obs_Id)
rm(temp)
gc()

#infer unknown depths based on observed depths within a sampling program
dat <- dat %>% mutate(combined_name = paste(OrganizationFormalName,ActivityConductingOrganizationText))
temp <- dat %>% group_by(combined_name) %>% 
    summarize(depths = sum(!is.na(lagos_sampledepth)),
              no_depths = sum(is.na(lagos_sampledepth)),
              proport_depth = round(sum(is.na(lagos_sampledepth))/(sum(!is.na(lagos_sampledepth))+sum(is.na(lagos_sampledepth)))*100,2),
              depth_75 = round(quantile(lagos_sampledepth,na.rm=TRUE,probs=0.75),1),
              sd_depth = round(sd(lagos_sampledepth,na.rm=TRUE),2)) %>% 
    filter(no_depths >0)
write_csv(temp,"no_depth_summary.csv")

temp2 <- temp %>% filter(depth_75 <=2) %>% 
    filter(sd_depth <=2) %>% 
    filter(proport_depth<=90) %>% 
    rename(lagos_sampledepth = depth_75)
dat <- coalesce_join(dat,temp2 %>% select(combined_name,lagos_sampledepth),by="combined_name")

temp <- dat %>% group_by(combined_name) %>% 
    summarize(depths = sum(!is.na(lagos_sampledepth)),
              no_depths = sum(is.na(lagos_sampledepth)),
              proport_depth = round(sum(is.na(lagos_sampledepth))/(sum(!is.na(lagos_sampledepth))+sum(is.na(lagos_sampledepth)))*100,2),
              depth_75 = round(quantile(lagos_sampledepth,na.rm=TRUE,probs=0.75),1),
              sd_depth = round(sd(lagos_sampledepth,na.rm=TRUE),2)) %>% 
    filter(no_depths >0)
temp2 <- temp %>% filter(depth_75 ==0) %>% 
    filter(sd_depth ==0) %>% 
    rename(lagos_sampledepth = depth_75)
dat <- coalesce_join(dat,temp2 %>% select(combined_name,lagos_sampledepth),by="combined_name")
rm(temp)
rm(temp2)
gc()


#assign grabs with grab equipment as inferred 0 depth
temp <- dat %>% filter(is.na(lagos_sampledepth))
equip <- unique(temp$SampleCollectionEquipmentName)
equip_to_keep <- c("Water Bottle",
                   "Open-Mouth Bottle","Grab sample","Bucket", "Open-top bailer")
equip <- equip[which(!equip %in% equip_to_keep)]
equip
temp <- temp %>% filter(SampleCollectionEquipmentName %in% equip_to_keep) %>% 
    filter(if_any(everything(), ~str_detect(tolower(.), "grab")))
dat$lagos_sampledepth[which(dat$Obs_Id %in% temp$Obs_Id)] <- 0
dat$lagos_sampleposition[which(dat$Obs_Id %in% temp$Obs_Id)] <- "INFERRED"
gc()





temp <- dat %>% filter(is.na(lagos_sampledepth)) %>% 
    group_by(combined_name) %>% 
    summarize(n=n())



#look at what is left with unassigned depths
temp <- dat %>% group_by(State) %>% 
    summarize(depths = sum(!is.na(lagos_sampledepth)),
              no_depths = sum(is.na(lagos_sampledepth)),
              proport_depth = round(sum(is.na(lagos_sampledepth))/(sum(!is.na(lagos_sampledepth))+sum(is.na(lagos_sampledepth)))*100,0),
              depth_75 = round(quantile(lagos_sampledepth,na.rm=TRUE,probs=0.75),1),
              sd_depth = round(sd(lagos_sampledepth,na.rm=TRUE),2)) 
write_csv(temp,"statesummary.csv")

####code to deal with remaining samples that don't have assigned depths
temp <- dat %>% filter(is.na(lagos_sampledepth))
dat <- dat %>% filter(!Obs_Id %in% temp$Obs_Id)
rm(temp)
rm(temp2)
gc()

# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# temp <- dat %>% filter(OrganizationFormalName=="Georgia DNR Environmental Protection Division") %>% 
#     filter(is.na(lagos_sampledepth))
# unique(temp$CharacteristicName)
# unique(temp$ActivityConductingOrganizationText)
# dat$lagos_sampledepth[which(dat$Obs_Id %in% temp$Obs_Id)] <- 0
# dat$lagos_sampleposition[which(dat$Obs_Id %in% temp$Obs_Id)] <- "EPI_COMPOSITE"
# 
# temp <- dat %>% filter(OrganizationFormalName=="Texas Commission on Environmental Quality") %>% 
#     filter(is.na(lagos_sampledepth))
# unique(temp$CharacteristicName)
# unique(temp$ActivityConductingOrganizationText)
# dat$lagos_sampledepth[which(dat$Obs_Id %in% temp$Obs_Id)] <- 0
# dat$lagos_sampleposition[which(dat$Obs_Id %in% temp$Obs_Id)] <- "SPECIFIED"
# 
# 
# temp <- dat %>% filter(is.na(lagos_sampledepth)) %>% 
#     filter(OrganizationFormalName == "Utah Department Of Environmental Quality") %>% 
#     filter(if_any(everything(), ~str_detect(tolower(.), "below thermo")))
# dat <- dat %>% filter(!Obs_Id %in% temp$Obs_Id)
# 
# temp <- dat %>% filter(is.na(lagos_sampledepth)) %>% 
#     filter(OrganizationFormalName == "Utah Department Of Environmental Quality") 
# unique(temp$ActivityConductingOrganizationText)
# temp <- temp %>% filter(ActivityConductingOrganizationText== "Division of Water Quality")
# dat$lagos_sampledepth[which(dat$Obs_Id %in% temp$Obs_Id)] <- 0.2
# dat$lagos_sampleposition[which(dat$Obs_Id %in% temp$Obs_Id)] <- "SPECIFIED"
# 
# 
# temp <- dat %>% filter(is.na(lagos_sampledepth)) %>% 
#     filter(OrganizationFormalName == "Orlando Streets Drainage Stormwater Utility Bureau(Florida)")
# dat$lagos_sampledepth[which(dat$Obs_Id %in% temp$Obs_Id)] <- 0
# dat$lagos_sampleposition[which(dat$Obs_Id %in% temp$Obs_Id)] <- "SPECIFIED"
# 
# temp <- dat %>%  
#     filter(OrganizationFormalName == "Arkansas Department of Environmental Quality")
# dat$lagos_sampledepth[which(dat$Obs_Id %in% temp$Obs_Id)] <- 0
# dat$lagos_sampleposition[which(dat$Obs_Id %in% temp$Obs_Id)] <- "SPECIFIED"
# 
# 
# temp <- dat %>% filter(is.na(lagos_sampledepth)) %>% 
#     filter(OrganizationFormalName == "Nebraska Department of Environment and Energy")
# dat$lagos_sampledepth[which(dat$Obs_Id %in% temp$Obs_Id)] <- 0
# dat$lagos_sampleposition[which(dat$Obs_Id %in% temp$Obs_Id)] <- "SPECIFIED"
# 
# 
# 
# temp <- dat %>% filter(is.na(lagos_sampledepth)) %>% 
#     filter(OrganizationFormalName == "DENR") %>% 
#     filter(sampledate >= ymd("2007-12-31"))
# dat$lagos_sampledepth[which(dat$Obs_Id %in% temp$Obs_Id)] <- 0
# dat$lagos_sampleposition[which(dat$Obs_Id %in% temp$Obs_Id)] <- "EPI_INT"
# 
# 
# temp <- dat %>% group_by(OrganizationFormalName) %>% 
#     summarize(depths = sum(!is.na(lagos_sampledepth)),no_depths = sum(is.na(lagos_sampledepth))) %>% 
#     filter(no_depths>99)
# write_csv(temp,"no_depth_info.csv")
# 
# 
# 
# temp <- dat %>% filter(is.na(lagos_sampledepth)) %>% 
#     filter(OrganizationFormalName == "McGlynn Laboratories, Inc (Florida)")
# 
# 
# temp <- dat %>% filter(is.na(lagos_sampledepth)) %>% 
#     filter(OrganizationFormalName == "National Park Service Water Resources Division")
# 
# unique(temp$SampleCollectionEquipmentName)
# 
# 
# temp <- dat %>% group_by(OrganizationFormalName) %>% 
#     summarize(depths = sum(!is.na(lagos_sampledepth)),no_depths = sum(is.na(lagos_sampledepth))) %>% 
#     filter(no_depths>99)
# write_csv(temp,"no_depth_info.csv")
# 
# temp <- dat %>% filter(is.na(lagos_sampledepth)) %>% 
#     filter(OrganizationFormalName=="DENR")
# 
# temp <- dat %>% filter(is.na(lagos_sampledepth)) 
# temp <- temp %>% group_by(sampledate,MonitoringLocationIdentifier,OrganizationIdentifier,CharacteristicName) %>% 
#     mutate(n=length(unique(ResultMeasureValue))) %>% 
#     ungroup() %>% 
#     filter(n >1) %>% 
#     select(sampledate,MonitoringLocationIdentifier,OrganizationIdentifier,CharacteristicName,Obs_Id,n,ResultMeasureValue) %>% 
#     arrange(OrganizationIdentifier,MonitoringLocationIdentifier,CharacteristicName,sampledate)
#     
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# temp <- dat %>% filter(OrganizationFormalName=="Utah Department Of Environmental Quality") %>% 
#     filter(is.na(lagos_sampledepth)) 
# 
# %>% 
#     filter(ActivityTypeCode=="Sample-Integrated Vertical Profile") %>% 
#     filter(!is.na(ActivityBottomDepthHeightMeasure.MeasureValue))
# dat$lagos_sampledepth[which(dat$Obs_Id %in% temp$Obs_Id)] <- temp$ActivityBottomDepthHeightMeasure.MeasureValue
# dat$lagos_sampleposition[which(dat$Obs_Id %in% temp$Obs_Id)] <- "INTEGRATED"
# 
# 
# 
# 
# 
# 
# 
# #########################################################
# 
# 
# 
# temp <- dat %>% filter(is.na(lagos_sampledepth)) %>% 
#     filter(if_any(everything(), ~str_detect(tolower(.), "at the surf")))
# dat$lagos_sampledepth[which(dat$Obs_Id %in% temp$Obs_Id)] <- 0
# dat$lagos_sampleposition[which(dat$Obs_Id %in% temp$Obs_Id)] <- "SPECIFIED"
# 
# temp <- dat %>% filter(is.na(lagos_sampledepth)) %>% 
#     filter(if_any(everything(), ~str_detect(tolower(.), "loch surf")))
# dat$lagos_sampledepth[which(dat$Obs_Id %in% temp$Obs_Id)] <- 0
# dat$lagos_sampleposition[which(dat$Obs_Id %in% temp$Obs_Id)] <- "SPECIFIED"
# 
# temp <- dat %>% filter(is.na(lagos_sampledepth)) %>% 
#     filter(if_any(everything(), ~str_detect(tolower(.), "lake surf")))
# dat$lagos_sampledepth[which(dat$Obs_Id %in% temp$Obs_Id)] <- 0
# dat$lagos_sampleposition[which(dat$Obs_Id %in% temp$Obs_Id)] <- "SPECIFIED"
# 
# temp <- dat %>% filter(is.na(lagos_sampledepth)) %>% 
#     filter(if_any(everything(), ~str_detect(tolower(.), "pond surf")))
# dat$lagos_sampledepth[which(dat$Obs_Id %in% temp$Obs_Id)] <- 0
# dat$lagos_sampleposition[which(dat$Obs_Id %in% temp$Obs_Id)] <- "SPECIFIED"
# 
# temp <- dat %>% filter(is.na(lagos_sampledepth)) %>% 
#     filter(if_any(everything(), ~str_detect(tolower(.), "surface water grab sample")))
# dat$lagos_sampledepth[which(dat$Obs_Id %in% temp$Obs_Id)] <- 0
# dat$lagos_sampleposition[which(dat$Obs_Id %in% temp$Obs_Id)] <- "SPECIFIED"
# 
# temp <- dat %>% filter(is.na(lagos_sampledepth)) %>% 
#     filter(if_any(everything(), ~str_detect(tolower(.), "at surface")))
# dat$lagos_sampledepth[which(dat$Obs_Id %in% temp$Obs_Id)] <- 0
# dat$lagos_sampleposition[which(dat$Obs_Id %in% temp$Obs_Id)] <- "SPECIFIED"
# 
# temp <- dat %>% filter(is.na(lagos_sampledepth)) %>% 
#     filter(if_any(ProjectIdentifier, ~str_detect(tolower(.), "surface")))
# dat$lagos_sampledepth[which(dat$Obs_Id %in% temp$Obs_Id)] <- 0
# dat$lagos_sampleposition[which(dat$Obs_Id %in% temp$Obs_Id)] <- "SPECIFIED"
# 
# temp <- dat %>% filter(is.na(lagos_sampledepth)) %>% 
#     filter(if_any(everything(), ~str_detect(tolower(.), "3 ft below")))
# dat$lagos_sampledepth[which(dat$Obs_Id %in% temp$Obs_Id)] <- 3*0.3048
# dat$lagos_sampleposition[which(dat$Obs_Id %in% temp$Obs_Id)] <- "SPECIFIED"
# 
# temp <- dat %>% filter(is.na(lagos_sampledepth)) %>% 
#     filter(tolower(ResultCommentText)=="surface")
# dat$lagos_sampledepth[which(dat$Obs_Id %in% temp$Obs_Id)] <- 0
# dat$lagos_sampleposition[which(dat$Obs_Id %in% temp$Obs_Id)] <- "SPECIFIED"
# 
# temp <- dat %>% filter(is.na(lagos_sampledepth)) %>% 
#     filter(tolower(ResultCommentText)=="surface sample")
# dat$lagos_sampledepth[which(dat$Obs_Id %in% temp$Obs_Id)] <- 0
# dat$lagos_sampleposition[which(dat$Obs_Id %in% temp$Obs_Id)] <- "SPECIFIED"
# 
# temp <- dat %>% filter(is.na(lagos_sampledepth)) %>% 
#     filter(tolower(ActivityCommentText)=="surface sample")
# dat$lagos_sampledepth[which(dat$Obs_Id %in% temp$Obs_Id)] <- 0
# dat$lagos_sampleposition[which(dat$Obs_Id %in% temp$Obs_Id)] <- "SPECIFIED"
# 
# temp <- dat %>% filter(is.na(lagos_sampledepth)) %>% 
#     filter(tolower(ActivityCommentText)=="surface")
# dat$lagos_sampledepth[which(dat$Obs_Id %in% temp$Obs_Id)] <- 0
# dat$lagos_sampleposition[which(dat$Obs_Id %in% temp$Obs_Id)] <- "SPECIFIED"
# 
# temp <- dat %>% filter(is.na(lagos_sampledepth)) %>% 
#     filter(if_any(everything(), ~str_detect(tolower(.), "1 ft below")))
# dat$lagos_sampledepth[which(dat$Obs_Id %in% temp$Obs_Id)] <- 1*0.3048
# dat$lagos_sampleposition[which(dat$Obs_Id %in% temp$Obs_Id)] <- "SPECIFIED"
# 
# temp <- dat %>% filter(is.na(lagos_sampledepth)) %>% 
#     filter(if_any(everything(), ~str_detect(tolower(.), "1m below")))
# dat$lagos_sampledepth[which(dat$Obs_Id %in% temp$Obs_Id)] <- 1
# dat$lagos_sampleposition[which(dat$Obs_Id %in% temp$Obs_Id)] <- "SPECIFIED"
# 
# temp <- dat %>% filter(is.na(lagos_sampledepth)) %>% 
#     filter(if_any(everything(), ~str_detect(tolower(.), "station 2 surface")))
# dat$lagos_sampledepth[which(dat$Obs_Id %in% temp$Obs_Id)] <- 0
# dat$lagos_sampleposition[which(dat$Obs_Id %in% temp$Obs_Id)] <- "SPECIFIED"
# 
# temp <- dat %>% filter(is.na(lagos_sampledepth)) %>% 
#     filter(if_any(everything(), ~str_detect(tolower(.), "30ft below")))
# dat$lagos_sampledepth[which(dat$Obs_Id %in% temp$Obs_Id)] <- 30*0.3048
# dat$lagos_sampleposition[which(dat$Obs_Id %in% temp$Obs_Id)] <- "SPECIFIED"
# 
# temp <- dat %>% filter(is.na(lagos_sampledepth)) %>% 
#     filter(if_any(everything(), ~str_detect(tolower(.), "branch, surface")))
# dat$lagos_sampledepth[which(dat$Obs_Id %in% temp$Obs_Id)] <- 0
# dat$lagos_sampleposition[which(dat$Obs_Id %in% temp$Obs_Id)] <- "SPECIFIED"
# 
# temp <- dat %>% filter(is.na(lagos_sampledepth)) %>% 
#     filter(if_any(everything(), ~str_detect(tolower(.), "dam,surface")))
# dat$lagos_sampledepth[which(dat$Obs_Id %in% temp$Obs_Id)] <- 0
# dat$lagos_sampleposition[which(dat$Obs_Id %in% temp$Obs_Id)] <- "SPECIFIED"
# 
# temp <- dat %>% filter(is.na(lagos_sampledepth)) %>% 
#     filter(if_any(everything(), ~str_detect(tolower(.), "above thermocline")))
# 
# temp <- dat %>% filter(is.na(lagos_sampledepth)) %>% 
#     filter(if_any(everything(), ~str_detect(tolower(.), "photic")))
# dat$lagos_sampledepth[which(dat$Obs_Id %in% temp$Obs_Id)] <- 0
# dat$lagos_sampleposition[which(dat$Obs_Id %in% temp$Obs_Id)] <- "EPI_INT"
# temp <- dat %>% filter(is.na(lagos_sampledepth)) %>% 
#     filter(if_any(everything(), ~str_detect(tolower(.), "epilimnion")))
# dat$lagos_sampledepth[which(dat$Obs_Id %in% temp$Obs_Id)] <- 0
# dat$lagos_sampleposition[which(dat$Obs_Id %in% temp$Obs_Id)] <- "EPI_INT"
# 
# #######################
# #Excludes
# 
# temp <- dat %>% filter(is.na(lagos_sampledepth)) %>%  
#     filter(if_any(everything(), ~str_detect(tolower(.), "bottom")))
# dat <- dat %>% filter(!Obs_Id %in% temp$Obs_Id)
# temp <- dat %>% filter(is.na(lagos_sampledepth)) %>% 
#     filter(if_any(everything(), ~str_detect(tolower(.), "subsurface")))
# dat <- dat %>% filter(!Obs_Id %in% temp$Obs_Id)
# temp <- dat %>% filter(is.na(lagos_sampledepth)) %>% 
#     filter(if_any(everything(), ~str_detect(tolower(.), "below thermocline")))
# dat <- dat %>% filter(!Obs_Id %in% temp$Obs_Id)
# 
# 
# temp <- dat %>% filter(is.na(lagos_sampledepth))
# temp2 <- dat %>% group_by(OrganizationFormalName) %>% 
#     summarize(depth=sum(!is.na(lagos_sampledepth)),no_depth=sum(is.na(lagos_sampledepth)))
# write_csv(temp2, "uknown_depths_program.csv")
# 
# 
# #assign grabs with grab equipment as inferred 0 depth
# temp <- dat %>% filter(is.na(lagos_sampledepth))
# equip <- unique(temp$SampleCollectionEquipmentName)
# equip_to_keep <- c("Water Bottle",
#                    "Open-Mouth Bottle","Grab sample","Bucket")
# equip <- equip[which(!equip %in% equip_to_keep)]
# equip
# temp <- temp %>% filter(!SampleCollectionEquipmentName %in% equip_to_keep)
# dat <- dat %>% filter(!Obs_Id %in% temp$Obs_Id)
# 
# temp <- dat %>% filter(is.na(lagos_sampledepth)) %>% 
#     filter(if_any(everything(), ~str_detect(tolower(.), "grab")))
# dat$lagos_sampledepth[which(dat$Obs_Id %in% temp$Obs_Id)] <- 0
# dat$lagos_sampleposition[which(dat$Obs_Id %in% temp$Obs_Id)] <- "INFERRED"
# 
# temp <- dat %>% filter(is.na(lagos_sampledepth))
# 
# temp <- dat %>% group_by(OrganizationFormalName) %>% 
#     summarize(depths = sum(!is.na(lagos_sampledepth)),no_depths = sum(is.na(lagos_sampledepth))) %>% 
#     filter(no_depths>99)
# 
# write_csv(temp,"no_depth.csv")

#remove repeated samples by randomly selecting one sample but prioritizing specified samples
dat <- dat %>% ungroup()
dat <- as.data.frame(dat)
dat <- dat %>% group_by(sampledate,MonitoringLocationIdentifier,OrganizationIdentifier,lagos_sampledepth,CharacteristicName) %>% 
    mutate(event_id = paste0(sampledate,MonitoringLocationIdentifier,OrganizationIdentifier,lagos_sampledepth,CharacteristicName)) %>% 
    ungroup()
gc()
temp <- dat %>% filter(lagos_sampleposition == "SPECIFIED") %>%  
    group_by(sampledate,MonitoringLocationIdentifier,OrganizationIdentifier,lagos_sampledepth,CharacteristicName) %>% 
    slice_sample(n=1) %>% ungroup()
gc()
temp2 <- dat %>% filter(lagos_sampleposition != "SPECIFIED") %>%  
    group_by(sampledate,MonitoringLocationIdentifier,OrganizationIdentifier,lagos_sampledepth,CharacteristicName) %>% 
    slice_sample(n=1) %>% ungroup()
gc()
temp2 <- temp2 %>% filter(!event_id %in% temp$event_id)
dat <- rbind(temp,temp2)
rm(temp)
rm(temp2)
gc()


#check to see if any odd variable units that should be excluded
unique(dat$ResultSampleFractionText)
unique(dat$source_unit)

# lagos_variables <- dbGetQuery(con,'select * from limno.lagosvariables_us', stringsAsFactors = FALSE) %>% 
#     select(variableid_lagos,variablename,variableunitsid)
# units <- dbGetQuery(con,'select * from limno.units', stringsAsFactors = FALSE) %>% 
#     select(variableunitsid,unitsabbreviation)
# dat <- dat %>% left_join(lagos_variables,by=c("CharacteristicName" = "variablename")) %>% 
#     left_join(units)

#temporary fix of ph label
# dat <- dat %>% mutate(CharacteristicName = case_when(CharacteristicName=='pH, equilibrated' ~'pH, field or lab',TRUE ~ as.character(CharacteristicName)))
# dat <- dat %>% mutate(CharacteristicName = case_when(CharacteristicName=='Phosphorus, soluable reactive orthophosphate' ~'Phosphorus, soluble reactive orthophosphate',TRUE ~ as.character(CharacteristicName)))
# dat <- dat %>% mutate(CharacteristicName = case_when(CharacteristicName=='Nitrogen, total dissolved Kjeldahl' ~'Nitrogen, dissolved Kjeldahl',TRUE ~ as.character(CharacteristicName)))
# dat <- dat %>% mutate(CharacteristicName = case_when(CharacteristicName=='Mercury, dissolved' ~'Mercury, total dissolved',TRUE ~ as.character(CharacteristicName)))

variables <- read_csv("lagos_variable.csv")
dat <- dat %>% select(-variableid_lagos,-limit_low,-limit_high,-variableshortname)
dat <- dat %>% left_join(variables %>% select(variablename,variableid_lagos,limit_low,limit_high,variableshortname), by = c("CharacteristicName" = "variablename"))

##filter egregios values and write to data file
egreg_dat <- dat %>% filter(ResultMeasureValue < limit_low | ResultMeasureValue > limit_high) %>% 
    select(Obs_Id,lagoslakeid,sampledate, ActivityConductingOrganizationText,OrganizationFormalName,
           CharacteristicName,source_parameter,ResultMeasureValue,source_value,source_unit,
           Conversion,DetectionQuantitationLimitMeasure.MeasureValue,
           Method_Id,
           MethodDescriptionText,ActivityCommentText,wqp_monitoringlocationname)
write_csv(egreg_dat,"egregious_values.csv")

dat <- dat %>% filter(!Obs_Id %in% egreg_dat$Obs_Id)
rm(egreg_dat)
gc()

lake_dat <- read_csv("lake_info.csv")
dat <- dat %>% 
    mutate(lagoslakeid = as.numeric(lagoslakeid)) %>% 
    left_join(lake_dat)
rm(lake_dat)
gc()


temp <- dat %>% select(Obs_Id,CharacteristicName,ResultMeasureValue,neon_zoneid) %>% 
    filter(ResultMeasureValue >0) %>% 
    group_by(CharacteristicName, neon_zoneid) %>% 
    mutate(outlier = ident.outlier(ResultMeasureValue)) %>% 
    ungroup() %>% 
    mutate(exclude = ifelse(ResultMeasureValue>outlier,"red","black")) %>% 
    mutate(exclude = ifelse(is.na(exclude),"black",exclude)) %>% 
    select(Obs_Id,exclude)

dat <- dat %>% select(-exclude)
dat <- dat %>% left_join(temp)
dat <- dat %>% mutate(exclude = ifelse(is.na(exclude),"black",exclude))
# temp <- dat %>% filter(exclue)
rm(temp)
gc()

for(i in 1:nrow(variables)){
    var.name <- variables$variablename[i]
    temp <- dat %>% filter(CharacteristicName==var.name) %>% filter(ResultMeasureValue>0)
    if(nrow(temp)>0) {
        p<- ggplot(data = temp,aes(x="",y=ResultMeasureValue,colour=exclude)) + 
            geom_jitter(height=0,alpha=0.5) +
            facet_wrap(vars(neon_zoneid),scales = "free_y") +
            theme_bw() +
            scale_color_identity() +
            labs(x=var.name)
        ggsave(plot = p,filename = paste0("graphics/jpg/",var.name,".jpg"),width=11,height=8.5)
    }
}

saveRDS(dat,"initial_process.rds")

outlier_dat <- dat %>% filter(exclude=="red") %>% 
    select(Obs_Id,lagoslakeid,sampledate, ActivityConductingOrganizationText,OrganizationFormalName,
           CharacteristicName,source_parameter,ResultMeasureValue,source_value,source_unit,
           Conversion,DetectionQuantitationLimitMeasure.MeasureValue,
           Method_Id,
           MethodDescriptionText,ActivityCommentText,wqp_monitoringlocationname)
write_csv(outlier_dat,"outlier_values.csv")

dat <- dat %>% filter(!Obs_Id %in% outlier_dat$Obs_Id)
rm(outlier_dat)
gc()
dat <- dat %>% select(-exclude)









#ratio analysis
dat <- dat %>% group_by(sampledate,MonitoringLocationIdentifier,OrganizationIdentifier,lagos_sampledepth) %>% 
    mutate(event_id = paste0(sampledate,MonitoringLocationIdentifier,OrganizationIdentifier,lagos_sampledepth)) %>% 
    ungroup()

s.ratio = (1+1*1/5)/(1-1*1/5)
s.ratio.l = (1-1*1/5)/(1+1*1/5)
flagdata <- NULL

#tp/tdp
flagdata <- ratio("Phosphorus, total dissolved","Phosphorus, total")
#tp/SRP
flagdata <- ratio("Phosphorus, soluble reactive orthophosphate","Phosphorus, total")
#tdp/srp
flagdata <- ratio("Phosphorus, soluble reactive orthophosphate","Phosphorus, total dissolved")
#tdn/tn
flagdata <- ratio("Nitrogen, total dissolved","Nitrogen, total")
#tkn/tn
flagdata <- ratio("Nitrogen, total Kjeldahl","Nitrogen, total")
#dkn/tn
flagdata <- ratio("Nitrogen, dissolved Kjeldahl","Nitrogen, total")
#dkn/tn
flagdata <- ratio("Nitrogen, dissolved Kjeldahl","Nitrogen, total dissolved")
#dkn/tkn
flagdata <- ratio("Nitrogen, dissolved Kjeldahl","Nitrogen, total Kjeldahl")
#ton/tn
flagdata <- ratio("Nitrogen, total organic","Nitrogen, total")
#no2no3/tn
flagdata <- ratio("Nitrogen, nitrite (NO2) + nitrate (NO3)","Nitrogen, total")
#nh4/tn
flagdata <- ratio("Nitrogen, NH4","Nitrogen, total")
#no2/tn
flagdata <- ratio("Nitrogen, nitrite (NO2)","Nitrogen, total")
#no2no3/tdn
flagdata <- ratio("Nitrogen, nitrite (NO2) + nitrate (NO3)","Nitrogen, total dissolved")
#nh4/tdn
flagdata <- ratio("Nitrogen, NH4","Nitrogen, total dissolved")
#no2/tdn
flagdata <- ratio("Nitrogen, nitrite (NO2)","Nitrogen, total dissolved")
#nh4/tdn
flagdata <- ratio("Nitrogen, NH4","Nitrogen, total Kjeldahl")
#nh4/tdn
flagdata <- ratio("Nitrogen, NH4","Nitrogen, dissolved Kjeldahl")
#doc/toc
flagdata <- ratio("Carbon, dissolved organic","Carbon, total organic")
#dic/tic
flagdata <- ratio("Carbon, dissolved inorganic","Carbon, total inorganic")
#atrazine
flagdata <- ratio("Atrazine, dissolved","Atrazine, total")
#arsenic
flagdata <- ratio("Arsenic, dissolved","Arsenic, total")
#aluminum
flagdata <- ratio("Aluminum, dissolved","Aluminum, total")
#selenium
flagdata <- ratio("Selenium, dissolved","Selenium, total")
#mercury
flagdata <- ratio("Mercury, total dissolved","Mercury, total")
#methylmercury
flagdata <- ratio("Methylmercury, dissolved","Methylmercury")

flagdata <- unique(flagdata)
ratio.data <- dat %>% filter(Obs_Id %in% flagdata) %>% 
    select(Obs_Id,lagoslakeid,sampledate, ActivityConductingOrganizationText,OrganizationFormalName,
           CharacteristicName,source_parameter,ResultMeasureValue,source_value,source_unit,
           Conversion,DetectionQuantitationLimitMeasure.MeasureValue,
           Method_Id,
           MethodDescriptionText,ActivityCommentText,wqp_monitoringlocationname)

write_csv(ratio.data,"ratio_values.csv")

dat <- dat %>% filter(!Obs_Id %in% ratio.data$Obs_Id)
rm(ratio.data)
gc()
dat <- dat %>% select(-event_id)



dat <- dat %>% 
    rename(sample_id = Obs_Id,
           sample_date = sampledate,
           parameter_id = variableid_lagos,
           parameter_name = CharacteristicName,
           parameter_value = ResultMeasureValue,
           parameter_detectionlimit_value = DetectionQuantitationLimitMeasure.MeasureValue,
           sample_depth_flag = lagos_sampleposition,
           sample_depth_m = lagos_sampledepth,
           source_id = OrganizationIdentifier,
           source_name = OrganizationFormalName,
           source_activityid = ActivityIdentifier,
           source_activityorg_name = ActivityConductingOrganizationText,
           source_sample_siteid = MonitoringLocationIdentifier,
           source_parameter_name = source_parameter,
           source_parameter_value = source_value,
           source_parameter_units = source_unit,
           parameter_conversionfactor = Conversion,
           source_detectionlimit_value = detectionlimit_legacy,
           source_detectionlimit_unit = detectionlimit_unit_legacy,
           parameter_detectionlimit_conversionfactor = Conversion_dl,
           source_detectionlimit_condition = ResultDetectionConditionText,
           source_detectionlimit_type = DetectionQuantitationLimitTypeName,
           source_value_qualifiercode = MeasureQualifierCode,
           source_labmethod_description = MethodDescriptionText,
           source_labmethod_id = Method_Id,
           source_labmethod_name = ResultAnalyticalMethod.MethodName,
           wqp_parameter_usgspcode = USGSPCode)
           

############Start creating colums

dat <- dat %>% unite('source_comments', 
                       ActivityCommentText, 
                       ResultCommentText,
                       ResultLaboratoryCommentText, sep = '#', remove = FALSE) %>%
mutate(source_comments = str_replace_all(source_comments,'#NA',''),
       source_comments = str_replace_all(source_comments,'NA#',''),
       source_comments = ifelse(source_comments=="NA",'',source_comments))

#Deal with classifying chlorophyll
dat$source_methodqualifier = NA #this is where we have identified corrected vs uncorrected
temp <- dat %>% filter(parameter_name=="Chlorophyll a")
unique(temp$source_parameter_name)
unique(temp$USGSPCode)
dat <- dat %>% mutate(source_methodqualifier = ifelse(source_parameter_name=="Chlorophyll a, corrected for pheophytin","corrected",source_methodqualifier))
dat <- dat %>% mutate(source_methodqualifier = ifelse(source_parameter_name=="Chlorophyll a, free of pheophytin","corrected",source_methodqualifier))
dat <- dat %>% mutate(source_methodqualifier = ifelse(source_parameter_name=="Chlorophyll a, uncorrected for pheophytin","uncorrected",source_methodqualifier))

#deal with usgs pcodes
dat <- dat %>% mutate(source_methodqualifier = ifelse(is.na(source_methodqualifier) & wqp_parameter_usgspcode==32209,"corrected",source_methodqualifier))
dat <- dat %>% mutate(source_methodqualifier = ifelse(is.na(source_methodqualifier) & wqp_parameter_usgspcode==70953,"uncorrected",source_methodqualifier))
dat <- dat %>% mutate(source_methodqualifier = ifelse(is.na(source_methodqualifier) & wqp_parameter_usgspcode==32217,"uncorrected",source_methodqualifier))
dat <- dat %>% mutate(source_methodqualifier = ifelse(is.na(source_methodqualifier) & wqp_parameter_usgspcode==32230,"uncorrected",source_methodqualifier))
dat <- dat %>% mutate(source_methodqualifier = ifelse(is.na(source_methodqualifier) & wqp_parameter_usgspcode==32234,"uncorrected",source_methodqualifier))

#look at what is left
temp <- dat %>% filter(parameter_name=="Chlorophyll a" & is.na(source_methodqualifier))
unique(temp$source_labmethodinfo)
unique(temp$source_labmethodname)
unique(temp$ResultAnalyticalMethod.MethodIdentifier)
unique(temp$ResultAnalyticalMethod.MethodIdentifierContext)

#notes
#USEPA Method 445 provides equations for both corrected and uncorrected

#assign indeterminate to unknown
dat <- dat %>% mutate(source_methodqualifier = ifelse(is.na(source_methodqualifier) & parameter_name=="Chlorophyll a","unknown",source_methodqualifier))

#check to make sure all chlorophylls have been assigned
temp <- dat %>% filter(parameter_name=="Chlorophyll a" & is.na(source_methodqualifier))

dat <- dat %>% rename(source_labmethod_qualifier = source_methodqualifier)
dat <- dat %>% rename(source_sample_type = source_sampleposition)
dat <- dat %>% mutate(source_sample_type = ifelse(source_sample_type=="UNKNOWN","INFERRED",source_sample_type))
temp <- dat %>% filter(str_detect(source_id,'NARS') | str_detect(ProjectIdentifier,'NARS_NLA'))
dat <- dat %>% filter(!sample_id %in% temp$sample_id)
# temp <- dat %>%
    # filter(if_any(everything(), ~str_detect(., "\'\"")))
dat$source_comments <- str_replace(dat$source_comments,"\'\"","\"")
dat$ActivityCommentText <- str_replace(dat$ActivityCommentText,"\'\"","\"")
temp <- dat %>%
filter(if_any(everything(), ~str_detect(., "\'\"")))


out.file <- dat %>% select(sample_id,
                           lagoslakeid,
                           sample_date,
                           parameter_id,
                           parameter_name,
                           parameter_value,
                           parameter_detectionlimit_value,
                           sample_depth_m,
                           sample_depth_flag,
                           source_id,
                           source_name,
                           source_activityorg_name,
                           source_sample_siteid,
                           source_activityid,
                           source_comments,
                           source_parameter_name,
                           wqp_parameter_usgspcode,
                           source_parameter_value,
                           source_parameter_units,
                           source_value_qualifiercode,
                           parameter_conversionfactor,
                           source_detectionlimit_value,
                           source_detectionlimit_unit,
                           parameter_detectionlimit_conversionfactor,
                           source_detectionlimit_condition,
                           source_detectionlimit_type,
                           source_labmethod_description,
                           source_labmethod_id,
                           source_labmethod_name,
                           source_labmethod_qualifier,
                           source_sample_type
                           )


saveRDS(out.file,"/Users/noahlottig/Lottig Dropbox/Noah Lottig/CL_LAGOSUS_exports/LAGOSUS_LIMNO/US_NEW/WQX_Final.rds")

# temp <- read_rds("/Users/noahlottig/Lottig Dropbox/Noah Lottig/CL_LAGOSUS_exports/LAGOSUS_LIMNO/US_NEW/WQX_Final.rds")
# 
# #merge in variable id values
# lagos_variables <- dbGetQuery(con,'select * from limno.lagosvariables_us', stringsAsFactors = FALSE) %>% 
#     select(variableid_lagos,variablename,variableunitsid)
# 
# units <- dbGetQuery(con,'select * from limno.units', stringsAsFactors = FALSE) %>% 
#     select(variableunitsid,unitsabbreviation)
# 
# dat2 <- dat2 %>% left_join(lagos_variables,by=c("CharacteristicName" = "variablename")) %>% 
#     left_join(units)
#     
# rm(lagos_variables)
# rm(dat)
# 
# out.file <- data.frame(
#     obs_id = dat2$Obs_Id,
#     lagoslakeid = dat2$lagoslakeid,
#     sampledate = dat2$sampledate,
#     lagos_variableid = dat2$variableid_lagos,
#     lagos_variablename = dat2$CharacteristicName,
#     datavalue = dat2$datavalue,
#     datavalue_unit = dat2$unitsabbreviation,
#     detectionlimit_value = dat2$DetectionQuantitationLimitMeasure.MeasureValue,
#     datavalue_conversion = dat2$Conversion,
#     detectionlimitvalue_conversion = dat2$Conversion_dl,
#     lagos_comments = NA,
#     lagos_sampledepth = dat2$sampledepth_m_lagos,
#     lagos_sampleposition = dat2$sampleposition_lagos,
#     lagos_sampletype = dat2$sampletype_lagos,
#     organization_id = dat2$OrganizationIdentifier,
#     organization_name = dat2$OrganizationFormalName,
#     source_activityid = dat2$ActivityIdentifier,
#     source_comments = dat2$source_comments,
#     source_detectionlimit_value = dat2$detectionlimit_legacy,
#     source_detectionlimit_unit = dat2$detectionlimit_unit_legacy,
#     source_labmethoddescription = NA,
#     source_labmethodid = dat2$source_labmethodid,
#     source_labmethod_name = NA,
#     source_parameter = dat2$source_parameter,
#     source_sampledepth = dat2$sampledepth_m_legacy,
#     source_sampleposition = dat2$sampleposition_legacy,
#     source_samplesiteid = dat2$source_samplesiteid,
#     source_sampletype = dat2$sampletype_legacy,
#     source_unit = dat2$source_unit,
#     source_value = dat2$source_value,
#     source_methodqualifier = dat2$source_methodqualifier
# )
# 
# str(out.file)
# 
# out.file <- out.file %>% 
#     mutate(lagoslakeid = as.integer(lagoslakeid),
#            sampledate = ymd(sampledate),
#            datavalue = as.numeric(datavalue),
#            detectionlimit_value = as.numeric(detectionlimit_value),
#            datavalue_conversion = as.numeric(datavalue_conversion),
#            detectionlimitvalue_conversion = as.numeric(detectionlimitvalue_conversion),
#            lagos_sampledepth = as.numeric(lagos_sampledepth),
#            source_sampledepth = as.numeric(source_sampledepth)
#            )
# 
# out.file <- out.file %>% filter(source_unit != "mg/kg") %>% 
#     filter(source_unit != "mg/g") %>% 
#     filter(source_unit != "mg/m2") %>% 
#     filter(source_unit != "ng/g")
# 
# write_csv(out.file, "db_import.csv")
# 
# # 
# # #create columns needed for the postgres table
# # dat2$qualifier_legacy_full <- dat2$comments_legacy
# # dat2 <- dat2 %>% 
# #     mutate(programtableid_lagos = NA,
# #            comments_lagos = NA, # set to null
# #            censorcode_flag_lagos = NA, #set to null
# #            samplesiteid_lagos_pre_cluster = NA, #set to null
# #            lakeid_legacy = NA, #set to null
# #            version_comment_lagos = NA, #set to null
# #            flag_lagos = NA, #set to null
# #            varshortname_comment_lagos = NA, #set to null
# #            lakeid_nhdid = NA, #set to null
# #            depth_comment_lagos = NA, #set to null
# #            primarysamplesite_flag_lagos = NA, #set to null
# #            valueid_lagos = NA, #set to null
# #            sitecordid_lagos = NA, #set to null
# #            samplesiteid_lagos = NA, #set to null
# #            programid_lagos_us = NA, #set to null
# #            qualifier_legacy_full_cleaned = NA, #set to null
# #            qualifier_detect_info = NA, #set to null
# #            lagos_min_depth = NA, #set to null
# #            legacy_min_depth = NA, #set to null
# #            sample_delta = NA, #set to null
# #            epi_depth = NA, #set to null
# #            lagos_epi_assignment = NA, #set ot null
# #            exported_epi_new = NA, #set to null
# #            exported_eventid_epi = NA #set to null)
# #     )
# # 
# # dat3 <- dat2 %>% 
# #     select(sampledate, #sample date
# #            variableid_lagos, #2 digit number for variable in lagos data base
# #            State, #state observation was made
# #            CharacteristicName, # parameter name from lagos
# #            sampledepth_m_legacy, #sample depth reported by collection agency
# #            datavalue, #result value in lagos units
# #            programtableid_lagos, #nothing
# #            sampletype_legacy, #how the sample was collected, grab, integrated, depth 
# #            sampledepth_m_lagos, #lagos assigned depth
# #            samplelayer_lagos, #lagos assigned sample later, EPI/OTHER
# #            sampletype_lagos, #lagos assigned type
# #            comments_legacy, #all comments/flags associated with data value
# #            comments_lagos, #comments we add to data
# #            censorcode_flag_lagos, #censor flags we add
# #            samplesiteid_legacy, #sample site reported by agancy
# #            lagoslakeid, #lagoslakeid
# #            samplesiteid_lagos_pre_cluster, #lagos assigned 
# #            labmethodname_legacy, #method name if available
# #            lakeid_legacy, #not sure
# #            labmethodinfo_legacy, #not sue
# #            methodqualifier_legacy, #not sure
# #            version_comment_lagos, #not sure
# #            flag_lagos,
# #            varshortname_comment_lagos,
# #            lakeid_nhdid, #nhdid
# #            depth_comment_lagos,
# #            primarysamplesite_flag_lagos,
# #            valueid_lagos, #assigned in postgres
# #            sitecordid_lagos,
# #            samplesiteid_lagos,
# #            programid_lagos_us, #nothing right now
# #            source_value, #value from source provider
# #            source_unit, #units from source provider
# #            source_parameter, #parameter name from source provider
# #            Obs_Id, #unique id we assign at import
# #            qualifier_legacy,
# #            qualifier_name_legacy, #type of detection limit, method, reporting
# #            detectionlimit_legacy, #detection limit value
# #            detectionlimit_unit_legacy, #units of detection limit
# #            qualifier_legacy_full,
# #            qualifier_legacy_full_cleaned,
# #            qualifier_detect_info,
# #            lagos_min_depth,
# #            legacy_min_depth,
# #            sample_delta,
# #            epi_depth,
# #            lagos_epi_assignment,
# #            exported_epi_new,
# #            exported_eventid_epi)
# # 
# # write_csv(dat3,paste0(data_dir,"processed/alldata_processed.csv"))
# # 
# # #dat3 would be the processed file that gets loaded into postgres
# # #yet todo is add observation id to each based on values already loaded in postgres and push file to postgres
# # 
# 
# 
# variables <- read_csv("lagos_variable.csv")
# 
# #create histograms
# options(scipen=999)
# pdf(file="graphics/histograms.pdf",width=8.5,height=11)
# par(mfrow=c(3,2),mar=c(4,10,4,4))
# 
# for(i in 1:length(params)) {
#     temp <- (as.numeric(out.file$datavalue))[which(out.file$lagos_variablename==variables$variablename[i])]
#     if(length(temp)>25) {
#         adjbox(x = temp ,main=variables$variablename[i],notch=TRUE,las=1)
#         if(!is.na(variables$limit[i])) {abline(h=variables$limit[i],col="red")}
#         mtext(side=3,adj=0.1,paste("n=",length(temp))) 
#     } else {next()}
# }
# dev.off()
# 
# 
# temp <- out.file %>% filter(lagos_variablename=="Calcium, dissolved") %>% 
#     filter(datavalue >20000)
# 
# out.file <- out.file %>% left_join(variables %>% select(variableid_lagos,limit), by = c("lagos_variableid" = "variableid_lagos"))
# out.file <- out.file %>% filter(datavalue < limit)
# out.file <- out.file %>% select(-limit)
# out.file <- out.file %>% filter(datavalue >=0)
# 
# 
# 
# pdf(file="graphics/distributions.pdf",width=8.5,height=11)
# par(mfrow=c(3,2))
# 
# for(i in 1:length(params)) {
#     temp <- log10(as.numeric(out.file$datavalue)+1)[which(out.file$lagos_variablename==variables$variablename[i])]
#     if(length(temp)>25) 
#     {
#         d <- density(x = temp,na.rm=T)
#         plot(d, main=variables$variablename[i])
#     } else 
#     {next()}
# }
# dev.off()
# 
