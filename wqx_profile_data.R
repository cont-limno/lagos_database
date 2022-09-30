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

# states_in_data<-unique(dat$State)
# length(unique(dat$State))
# #Add Activity data
# activities <- read_csv("big_data/activities.csv", 
#                        col_types = cols(ActivityStartDate = col_date(format = "%Y-%m-%d"), 
#                                         ActivityRelativeDepthName = col_character()))
# activities <- activities %>% distinct() %>% 
#     rename(sampledate = ActivityStartDate)
# 
# activities <- activities %>% group_by(OrganizationIdentifier,ActivityIdentifier,sampledate) %>% 
#     mutate(n=n()) %>% ungroup() %>% filter(n==1) %>% select(-n)
# 
# dat <- dat %>% left_join(activities)
# 
# temp <- dat %>% group_by(State) %>% 
#     summarize(n=sum(!is.na(ActivityRelativeDepthName)))
# rm(activities)
# gc()
# #check to make sure no weird sample fractions and activity types made it in
# #make sure these get excluded 


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
unique(dat$CharacteristicName)

meter_vars <-c("Turbidity",
               "pH, field or lab",
               "Oxygen, dissolved",
               "Conductivity",
               "pH",
               "Salinity",
               "Turbidity",
               "Turbidity Field",
               "Depth")
