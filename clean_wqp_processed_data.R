rm(list=ls())
library(tidyverse)
library(lubridate)
library(fs)
library(RPostgreSQL)
library(robustbase)

#info for connecting to postgres DB
dbDisconnect(con)
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname = "lagos_us_limno",
                 host = '144.92.62.199', port = 5432,
                 user = "postgres", password = 'SparklingRusty')

#read in the datafiles
data_dir <- "~/Dropbox/CL_LAGOSUS_exports/LAGOSUS_LIMNO/US_NEW"
#list files
files <- data_dir %>% dir_ls(regexp = "\\.csv$")
files
dat <- read_csv(files[5], col_types = cols(.default = "c"))

# dat <- data_dir %>% 
    # dir_ls(regexp = "\\.csv$") %>% 
    # map_dfr(~read_csv(.x, col_types = cols(.default = "c")), .id = "source")

# column_drop <- c("X","OrganizationIdentifier","OrganizationFormalName","ActivityIdentifier","ActivityTypeCode",'ActivityMediaName',"ActivityMediaSubdivisionName","ActivityStartTime.Time","ActivityStartDateTime","ActivityStartTime.TimeZoneCode","ActivityEndDate","ActivityEndTime.Time",
#                  "ActivityEndTime.TimeZoneCode","ActivityDepthHeightMeasure.MeasureUnitCode","ActivityDepthAltitudeReferencePointText","ActivityTopDepthHeightMeasure.MeasureUnitCode",
#                  "ActivityBottomDepthHeightMeasure.MeasureValue","ActivityBottomDepthHeightMeasure.MeasureUnitCode","ProjectIdentifier","ActivityConductingOrganizationText",
#                  "ActivityCommentText","HydrologicCondition","HydrologicEvent","SampleCollectionMethod.MethodIdentifier","SampleCollectionMethod.MethodName",
#                  "SampleCollectionEquipmentName","ResultDetectionConditionText","ResultSampleFractionText","ResultStatusIdentifier","StatisticalBaseCode","ResultValueTypeName","ResultTemperatureBasisText",
#                  "ResultCommentText","USGSPCode","ResultAnalyticalMethod.MethodIdentifier","ResultAnalyticalMethod.MethodIdentifierContext","AnalysisStartDate","ResultLaboratoryCommentText","DetectionQuantitationLimitTypeName",
#                  "DetectionQuantitationLimitMeasure.MeasureValue","DetectionQuantitationLimitMeasure.MeasureUnitCode","PreparationStartDate","ProviderName","SampleAquifer","ResultMeasure.MeasureUnitCode")

#reduce source column
dat$source <- sapply(strsplit(dat$source, split= "/", fixed = TRUE), tail, 1L)

#rename columns to fit LAGOS Schema
dat2 <- dat %>% 
    rename(sampledate = ActivityStartDate) %>% 
    rename(sampledepth_m_legacy = ActivityDepthHeightMeasure.MeasureValue) %>% #depth at which the sample was taken
    rename(alt_sampledepth =ResultDepthHeightMeasure.MeasureValue) %>% #depth at which the result was derived from
    rename(datavalue = ResultMeasureValue) %>% 
    rename(qualifier_legacy = DetectionQuantitationLimitTypeName) %>% 
    rename(labmethodname_legacy = ResultAnalyticalMethod.MethodName) %>% 
    rename(labmethodinfo_legacy = MethodDescriptionText) %>% 
    rename(samplesiteid_legacy = MonitoringLocationIdentifier)


dat2 <- dat2 %>% unite('comments_legacy', MeasureQualifierCode,  ResultLaboratoryCommentText, ResultCommentText,ActivityCommentText, sep = '$', remove = FALSE) %>% 
    # select(-MeasureQualifierCode,-ResultLaboratoryCommentText) %>% 
    mutate(comments_legacy = str_replace_all(comments_legacy,'NA','')) %>% 
    mutate(comments_legacy = str_replace_all(comments_legacy,'\\$',' '))

unique(dat2$comments_legacy)
#check on this field with austin "ResultLaboratoryCommentCode"


#Assign Depths
unique(as.numeric(dat2$sampledepth_m_legacy))
unique(as.numeric(dat2$alt_sampledepth))
dat2 <- dat2 %>% mutate_at(c('sampledepth_m_legacy','alt_sampledepth'),as.numeric)

dat2$sampleposition_legacy <- ifelse(is.na(dat2$sampledepth_m_legacy) & is.na(dat2$alt_sampledepth),"UNKNOWN","SPECIFIED")
dat2$sampledepth_m_legacy <- ifelse(dat2$sampleposition_legacy == 'SPECIFIED' & is.na(dat2$sampledepth_m_legacy), dat2$alt_sampledepth, dat2$sampledepth_m_legacy)
dat2 <- dat2 %>% select(-alt_sampledepth)

#update this with new names

dat2$sampledepth_m_legacy[which(dat2$CharacteristicName=="Secchi" & is.na(dat2$sampledepth_m_legacy))] <- 0

#Assign sample types based on equipment used
sample_equip <- c(unique(dat2$SampleCollectionEquipmentName),
                  unique(dat2$SampleCollectionMethod.MethodIdentifier),
                  unique(dat2$SampleCollectionMethod.MethodName),
                  unique(dat2$SampleCollectionMethod.MethodIdentifierContext))

sample_equip #see the equipment

grab_equip = c("Grab sample","Bucket","Open-Mouth Bottle","Sampler, frame-type, Teflon bottle","Syringe",
               "Water Bottle","Secchi Disk","Horizontal Secchi Disk","GRAB-001","Stainless Bucket","Water Bottle",
               "Grab","NPS_GRAB","SECCHI_DISK","HORIZONTAL_DISK","MIDN_UVA_SPGRAB","Grab Sample Collected With A Stainless Bucket",
               "SRBC Standard Grab Sample Method","Grab","Grab Sample Collected With A Water Bottle",
               "Grab sample  (dip)","Water Grab Sampling","Grab sample collection",
               "Unspecified Standard Grab Sample Procedure","USGS parameter code 82398")

sample_equip <- sample_equip[! sample_equip %in% grab_equip] #remove grab samples from list

#assign grab samples to appropriate observations
dat2$sampletype_legacy<-''
dat2$sampletype_legacy[which(dat2$SampleCollectionEquipmentName %in% grab_equip)] <- "GRAB"
dat2$sampletype_legacy[which(dat2$SampleCollectionMethod.MethodIdentifier %in% grab_equip)] <- "GRAB"
dat2$sampletype_legacy[which(dat2$SampleCollectionMethod.MethodName %in% grab_equip)] <- "GRAB"
dat2$sampletype_legacy[which(dat2$SampleCollectionMethod.MethodIdentifierContext %in% grab_equip)] <- "GRAB"
sample_equip

depth_equip <- c("Van Dorn Bottle","Peristaltic pump","Pump/Submersible","Submersible gear pump",
                 "Probe/Sensor","Van Dorn sampler","Pump/Non-Submersible","Suction lift peristaltic pump",
                 "Van Dorn bottle","multiprobesonde","Multi-Probe Sonde" )

sample_equip <- sample_equip[! sample_equip %in% depth_equip] #remove depth specific samples from list

dat2$sampletype_legacy[which(dat2$sampletype_legacy=="" & dat2$SampleCollectionEquipmentName %in% depth_equip)] <- "DEPTH"
dat2$sampletype_legacy[which(dat2$sampletype_legacy=="" & dat2$SampleCollectionMethod.MethodIdentifier %in% depth_equip)] <- "DEPTH"
dat2$sampletype_legacy[which(dat2$sampletype_legacy=="" & dat2$SampleCollectionMethod.MethodName %in% depth_equip)] <- "DEPTH"
dat2$sampletype_legacy[which(dat2$SampleCollectionMethod.MethodIdentifierContext %in% depth_equip)] <- "DEPTH"

sample_equip

int_equip <- c("2meterMPCA","Integrated water sampler")
sample_equip <- sample_equip[! sample_equip %in% int_equip] #remove integrated samples from list


dat2$sampletype_legacy[which(dat2$SampleCollectionEquipmentName %in% int_equip)] <- "INTEGRATED"
dat2$sampletype_legacy[which(dat2$SampleCollectionMethod.MethodIdentifier %in% int_equip)] <- "INTEGRATED"
dat2$sampletype_legacy[which(dat2$SampleCollectionMethod.MethodName %in% int_equip)] <- "INTEGRATED"
dat2$sampletype_legacy[which(dat2$SampleCollectionMethod.MethodIdentifierContext %in% int_equip)] <- "INTEGRATED"
sample_equip
#make sure no obvious sampling devices were missed

dat2$sampletype_legacy[which(dat2$sampletype_legacy=="")] <- "UNKNOWN"


dat2$sampledepth_m_lagos <- dat2$sampledepth_m_legacy
dat2$samplelayer_lagos <- dat2$sampleposition_legacy

#examine unknown depth location samples
#basic checks to see how much data exists, what the programs, see if there is anything we need to really figure out
temp <- dat2 %>% filter(sampletype_legacy=="UNKNOWN") %>% filter(is.na(sampledepth_m_legacy))

#look at programs
table(temp$OrganizationFormalName)

#look at approaches for programs with lots of samples
temp2 <- temp %>% filter(OrganizationFormalName=="USGS Virginia Water Science Center")

sample_equip_temp <- c(unique(temp2$SampleCollectionEquipmentName),
                  unique(temp2$SampleCollectionMethod.MethodIdentifier),
                  unique(temp2$SampleCollectionMethod.MethodName),
                  unique(temp2$SampleCollectionMethod.MethodIdentifierContext))
sample_equip_temp

unique(temp2$ResultCommentText)



#assign sample positions based on type of sample (secchi) or type of gear used
dat2$samplelayer_lagos[which(dat2$CharacteristicName=="Secchi")] <- "EPI"

#assign grab and integrated samples as EPI when legacy is unknown or not specified
dat2$samplelayer_lagos[which(dat2$samplelayer_lagos=="UNKNOWN" & dat2$sampletype_legacy=="GRAB")] <- "EPI"
dat2$samplelayer_lagos[which(dat2$samplelayer_lagos=="UNKNOWN" & dat2$ResultCommentText=="Surface")] <- "EPI"

#note: may want to revist this
dat2$samplelayer_lagos[which(dat2$samplelayer_lagos=="UNKNOWN" & dat2$sampletype_legacy=="INTEGRATED")] <- "EPI"

dat2$sampletype_lagos <- dat2$sampletype_legacy

#look at 
dat3 <- dat2 %>% filter(sampletype_lagos=="UNKNOWN" & is.na(sampledepth_m_legacy))
write_csv(dat3,"unknownlocations.csv")
#make sure we aren't dropping anything important
unique(dat3$samplelayer_lagos)
unique(dat3$sampletype_lagos)

#drop samples for which we can't determine a depth or sample
dat2 <- dat2 %>% filter(paste0(sampledepth_m_legacy,sampletype_lagos)!="NAUNKNOWN")
nrow(dat)-nrow(dat2) #should match dat3 rows



################ QAQC #################

#parameter Mapping
params_maps<- dat2 %>% select(CharacteristicName, source_parameter) %>% distinct()
params_maps

#check to make sure no weird sample fractions and activity types made it in
unique(dat2$ActivityTypeCode)
unique(dat2$ResultSampleFractionText)

dat2 <- dat2 %>% filter(source_parameter!="Depth")



params <- unique(dat2$CharacteristicName)


#create histograms
pdf(file="graphics/VA_histograms.pdf",width=8.5,height=11)
par(mfrow=c(3,2))

for(i in 1:length(params)) {
    temp <- (as.numeric(dat2$datavalue))[which(dat2$CharacteristicName==params[i])]
    if(length(temp>100)) 
        {
        adjbox(x = temp ,main=params[i],notch=TRUE)
        mtext(side=3,adj=0.1,paste("n=",length(temp))) 
    } else 
                  {next()}
}
dev.off()

pdf(file="graphics/VA_distributions.pdf",width=8.5,height=11)
par(mfrow=c(3,2))

for(i in 1:length(params)) {
    temp <- log10(as.numeric(dat2$datavalue)+1)[which(dat2$CharacteristicName==params[i])]
    if(length(temp>100)) 
    {
        d <- density(x = temp,na.rm=T)
        plot(d, main=params[i])
    } else 
    {next()}
}
dev.off()










#Link to LAGOSLAKEID sites

lagos_sites_us <- dbGetQuery(con,'select * from limno.lagoslakesites', stringsAsFactors = FALSE) %>% 
    select(samplesiteid_legacy,lagoslakeid,samplesiteid_lagos)

dat2 <- dat2 %>% left_join(lagos_sites_us)
# dat3 <- dat2 %>% filter(is.na(lagoslakeid)) %>% 
#     select(source,OrganizationIdentifier,samplesiteid_legacy) %>% 
#     distinct()
# write_csv(dat3,"nolink.csv")
# dat4 <- dat2 %>% filter(!is.na(lagoslakeid)) %>% 
#     select(source,OrganizationIdentifier,samplesiteid_legacy) %>% 
#     distinct()
# write_csv(dat4,"linked.csv")


lagos_variables <- dbGetQuery(con,'select * from limno.lagosvariables_us', stringsAsFactors = FALSE)

write_csv(lagos_variables,"lagos_variables.csv")
#columns to add
#variableid_lagos
#programtableid_lagos
#comments_lagos
#censorcode_flag_lagos

#lakeid_legacy
#methodqualifier_legacy
#flag_lagos
#lakeid_nhdid
#depth_comment_lagos