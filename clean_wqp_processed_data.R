rm(list=ls())
library(tidyverse)
library(lubridate)
library(fs)

data_dir <- "~/Dropbox/CL_LAGOSUS_exports/LAGOSUS_LIMNO/US_NEW"
dat <- data_dir %>% 
    dir_ls(regexp = "\\.csv$") %>% 
    map_dfr(~read_csv(.x, col_types = cols(.default = "c")), .id = "source")

column_drop <- c("X","OrganizationIdentifier","OrganizationFormalName","ActivityIdentifier","ActivityTypeCode",'ActivityMediaName',"ActivityMediaSubdivisionName","ActivityStartTime.Time","ActivityStartDateTime","ActivityStartTime.TimeZoneCode","ActivityEndDate","ActivityEndTime.Time",
                 "ActivityEndTime.TimeZoneCode","ActivityDepthHeightMeasure.MeasureUnitCode","ActivityDepthAltitudeReferencePointText","ActivityTopDepthHeightMeasure.MeasureUnitCode",
                 "ActivityBottomDepthHeightMeasure.MeasureValue","ActivityBottomDepthHeightMeasure.MeasureUnitCode","ProjectIdentifier","ActivityConductingOrganizationText",
                 "ActivityCommentText","HydrologicCondition","HydrologicEvent","SampleCollectionMethod.MethodIdentifier","SampleCollectionMethod.MethodName",
                 "SampleCollectionEquipmentName","ResultDetectionConditionText","ResultSampleFractionText","ResultStatusIdentifier","StatisticalBaseCode","ResultValueTypeName","ResultTemperatureBasisText",
                 "ResultCommentText","USGSPCode","ResultAnalyticalMethod.MethodIdentifier","ResultAnalyticalMethod.MethodIdentifierContext","AnalysisStartDate","ResultLaboratoryCommentText","DetectionQuantitationLimitTypeName",
                 "DetectionQuantitationLimitMeasure.MeasureValue","DetectionQuantitationLimitMeasure.MeasureUnitCode","PreparationStartDate","ProviderName","SampleAquifer","ResultMeasure.MeasureUnitCode")



#rename columns to fit LAGOS Schema
dat2 <- dat %>% 
    rename(sampledate = ActivityStartDate) %>% 
    rename(sampledepth_m_legacy = ActivityDepthHeightMeasure.MeasureValue) %>% 
    rename(alt_sampledepth =ResultDepthHeightMeasure.MeasureValue) %>% 
    rename(datavalue = ResultMeasureValue) %>% 
    rename(qualifier_legacy = DetectionQuantitationLimitTypeName) %>% 
    rename(labmethodname_legacy = ResultAnalyticalMethod.MethodName) %>% 
    rename(labmethodinfo_legacy = MethodDescriptionText) %>% 
    rename(samplesiteid_legacy = MonitoringLocationIdentifier)


dat2 <- dat2 %>% unite('comments_legacy', MeasureQualifierCode,  ResultLaboratoryCommentText, sep = '$', remove = FALSE) %>% 
    select(-MeasureQualifierCode,-ResultLaboratoryCommentText) %>% 
    mutate(comments_legacy = str_replace_all(comments_legacy,'NA','')) %>% 
    mutate(comments_legacy = str_replace_all(comments_legacy,'\\$',' '))

unique(dat2$comments_legacy)
#check on this field with austin "ResultLaboratoryCommentCode"


#Assign Depths
unique(dat2$sampledepth_m_legacy)
unique(as.numeric(dat2$alt_sampledepth))
dat2 <- dat2 %>% mutate_at(c('sampledepth_m_legacy','alt_sampledepth'),as.numeric)

dat2$sampleposition_legacy <- ifelse(is.na(dat2$sampledepth_m_legacy) & is.na(dat2$alt_sampledepth),"UNKNOWN","SPECIFIED")
dat2$sampledepth_m_legacy <- ifelse(dat2$sampleposition_legacy == 'SPECIFIED' & is.na(dat2$sampledepth_m_legacy), dat2$alt_sampledepth, dat2$sampledepth_m_legacy)
dat2 <- dat2 %>% select(-alt_sampledepth)
dat2$sampledepth_m_legacy[which(dat2$CharacteristicName=="Depth, Secchi disk depth" | dat2$CharacteristicName=="Secchi, Horizontal Distance")] <- 0

#Assign sample types based on equipment used
unique(dat2$SampleCollectionEquipmentName)
grab_equip = c("Grab sample","Bucket","Open-Mouth Bottle","Sampler, frame-type, Teflon bottle","Syringe","Water Bottle","Secchi Disk","Horizontal Secchi Disk")
unique(dat2$SampleCollectionMethod.MethodIdentifier)
grab_equip <- c(grab_equip,"GRAB-001","Stainless Bucket","Water Bottle","Grab","NPS_GRAB","SECCHI_DISK","HORIZONTAL_DISK","MIDN_UVA_SPGRAB")
unique(dat2$SampleCollectionMethod.MethodName)
grab_equip <- c(grab_equip,"Grab Sample Collected With A Stainless Bucket","SRBC Standard Grab Sample Method","Grab",
                "Grab Sample Collected With A Water Bottle","Grab sample  (dip)","Water Grab Sampling","Grab sample collection",
                "Unspecified Standard Grab Sample Procedure")

dat2$sampletype_legacy<-''
dat2$sampletype_legacy[which(dat2$SampleCollectionEquipmentName %in% grab_equip)] <- "GRAB"
dat2$sampletype_legacy[which(dat2$SampleCollectionMethod.MethodIdentifier %in% grab_equip)] <- "GRAB"
dat2$sampletype_legacy[which(dat2$SampleCollectionMethod.MethodName %in% grab_equip)] <- "GRAB"

unique(dat2$SampleCollectionEquipmentName)
depth_equip <- c("Van Dorn Bottle","Peristaltic pump","Pump/Submersible","Submersible gear pump","Probe/Sensor","Van Dorn sampler","Pump/Non-Submersible")
unique(dat2$SampleCollectionMethod.MethodIdentifier)
depth_eqip <- c(depth_equip)
unique(dat2$SampleCollectionMethod.MethodName)
depth_eqip <- c(depth_equip,"Suction lift peristaltic pump")

dat2$sampletype_legacy[which(dat2$sampletype_legacy=="" & dat2$SampleCollectionEquipmentName %in% depth_equip)] <- "DEPTH"
dat2$sampletype_legacy[which(dat2$sampletype_legacy=="" & dat2$SampleCollectionMethod.MethodIdentifier %in% depth_equip)] <- "DEPTH"
dat2$sampletype_legacy[which(dat2$sampletype_legacy=="" & dat2$SampleCollectionMethod.MethodName %in% depth_equip)] <- "DEPTH"

unique(dat2$SampleCollectionEquipmentName)
int_equip <- c()
unique(dat2$SampleCollectionMethod.MethodIdentifier)
int_equip <- c(int_equip,"2meterMPCA")
unique(dat2$SampleCollectionMethod.MethodName)
int_equip <- c(int_equip,"Integrated water sampler")

dat2$sampletype_legacy[which(dat2$SampleCollectionEquipmentName %in% int_equip)] <- "INTEGRATED"
dat2$sampletype_legacy[which(dat2$SampleCollectionMethod.MethodIdentifier %in% int_equip)] <- "INTEGRATED"
dat2$sampletype_legacy[which(dat2$SampleCollectionMethod.MethodName %in% int_equip)] <- "INTEGRATED"

dat2$sampletype_legacy[which(dat2$sampletype_legacy=="")] <- "UNKNOWN"

dat2$sampledepth_m_lagos <- dat2$sampledepth_m_legacy
dat2$samplelayer_lagos <- dat2$sampleposition_legacy
dat2$samplelayer_lagos[which(dat2$CharacteristicName=="Depth, Secchi disk depth" | dat2$CharacteristicName=="Secchi, Horizontal Distance")] <- "EPI"

#assign grab and integrated samples as EPI when legacy is unknown or not specified
dat2$samplelayer_lagos[which(dat2$samplelayer_lagos=="UNKNOWN" & dat2$sampletype_legacy=="GRAB")] <- "EPI"
dat2$samplelayer_lagos[which(dat2$samplelayer_lagos=="UNKNOWN" & dat2$sampletype_legacy=="INTEGRATED")] <- "EPI"

dat2$sampletype_lagos <- dat2$sampletype_legacy
dat2$LaboratoryName <- NULL


dat3 <- dat2 %>% filter(samplelayer_lagos=="UNKNOWN")

#columns to add
#variableid_lagos
#programtableid_lagos
#comments_lagos
#censorcode_flag_lagos
#lagoslakeid
#samplesiteid_lagos
#lakeid_legacy
#methodqualifier_legacy
#flag_lagos
#lakeid_nhdid
#depth_comment_lagos