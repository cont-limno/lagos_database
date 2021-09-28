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
files[2]
dat <- read_csv(files[2], col_types = cols(.default = "c"))

#reduce source column
# dat$source <- sapply(strsplit(dat$source, split= "/", fixed = TRUE), tail, 1L)

#link to lagoslakeid
{
lagos_link <- read_csv("lagos_wqp_20210920.csv") %>% 
    rename(MonitoringLocationIdentifier = wqp_monitoringlocationidentifier)

dat <- dat %>% left_join(lagos_link) %>% drop_na(lagoslakeid)
}
#rename columns to fit LAGOS Schema
dat2 <- dat %>% 
    rename(sampledate = ActivityStartDate) %>% 
    rename(sampledepth_m_legacy = ActivityDepthHeightMeasure.MeasureValue) %>% #depth at which the sample was taken
    rename(alt_sampledepth =ResultDepthHeightMeasure.MeasureValue) %>% #depth at which the result was derived from
    rename(datavalue = ResultMeasureValue) %>% 
    rename(qualifier_legacy = DetectionQuantitationLimitTypeName) %>% 
    rename(labmethodname_legacy = ResultAnalyticalMethod.MethodName) %>% 
    rename(labmethodinfo_legacy = MethodDescriptionText) %>% 
    rename(samplesiteid_legacy = MonitoringLocationIdentifier) %>% 
    rename(qualifier_name_legacy = ResultLaboratoryCommentText)


dat2 <- dat2 %>% unite('comments_legacy', MeasureQualifierCode,  ResultCommentText,ActivityCommentText, sep = '$', remove = FALSE) %>% 
    # select(-MeasureQualifierCode,-ResultLaboratoryCommentText) %>% 
    mutate(comments_legacy = str_replace_all(comments_legacy,'NA','')) %>% 
    mutate(comments_legacy = str_replace_all(comments_legacy,'\\$',' '))



#Assign Depths and make sure there are no non-numeric values
unique(dat2$sampledepth_m_legacy[is.na(as.numeric(dat2$sampledepth_m_legacy))])
unique(dat2$alt_sampledepth[is.na(as.numeric(dat2$alt_sampledepth))])
dat2 <- dat2 %>% mutate_at(c('sampledepth_m_legacy','alt_sampledepth'),as.numeric)

dat2$sampleposition_legacy <- ifelse(is.na(dat2$sampledepth_m_legacy) & is.na(dat2$alt_sampledepth),"UNKNOWN","SPECIFIED")
dat2$sampledepth_m_legacy <- ifelse(dat2$sampleposition_legacy == 'SPECIFIED' & is.na(dat2$sampledepth_m_legacy), dat2$alt_sampledepth, dat2$sampledepth_m_legacy)
dat2 <- dat2 %>% select(-alt_sampledepth)

#assing secchi to depth of 0
dat2$sampledepth_m_legacy[which(dat2$CharacteristicName=="Secchi" & is.na(dat2$sampledepth_m_legacy))] <- 0

#Assign sample types based on equipment used
sample_equip <- c(unique(dat2$SampleCollectionEquipmentName),
                  unique(dat2$SampleCollectionMethod.MethodIdentifier),
                  unique(dat2$SampleCollectionMethod.MethodName),
                  unique(dat2$SampleCollectionMethod.MethodIdentifierContext))

#filter out known grab methods
grab_equip = c("Grab sample","Bucket","Open-Mouth Bottle","Sampler, frame-type, Teflon bottle","Syringe",
               "Water Bottle","Secchi Disk","Horizontal Secchi Disk","GRAB-001","Stainless Bucket","Water Bottle",
               "Grab","NPS_GRAB","SECCHI_DISK","HORIZONTAL_DISK","MIDN_UVA_SPGRAB","Grab Sample Collected With A Stainless Bucket",
               "SRBC Standard Grab Sample Method","Grab","Grab Sample Collected With A Water Bottle",
               "Grab sample  (dip)","Water Grab Sampling","Grab sample collection",
               "Unspecified Standard Grab Sample Procedure","USGS parameter code 82398")
sample_equip <- sample_equip[! sample_equip %in% grab_equip] #remove grab samples from list

sample_equip #see the equipment
#if grabs still exist, add to list and remove again

#assign grab samples to appropriate observations
dat2$sampletype_legacy<-''
dat2$sampletype_legacy[which(dat2$SampleCollectionEquipmentName %in% grab_equip)] <- "GRAB"
dat2$sampletype_legacy[which(dat2$SampleCollectionMethod.MethodIdentifier %in% grab_equip)] <- "GRAB"
dat2$sampletype_legacy[which(dat2$SampleCollectionMethod.MethodName %in% grab_equip)] <- "GRAB"
dat2$sampletype_legacy[which(dat2$SampleCollectionMethod.MethodIdentifierContext %in% grab_equip)] <- "GRAB"

#filter out known approaches for depth distributed sampling
depth_equip <- c("Van Dorn Bottle","Peristaltic pump","Pump/Submersible","Submersible gear pump",
                 "Probe/Sensor","Van Dorn sampler","Pump/Non-Submersible","Suction lift peristaltic pump",
                 "Van Dorn bottle","multiprobesonde","Multi-Probe Sonde","Sonde","sonde01" )

sample_equip <- sample_equip[! sample_equip %in% depth_equip] #remove depth specific samples from list

#look and make sure no depth distributed remain and reprocess if necessary
sample_equip

dat2$sampletype_legacy[which(dat2$sampletype_legacy=="" & dat2$SampleCollectionEquipmentName %in% depth_equip)] <- "DEPTH"
dat2$sampletype_legacy[which(dat2$sampletype_legacy=="" & dat2$SampleCollectionMethod.MethodIdentifier %in% depth_equip)] <- "DEPTH"
dat2$sampletype_legacy[which(dat2$sampletype_legacy=="" & dat2$SampleCollectionMethod.MethodName %in% depth_equip)] <- "DEPTH"
dat2$sampletype_legacy[which(dat2$SampleCollectionMethod.MethodIdentifierContext %in% depth_equip)] <- "DEPTH"

#filter out integrated sampling equipment
int_equip <- c("2meterMPCA","Integrated water sampler")
sample_equip <- sample_equip[! sample_equip %in% int_equip] #remove integrated samples from list

#make sure nothing is left and refilter as needed
sample_equip

dat2$sampletype_legacy[which(dat2$SampleCollectionEquipmentName %in% int_equip)] <- "INTEGRATED"
dat2$sampletype_legacy[which(dat2$SampleCollectionMethod.MethodIdentifier %in% int_equip)] <- "INTEGRATED"
dat2$sampletype_legacy[which(dat2$SampleCollectionMethod.MethodName %in% int_equip)] <- "INTEGRATED"
dat2$sampletype_legacy[which(dat2$SampleCollectionMethod.MethodIdentifierContext %in% int_equip)] <- "INTEGRATED"

#assign all remaining to unknown
dat2$sampletype_legacy[which(dat2$sampletype_legacy=="")] <- "UNKNOWN"


dat2$sampledepth_m_lagos <- dat2$sampledepth_m_legacy
dat2$samplelayer_lagos <- dat2$sampleposition_legacy


################### QAQC WHILE PROCESSING

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

################# END QAQC ############################

#assign sample positions based on type of sample (secchi) or type of gear used
dat2$samplelayer_lagos[which(dat2$CharacteristicName=="Secchi")] <- "EPI"

#assign grab and integrated samples as EPI when legacy is unknown or not specified
dat2$samplelayer_lagos[which(dat2$samplelayer_lagos=="UNKNOWN" & dat2$sampletype_legacy=="GRAB")] <- "EPI"
dat2$samplelayer_lagos[which(dat2$samplelayer_lagos=="UNKNOWN" & dat2$ResultCommentText=="Surface")] <- "EPI"

#note: may want to revist this
dat2$samplelayer_lagos[which(dat2$samplelayer_lagos=="UNKNOWN" & dat2$sampletype_legacy=="INTEGRATED")] <- "EPI"

dat2$sampletype_lagos <- dat2$sampletype_legacy


######### QAQC TO EXAMINE UNKOWN LOCATION DEPTH AND SEE IF SOMETHING CAN BE FIXED ####################
#look at 
dat3 <- dat2 %>% filter(sampletype_lagos=="UNKNOWN" & is.na(sampledepth_m_legacy))
write_csv(dat3,"unknownlocations.csv")
#make sure we aren't dropping anything important

############# END QAQC

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

temp <- dat2 %>% filter(ResultSampleFractionText ==   "Non-Filterable (Particle)")
################## End QAQC


#filter out depth because we don't want it
dat2 <- dat2 %>% filter(source_parameter!="Depth")

#merge in variable id values
lagos_variables <- dbGetQuery(con,'select * from limno.lagosvariables_us', stringsAsFactors = FALSE) %>% 
    select(variableid_lagos,variablename)

dat2 <- dat2 %>% left_join(lagos_variables,by=c("CharacteristicName" = "variablename"))
rm(lagos_variables)

#Deal with classifying chlorophyll
dat2$methodqualifier_legacy = NA #this is where we have identified corrected vs uncorrected
temp <- dat2 %>% filter(CharacteristicName=="Chlorophyll a")
unique(temp$source_parameter)
unique(temp$USGSPCode)
dat2 <- dat2 %>% mutate(methodqualifier_legacy = ifelse(source_parameter=="Chlorophyll a, corrected for pheophytin","corrected",methodqualifier_legacy))
dat2 <- dat2 %>% mutate(methodqualifier_legacy = ifelse(source_parameter=="Chlorophyll a, uncorrected for pheophytin","uncorrected",methodqualifier_legacy))
#deal with usgs pcodes
dat2 <- dat2 %>% mutate(methodqualifier_legacy = ifelse(is.na(methodqualifier_legacy) & USGSPCode==32209,"corrected",methodqualifier_legacy))
dat2 <- dat2 %>% mutate(methodqualifier_legacy = ifelse(is.na(methodqualifier_legacy) & USGSPCode==70953,"uncorrected",methodqualifier_legacy))

#look at what is left
temp <- dat2 %>% filter(source_parameter=="Chlorophyll a" & is.na(methodqualifier_legacy))
unique(temp$labmethodinfo_legacy)
unique(temp$labmethodname_legacy)
unique(temp$ResultAnalyticalMethod.MethodIdentifier)
unique(temp$ResultAnalyticalMethod.MethodIdentifierContext)

#notes
#USEPA Method 445 provides equations for both corrected and uncorrected

#assign indeterminate to unknown
dat2 <- dat2 %>% mutate(methodqualifier_legacy = ifelse(is.na(methodqualifier_legacy) & source_parameter=="Chlorophyll a","unknown",methodqualifier_legacy))

#check to make sure all chlorophylls have been assigned
temp <- dat2 %>% filter(source_parameter=="Chlorophyll a" & is.na(methodqualifier_legacy))


#create columns needed for the postgres table
dat2$qualifier_legacy_full <- dat2$comments_legacy
dat2 <- dat2 %>% 
    mutate(programtableid_lagos = NA,
           comments_lagos = NA, # set to null
           censorcode_flag_lagos = NA, #set to null
           samplesiteid_lagos_pre_cluster = NA, #set to null
           lakeid_legacy = NA, #set to null
           version_comment_lagos = NA, #set to null
           flag_lagos = NA, #set to null
           varshortname_comment_lagos = NA, #set to null
           lakeid_nhdid = NA, #set to null
           depth_comment_lagos = NA, #set to null
           primarysamplesite_flag_lagos = NA, #set to null
           valueid_lagos = NA, #set to null
           sitecordid_lagos = NA, #set to null
           samplesiteid_lagos = NA, #set to null
           programid_lagos_us = NA, #set to null
           qualifier_legacy_full_cleaned = NA, #set to null
           qualifier_detect_info = NA, #set to null
           lagos_min_depth = NA, #set to null
           legacy_min_depth = NA, #set to null
           sample_delta = NA, #set to null
           epi_depth = NA, #set to null
           lagos_epi_assignment = NA, #set ot null
           exported_epi_new = NA, #set to null
           exported_eventid_epi = NA #set to null)
    )

dat3 <- dat2 %>% 
    select(sampledate,variableid_lagos,CharacteristicName,sampledepth_m_legacy,datavalue,programtableid_lagos,sampletype_legacy,sampledepth_m_lagos,
           samplelayer_lagos,sampletype_lagos,comments_legacy,comments_lagos,censorcode_flag_lagos,samplesiteid_legacy,lagoslakeid,
           samplesiteid_lagos_pre_cluster,labmethodname_legacy,lakeid_legacy,labmethodinfo_legacy,methodqualifier_legacy,
           version_comment_lagos,flag_lagos,varshortname_comment_lagos,lakeid_nhdid,depth_comment_lagos,primarysamplesite_flag_lagos,
           valueid_lagos,sitecordid_lagos,samplesiteid_lagos,programid_lagos_us,source_value, source_unit, source_parameter, 
           qualifier_legacy,qualifier_name_legacy,
           detectionlimit_legacy,detectionlimit_unit_legacy,qualifier_legacy_full,qualifier_legacy_full_cleaned,qualifier_detect_info,
           lagos_min_depth,legacy_min_depth,sample_delta,epi_depth,lagos_epi_assignment,exported_epi_new,exported_eventid_epi)

write_csv(dat3,"processed_data/MD_processed.csv")

#dat3 would be the processed file that gets loaded into postgres
#yet todo is add observation id to each based on values already loaded in postgres and push file to postgres



params <- unique(dat2$CharacteristicName)


#create histograms
pdf(file="graphics/MD_histograms.pdf",width=8.5,height=11)
par(mfrow=c(3,2))

for(i in 1:length(params)) {
    temp <- (as.numeric(dat3$datavalue))[which(dat3$CharacteristicName==params[i])]
    if(length(temp)>25) {
        adjbox(x = temp ,main=params[i],notch=TRUE)
        mtext(side=3,adj=0.1,paste("n=",length(temp))) 
    } else {next()}
}
dev.off()

pdf(file="graphics/MD_distributions.pdf",width=8.5,height=11)
par(mfrow=c(3,2))

for(i in 1:length(params)) {
    temp <- log10(as.numeric(dat2$datavalue)+1)[which(dat2$CharacteristicName==params[i])]
    if(length(temp)>25) 
    {
        d <- density(x = temp,na.rm=T)
        plot(d, main=params[i])
    } else 
    {next()}
}
dev.off()

