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

data_dir <- "~/Lottig Dropbox/Noah Lottig/CL_LAGOSUS_exports/LAGOSUS_LIMNO/US_NEW/"
#list files
dat <- read_csv(paste0(data_dir,"alldata.csv"), col_types = cols(.default = "c",ResultMeasureValue=col_double()))
unique(dat$State)

#check to make sure no weird sample fractions and activity types made it in
#make sure these get excluded 
activitytypes <- unique(dat$ActivityTypeCode)
types_to_exclude <- c("Quality Control Sample-Reference Sample",
                      "Quality Control Field Sample Equipment Rinsate Blank",
                      "Quality Control Sample-Blind Duplicate",
                      "Quality Control Sample-Blind",
                      "Quality Control Field Replicate Portable Data Logger",
                      "Quality Control Sample-Inter-lab Split",
                      "Quality Control Sample-Equipment Blank",
                      "Sample-Integrated Cross-Sectional Profile",
                      "Quality Control Sample-Lab Matrix Spike",
                      "Quality Control Sample-Field Spike",
                      "Quality Control Sample-Other",
                      "Quality Control Sample-Trip Blank",
                      "Quality Control Sample-Lab Duplicate",
                      "Quality Control Sample-Measurement Precision Sample",
                      "Quality Control Sample-Lab Blank",
                      "Quality Control Sample-Lab Matrix Spike Duplicate")
activitytypes <- activitytypes[!activitytypes %in% types_to_exclude]
activitytypes #check to make sure we want all of this

dat <- dat %>% filter(ActivityTypeCode %in% activitytypes)

unique(dat$ResultSampleFractionText)

dat <- dat %>% filter(!grepl("sediment",tolower(SampleCollectionMethod.MethodName))) #remove sediment samples that were included

#filter out depth because we don't want it
dat <- dat %>% filter(source_parameter!="Depth") %>% filter(CharacteristicName!="Depth")

dat <- dat %>% filter(source_unit != "mg/kg") %>% 
    filter(source_unit != "mg/g") %>% 
    filter(source_unit != "mg/m2") %>% 
    filter(source_unit != "ng/g")

unique(dat$source_unit)

lagos_variables <- dbGetQuery(con,'select * from limno.lagosvariables_us', stringsAsFactors = FALSE) %>% 
    select(variableid_lagos,variablename,variableunitsid)
units <- dbGetQuery(con,'select * from limno.units', stringsAsFactors = FALSE) %>% 
    select(variableunitsid,unitsabbreviation)
dat <- dat %>% left_join(lagos_variables,by=c("CharacteristicName" = "variablename")) %>% 
    left_join(units)
variables <- read_csv("lagos_variable.csv")
dat <- dat %>% left_join(variables %>% select(variableid_lagos,limit_low,limit_high), by = c("variableid_lagos" = "variableid_lagos"))


#identify repeated nines and remove
dat <- dat %>% mutate(datavalue_int = as.integer(source_value))
nines <- c(999,9999,99999,999999,9999999,99999999,999999999)
nines_dat <- dat %>% filter(datavalue_int %in% nines) %>% 
    select(Obs_Id,lagoslakeid,ActivityStartDate, ActivityConductingOrganizationText,OrganizationFormalName,
           CharacteristicName,source_parameter,ResultMeasureValue,source_value,source_unit,
           Conversion,DetectionQuantitationLimitMeasure.MeasureValue,
           ResultAnalyticalMethod.MethodIdentifierContext,ResultAnalyticalMethod.MethodIdentifier,
           MethodDescriptionText,ActivityCommentText,wqp_monitoringlocationname,datavalue_int) %>% 
    filter(ResultMeasureValue-datavalue_int==0)

dat <- dat %>% filter(!Obs_Id %in% nines_dat$Obs_Id)


#pre egreggious values plots
options(scipen=999)
pdf(file="graphics/histograms_pre.pdf",width=8.5,height=11)
par(mfrow=c(3,2),mar=c(4,10,4,4))

for(i in 1:nrow(variables)) {
    temp <- (as.numeric(dat$ResultMeasureValue))[which(dat$CharacteristicName==variables$variablename[i])]
    if(length(temp)>25) {
        adjbox(x = temp ,main=paste(variables$variablename[i],variables$unitsabbreviation[i],sep=" - "),notch=TRUE,las=1)
        abline(h=variables$limit_high[i],col="red")
        abline(h=variables$limit_low[i],col="blue")
        mtext(side=3,adj=0.1,paste("n=",length(temp))) 
    } else {next()}
}
dev.off()

#qaqc and look at variables if needed
temp <- dat %>% filter(CharacteristicName=="Color, true") %>% 
    filter(ResultMeasureValue >1000) %>% 
    select(ActivityConductingOrganizationText,OrganizationFormalName,
           CharacteristicName,source_parameter,ResultMeasureValue,source_value,source_unit,
           Conversion,ResultAnalyticalMethod.MethodIdentifierContext,ResultAnalyticalMethod.MethodIdentifier,
           MethodDescriptionText,ActivityCommentText,wqp_monitoringlocationname)

#filter egregios values and write to data file
egreg_dat <- dat %>% filter(ResultMeasureValue < limit_low | ResultMeasureValue > limit_high) %>% 
    select(Obs_Id,lagoslakeid,ActivityStartDate, ActivityConductingOrganizationText,OrganizationFormalName,
               CharacteristicName,source_parameter,ResultMeasureValue,source_value,source_unit,
               Conversion,DetectionQuantitationLimitMeasure.MeasureValue,
           ResultAnalyticalMethod.MethodIdentifierContext,ResultAnalyticalMethod.MethodIdentifier,
               MethodDescriptionText,ActivityCommentText,wqp_monitoringlocationname)

dat <- dat %>% filter(!Obs_Id %in% egreg_dat$Obs_Id)

egreg_dat <- rbind(egreg_dat,(nines_dat %>% select(-datavalue_int)))
write_csv(egreg_dat,"excluded_values.csv")
rm(egreg_dat)
rm(nines_dat)


#plot data after removing egregious values
options(scipen=999)
pdf(file="graphics/histograms_post.pdf",width=8.5,height=11)
par(mfrow=c(3,2),mar=c(4,10,4,4))

for(i in 1:nrow(variables)) {
    temp <- (as.numeric(dat$ResultMeasureValue))[which(dat$CharacteristicName==variables$variablename[i])]
    if(length(temp)>25) {
        adjbox(x = temp ,main=paste(variables$variablename[i],variables$unitsabbreviation[i],sep=" - "),notch=TRUE,las=1)
        abline(h=variables$limit_high[i],col="red")
        abline(h=variables$limit_low[i],col="blue")
        mtext(side=3,adj=0.1,paste("n=",length(temp))) 
    } else {next()}
}
dev.off()



#rename columns to fit LAGOS Schema
dat2 <- dat %>% 
    rename(sampledate = ActivityStartDate) %>% 
    # rename(sampledepth_m_legacy = ActivityDepthHeightMeasure.MeasureValue) %>% #depth at which the sample was taken
    # rename(alt_sampledepth =ResultDepthHeightMeasure.MeasureValue) %>% #depth at which the result was derived from
    rename(datavalue = ResultMeasureValue) %>% 
    rename(qualifier_legacy = DetectionQuantitationLimitTypeName) %>% 
    rename(source_labmethodid = Method_Id) %>% 
    rename(source_labmethodname = ResultAnalyticalMethod.MethodName) %>% 
    rename(source_labmethoddescription = MethodDescriptionText) %>% 
    rename(source_samplesiteid = MonitoringLocationIdentifier)

#Assign Depths
unique(dat2$ActivityDepthHeightMeasure.MeasureUnitCode)
unique(dat2$ResultDepthHeightMeasure.MeasureUnitCode)
unique(dat2$ActivityBottomDepthHeightMeasure.MeasureUnitCode)
unique(dat2$ActivityTopDepthHeightMeasure.MeasureUnitCode)
dat2 <- dat2 %>% mutate_at(c('ActivityDepthHeightMeasure.MeasureValue',
                             'ResultDepthHeightMeasure.MeasureValue',
                             'ActivityBottomDepthHeightMeasure.MeasureValue',
                             'ActivityTopDepthHeightMeasure.MeasureValue')
                           ,as.numeric)

dat2$sampledepth_m_legacy <- dat2$ActivityDepthHeightMeasure.MeasureValue
temp <- dat2 %>% filter(is.na(sampledepth_m_legacy)) %>% 
    filter(!is.na(ResultDepthHeightMeasure.MeasureValue))
dat2$sampledepth_m_legacy[which(dat2$Obs_Id %in% temp$Obs_Id)] <- temp$ResultDepthHeightMeasure.MeasureValue
temp <- dat2 %>% filter(is.na(sampledepth_m_legacy)) %>% 
    filter(!is.na(ActivityBottomDepthHeightMeasure.MeasureValue))
dat2$sampledepth_m_legacy[which(dat2$Obs_Id %in% temp$Obs_Id)] <- temp$ActivityBottomDepthHeightMeasure.MeasureValue
temp <- dat2 %>% filter(is.na(sampledepth_m_legacy)) %>% 
    filter(!is.na(ActivityTopDepthHeightMeasure.MeasureValue))
dat2$sampledepth_m_legacy[which(dat2$Obs_Id %in% temp$Obs_Id)] <- temp$ActivityTopDepthHeightMeasure.MeasureValue

#assign depth to samples with known assigned sample depths
dat2$sampleposition_legacy <- ifelse(is.na(dat2$sampledepth_m_legacy),"UNKNOWN","SPECIFIED")

dat2$sampledepth_m_lagos <- dat2$sampledepth_m_legacy
dat2$sampleposition_lagos <- dat2$sampleposition_legacy

#assign secchi to depth of 0
dat2$sampledepth_m_lagos[which(dat2$CharacteristicName=="Secchi" & is.na(dat2$sampledepth_m_legacy))] <- 0
dat2$sampleposition_lagos[which(dat2$CharacteristicName=="Secchi")] <- "SPECIFIED"

temp <- dat2 %>% filter(is.na(sampledepth_m_lagos)) %>% 
    filter(tolower(ActivityDepthAltitudeReferencePointText)=="surface")
dat2$sampledepth_m_lagos[which(dat2$Obs_Id %in% temp$Obs_Id)] <- 0
dat2$sampleposition_lagos[which(dat2$Obs_Id %in% temp$Obs_Id)] <- "SPECIFIED"

temp <- dat2 %>% filter(is.na(sampledepth_m_lagos)) %>% 
    filter(tolower(ActivityDepthAltitudeReferencePointText)=="water surface")
dat2$sampledepth_m_lagos[which(dat2$Obs_Id %in% temp$Obs_Id)] <- 0
dat2$sampleposition_lagos[which(dat2$Obs_Id %in% temp$Obs_Id)] <- "SPECIFIED"

temp <- dat2 %>% filter(is.na(sampledepth_m_lagos)) %>% 
    filter(tolower(SampleCollectionMethod.MethodName)=="surface water grab sample")
dat2$sampledepth_m_lagos[which(dat2$Obs_Id %in% temp$Obs_Id)] <- 0
dat2$sampleposition_lagos[which(dat2$Obs_Id %in% temp$Obs_Id)] <- "SPECIFIED"

temp <- dat2 %>% filter(is.na(sampledepth_m_lagos)) %>% 
    filter(tolower(ResultCommentText)=="surface sample")
dat2$sampledepth_m_lagos[which(dat2$Obs_Id %in% temp$Obs_Id)] <- 0
dat2$sampleposition_lagos[which(dat2$Obs_Id %in% temp$Obs_Id)] <- "SPECIFIED"

temp <- dat2 %>% filter(is.na(sampledepth_m_lagos)) %>% 
    filter(tolower(ProjectIdentifier)=="surface")
dat2$sampledepth_m_lagos[which(dat2$Obs_Id %in% temp$Obs_Id)] <- 0
dat2$sampleposition_lagos[which(dat2$Obs_Id %in% temp$Obs_Id)] <- "SPECIFIED"

temp <- dat2 %>% filter(is.na(sampledepth_m_lagos)) %>% 
    filter(grepl("surface sample",tolower(wqp_monitoringlocationname)))
dat2$sampledepth_m_lagos[which(dat2$Obs_Id %in% temp$Obs_Id)] <- 0
dat2$sampleposition_lagos[which(dat2$Obs_Id %in% temp$Obs_Id)] <- "SPECIFIED"

#Assign sample types based on equipment used
sample_equip <- data.frame(equip=unique(tolower(c(unique(dat2$SampleCollectionEquipmentName),
                  unique(dat2$SampleCollectionMethod.MethodIdentifier),
                  unique(dat2$SampleCollectionMethod.MethodName),
                  unique(dat2$SampleCollectionMethod.MethodIdentifierContext)))))

sample_exclude <- read_csv("sample_equip_exclude.csv")

sample_equip <- sample_equip %>% filter(!equip %in% sample_exclude$equip)
sample_equip <- sample_equip$equip

#filter out known grab methods
grab_equip = tolower(c("Grab sample","Bucket","Open-Mouth Bottle","Sampler, frame-type, Teflon bottle","Syringe",
               "Water Bottle","Secchi Disk","Horizontal Secchi Disk","GRAB-001","Stainless Bucket","Water Bottle",
               "Grab","NPS_GRAB","SECCHI_DISK","HORIZONTAL_DISK","MIDN_UVA_SPGRAB","Grab Sample Collected With A Stainless Bucket",
               "SRBC Standard Grab Sample Method","Grab","Grab Sample Collected With A Water Bottle",
               "Grab sample  (dip)","Water Grab Sampling","Grab sample collection",
               "Unspecified Standard Grab Sample Procedure","USGS parameter code 82398", "GRAB", "glac grab sample for coliforms",
               "grab sample (water bottle)","grab water sample for water bacteriology","water grab sample","water  quality grab sampling.",
               "standard grab method","thompson spring grab sample","standard grab","grab sampling","standard grab sampler","open-top bailer","petite ponar grab",
               "benthic grab (other)","phytoplankton-water bottle","grab-01","ga grab","grab-1","direct grab","intermediate grab","tdecwrgrab","grab-dtsc",
               "grab-sampler","water_grab","grba_grab","crla_gs_gr","klmn_grab","wq-grab","blcacure_grab","standard uhl sampling procedure - grab samples",
               "water sampling, grab","surface water grab sample","ground water grab sample","grab water sample","bio-chem lab standard grab sample procedure", 
               "grab sample-grabber bottle","adem sops-2000 series-surface water","black canyon np and curecanti nra grab sample collection procedure",
               "water quality grab sample method","water quality grab sampling","standard grab sample procedure","absentee shawnee grab",
               "grab sample.  submerge and fill a water sampling vessel, or sample directly into the sample bottle provided by the an...",
               "grab water sample taken from a reservoir by using a bottle","grab water sample taken from a lake by using a bottle","grab-using sampler",
               "grab-direct to sample container","abs_grab","cupn_grab","glac_grab1","grab water sample taken from a reservoir by using a water bottle",
               "fdep sop surface water sample","water samples taken from a reservoir by using a water bottle and composited" ,
               "water samples taken from a lake by a water bottle and composited","water samples taken from a reservoir by using a bottle and composited",
               "water samples taken from a lake by using bottles and composited"))
sample_equip <- sample_equip[! sample_equip %in% grab_equip] #remove grab samples from list

#filter out integrated sampling equipment
int_equip <- tolower(c("2meterMPCA","Integrated water sampler","double-valve bailer","gravity core","push core",
                       "comp-vertdis-sampler","comp-int vert/pump","comp-vert","single vertical","multiple verticals",
                       "sample-composite vertical profile","lake depth integrated water sample","wqcd vertically integrating lake sampling procedure",
                       "water samples taken from vertical profile of a lake by using a van dorn bottle and composited",
                       "water samples taken from a lake by using an integrated verticle tube and composited","composite-vertical-discrete-sampler",
                       "lake surface 2m depth-integrated sampling. lower a 2-meter-long, 2-inch-diameter pvc pipe vertically into the water, ...",
                       "integrated water sampler","composite-discrete vertical","composite-integrated vertical/submersible pump & hose",
                       "flbs integrated vertical water sample","integrated water sample","flbs chlorophyll-a integrated depths sample",
                       "water samples taken from a reservoir by using an integrated verticle tube and composited",
                       "sop for the collection of lake or non-wadable wetland water samples using 6-foot depth integrated column sampler." ))
sample_equip <- sample_equip[! sample_equip %in% int_equip] #remove integrated samples from list

#filter out known approaches for depth distributed sampling
depth_equip <- tolower(c("Van Dorn Bottle","Peristaltic pump","Pump/Submersible","Submersible gear pump",
                         "Probe/Sensor","Van Dorn sampler","Pump/Non-Submersible","Suction lift peristaltic pump",
                         "Van Dorn bottle","multiprobesonde","Multi-Probe Sonde","Sonde","sonde01","Kemmerer Bottle",
                         "weighted-bottle sampler","kemmerer bottle","van dorn sampler","submersible centrifugal pump",
                         "suction pump","van dorn bottle","probe/sensor","niskin bottle","probe/sensor","gas reciprocating pump",
                         "pump/non-submersible","bladder pump","van_dorn","probe","ysi 556","multi-probe_2_wqx","sonde01",
                         "ysi 6600","multi parameter","wqprobe/sensor","field measurements using a multi-probe meter","direct field measurements using intrumentation",
                         "flbs chlorophyll-a sampling at depth","van dorn grab sample","niskin bottle water sample","thompson van dorn lake sample",
                         "wqcd lake sampling procedure using a van dorn bottle",
                         "lake depth point sampling.  lake water is sampled at a discrete depth in the water column using a vertical kemmerer- ...",
                         "submersible pump","weighted bottle","van dorn water sample","in situ measurement with probe",
                         "split grab water sample taken from a reservoir by using a van dorn bottle","crater lake long term monitoring-water sample at depth" ,
                         "glac water samples at 10-meters depth by geotech 1.66-inch bladder pump","grab water sample taken from a lake by using a van dorn bottle",
                         "in situ measurement with probe","multi probe sonde","pump/bladder" ,"crater lake limnology and water quality samples at depth",
                         "water samples taken from a lake by a van dorn bottle and composited" ,"water samples taken from a reservoir by using a van dorn bottle and composited",
                         "grab water sample taken from a reservoir by using a van dorn bottle"
                         ))

sample_equip <- sample_equip[! sample_equip %in% depth_equip] #remove depth specific samples from list


sample_equip #see the equipment
#if grabs still exist, add to list and remove again

#assign grab samples to appropriate observations
dat2$sampletype_legacy<-''

dat2$sampletype_legacy[which(dat2$sampletype_legacy=="" & tolower(dat2$SampleCollectionEquipmentName) %in% depth_equip)] <- "DEPTH"
dat2$sampletype_legacy[which(dat2$sampletype_legacy=="" & tolower(dat2$SampleCollectionMethod.MethodIdentifier) %in% depth_equip)] <- "DEPTH"
dat2$sampletype_legacy[which(dat2$sampletype_legacy=="" & tolower(dat2$SampleCollectionMethod.MethodName) %in% depth_equip)] <- "DEPTH"
dat2$sampletype_legacy[which(dat2$sampletype_legacy=="" & tolower(dat2$SampleCollectionMethod.MethodIdentifierContext) %in% depth_equip)] <- "DEPTH"

dat2$sampletype_legacy[which(tolower(dat2$SampleCollectionEquipmentName) %in% int_equip)] <- "INTEGRATED"
dat2$sampletype_legacy[which(tolower(dat2$SampleCollectionMethod.MethodIdentifier) %in% int_equip)] <- "INTEGRATED"
dat2$sampletype_legacy[which(tolower(dat2$SampleCollectionMethod.MethodName) %in% int_equip)] <- "INTEGRATED"
dat2$sampletype_legacy[which(tolower(dat2$SampleCollectionMethod.MethodIdentifierContext) %in% int_equip)] <- "INTEGRATED"

dat2$sampletype_legacy[which(tolower(dat2$SampleCollectionEquipmentName) %in% grab_equip)] <- "GRAB"
dat2$sampletype_legacy[which(tolower(dat2$SampleCollectionMethod.MethodIdentifier) %in% grab_equip)] <- "GRAB"
dat2$sampletype_legacy[which(tolower(dat2$SampleCollectionMethod.MethodName) %in% grab_equip)] <- "GRAB"
dat2$sampletype_legacy[which(tolower(dat2$SampleCollectionMethod.MethodIdentifierContext) %in% grab_equip)] <- "GRAB"


#One off assignments

dat2$sampletype_legacy[which(dat2$OrganizationIdentifier == "TCEQMAIN")] <- "GRAB" #per SOP manual samples are collected as just below surface
dat2$sampletype_legacy[which(dat2$sampletype_legacy=="" & dat2$ActivityTypeCode=="Sample-Integrated Vertical Profile")] <- "INTEGRATED"
dat2$sampletype_legacy[which(dat2$OrganizationIdentifier == "21GAEPD_WQX" & is.na(dat2$sampledepth_m_lagos))] <- "INTEGRATED" #per GA SOP, integrated Photozone samples
dat2$sampletype_legacy[which(tolower(dat2$ActivityCommentText)=="integrated" & is.na(dat2$sampledepth_m_lagos))] <- "INTEGRATED"
dat2$sampletype_legacy[which(tolower(dat2$source_labmethodname)=="unspecified standard grab sample procedure" & is.na(dat2$sampledepth_m_lagos))] <- "GRAB"
dat2$sampletype_legacy[which(tolower(dat2$ProjectIdentifier)=="hepgrab" & is.na(dat2$sampledepth_m_lagos))] <- "GRAB"

temp <- dat2 %>% filter(is.na(sampledepth_m_lagos)) %>% 
    filter(grepl("collected by grab",tolower(ActivityCommentText))) %>% 
    filter(sampletype_legacy=="")
dat2$sampletype_legacy[which(dat2$Obs_Id %in% temp$Obs_Id)] <- "GRAB"  

temp <- dat2 %>% filter(is.na(sampledepth_m_lagos)) %>% 
    filter(grepl("grab sample",tolower(source_labmethodname))) %>% 
    filter(sampletype_legacy=="")
dat2$sampletype_legacy[which(dat2$Obs_Id %in% temp$Obs_Id)] <- "GRAB" 

# temp<- dat2 %>% filter(is.na(sampledepth_m_lagos)) %>%
#      filter(sampletype_legacy=="")
# temp <- as.data.frame(temp)
# sink("uniquevals.txt")
# for(i in 1:ncol(temp)) {
#     print(names(temp[i]))
#     print(unique(tolower(temp[[i]])))
# }
# sink()

#assign all remaining to unknown
dat2$sampletype_legacy[which(dat2$sampletype_legacy=="")] <- "UNKNOWN"
#all legacy sample type assigments made, transfer to sampletype lagos and any assignments now are lagos assigned
dat2$sampletype_lagos <- dat2$sampletype_legacy


#remove sensor like values without depth because no known knowlege
temp3 <- dat2 %>% filter(sampletype_legacy=="UNKNOWN") %>% 
    filter(is.na(sampledepth_m_lagos)) %>% 
    filter(CharacteristicName=="pH, equilibrated") %>% 
    group_by(source_samplesiteid,sampledate) %>% 
    mutate(n=n()) %>% 
    ungroup() %>% 
    filter(n>1)
dat2 <- dat2 %>% filter(!Obs_Id %in% temp3$Obs_Id )
rm(temp3)
temp3 <- dat2 %>% filter(sampletype_legacy=="UNKNOWN") %>% 
    filter(is.na(sampledepth_m_lagos)) %>% 
    filter(CharacteristicName=="pH, equilibrated")
dat2$sampledepth_m_lagos[which(dat2$Obs_Id %in% temp3$Obs_Id)] <- 0
dat2$sampleposition_lagos[which(dat2$Obs_Id %in% temp3$Obs_Id)] <- "SPECIFIED"
rm(temp3)

temp3 <- dat2 %>% filter(sampletype_legacy=="UNKNOWN") %>% 
    filter(is.na(sampledepth_m_lagos)) %>% 
    filter(CharacteristicName=="Oxygen, dissolved") %>% 
    group_by(source_samplesiteid,sampledate) %>% 
    mutate(n=n()) %>% 
    ungroup() %>% 
    filter(n>1)
dat2 <- dat2 %>% filter(!Obs_Id %in% temp3$Obs_Id )
rm(temp3)
temp3 <- dat2 %>% filter(sampletype_legacy=="UNKNOWN") %>% 
    filter(is.na(sampledepth_m_lagos)) %>% 
    filter(CharacteristicName=="Oxygen, dissolved")
dat2$sampledepth_m_lagos[which(dat2$Obs_Id %in% temp3$Obs_Id)] <- 0
dat2$sampleposition_lagos[which(dat2$Obs_Id %in% temp3$Obs_Id)] <- "SPECIFIED"
rm(temp3)

temp3 <- dat2 %>% filter(sampletype_legacy=="UNKNOWN") %>% 
    filter(is.na(sampledepth_m_lagos)) %>% 
    filter(CharacteristicName=="Conductivity") %>% 
    group_by(source_samplesiteid,sampledate) %>% 
    mutate(n=n()) %>% 
    ungroup() %>% 
    filter(n>1)
dat2 <- dat2 %>% filter(!Obs_Id %in% temp3$Obs_Id )
rm(temp3)
temp3 <- dat2 %>% filter(sampletype_legacy=="UNKNOWN") %>% 
    filter(is.na(sampledepth_m_lagos)) %>% 
    filter(CharacteristicName=="Conductivity")
dat2$sampledepth_m_lagos[which(dat2$Obs_Id %in% temp3$Obs_Id)] <- 0
dat2$sampleposition_lagos[which(dat2$Obs_Id %in% temp3$Obs_Id)] <- "SPECIFIED"
rm(temp3)

temp3 <- dat2 %>% filter(sampletype_legacy=="UNKNOWN") %>% 
    filter(is.na(sampledepth_m_lagos)) %>% 
    filter(CharacteristicName=="Turbidity") %>% 
    group_by(source_samplesiteid,sampledate) %>% 
    mutate(n=n()) %>% 
    ungroup() %>% 
    filter(n>1)
dat2 <- dat2 %>% filter(!Obs_Id %in% temp3$Obs_Id )
rm(temp3)
temp3 <- dat2 %>% filter(sampletype_legacy=="UNKNOWN") %>% 
    filter(is.na(sampledepth_m_lagos)) %>% 
    filter(CharacteristicName=="Turbidity")
dat2$sampledepth_m_lagos[which(dat2$Obs_Id %in% temp3$Obs_Id)] <- 0
dat2$sampleposition_lagos[which(dat2$Obs_Id %in% temp3$Obs_Id)] <- "SPECIFIED"
rm(temp3)

################### QAQC WHILE PROCESSING

#examine unknown depth location samples
#basic checks to see how much data exists, what the programs, see if there is anything we need to really figure out
temp <- dat2 %>% filter(sampletype_legacy=="UNKNOWN") %>% filter(is.na(sampledepth_m_lagos)) # %>% filter(CharacteristicName == "Chlorophyll a")

#look at programs
temp_table <- table(temp$OrganizationFormalName)
unknown_depths <- as.data.frame(temp_table) 
temp_table2 <- table(temp$CharacteristicName)
samples <- as.data.frame(temp_table2)

#look at approaches for programs with lots of samples
temp2 <- temp %>% filter(OrganizationFormalName=="Seminole  County (Florida)")
unique(temp2$CharacteristicName)

sample_equip_temp <- c(unique(temp2$SampleCollectionEquipmentName),
                  unique(temp2$SampleCollectionMethod.MethodIdentifier),
                  unique(temp2$SampleCollectionMethod.MethodName),
                  unique(temp2$SampleCollectionMethod.MethodIdentifierContext))
sample_equip_temp

unique(temp2$ResultCommentText)

################# END QAQC ############################

dat3 <- dat2 %>% filter(sampletype_legacy=="UNKNOWN" & is.na(sampledepth_m_lagos))
write_csv(dat3,"unknownlocations.csv")
rm(dat3)

#remove observations with unknown sample type and unknown depth
dat2 <- dat2 %>% filter(paste0(sampledepth_m_lagos,sampletype_legacy)!="NAUNKNOWN")

#if more than 1 observation was made with a depth gear, drop the data
#if only 1 observation was made, assumme it was a surface sample
temp <- dat2 %>% 
    filter(is.na(sampledepth_m_lagos)) %>% 
    filter(sampletype_lagos=="DEPTH") %>% 
    group_by(CharacteristicName,source_samplesiteid,sampledate) %>% 
    mutate(n=n()) %>% 
    ungroup() %>% 
    filter(n>1)
dat2 <- dat2 %>% filter(!Obs_Id %in% temp$Obs_Id)

temp <- dat2 %>% 
    filter(is.na(sampledepth_m_lagos)) %>% 
    filter(sampletype_lagos=="DEPTH")
dat2$sampledepth_m_lagos[which(dat2$Obs_Id %in% temp$Obs_Id)] <- 0
dat2$sampleposition_lagos[which(dat2$Obs_Id %in% temp$Obs_Id)] <- "SPECIFIED"





############Start creating colums

dat2 <- dat2 %>% unite('source_comments', 
                       ActivityCommentText, 
                       ResultCommentText,
                       ResultLaboratoryCommentText,
                       MeasureQualifierCode,
                       ResultDetectionConditionText, sep = '$', remove = FALSE) %>%
mutate(comments_legacy = str_replace_all(source_comments,'NA','')) %>%
mutate(comments_legacy = str_replace_all(source_comments,'\\$',' '))


# obs <- data.frame(table(dat2$CharacteristicName))
# write_csv(obs,"data_summary.csv")
# 
# temp <- dat2 %>% 
#     filter(is.na(sampledepth_m_lagos)) %>% 
#     filter(sampletype_lagos=="GRAB")
# 
# table(temp$OrganizationFormalName)


################ QAQC #################

#parameter Mapping
params_maps<- dat2 %>% select(CharacteristicName, source_parameter) %>% distinct()
params_maps
temp <- dat2 %>% filter(ResultSampleFractionText ==   "Non-Filterable (Particle)")
################## End QAQC






#Deal with classifying chlorophyll
dat2$source_methodqualifier = NA #this is where we have identified corrected vs uncorrected
temp <- dat2 %>% filter(CharacteristicName=="Chlorophyll a")
unique(temp$source_parameter)
unique(temp$USGSPCode)
dat2 <- dat2 %>% mutate(source_methodqualifier = ifelse(source_parameter=="Chlorophyll a, corrected for pheophytin","corrected",source_methodqualifier))
dat2 <- dat2 %>% mutate(source_methodqualifier = ifelse(source_parameter=="Chlorophyll a, uncorrected for pheophytin","uncorrected",source_methodqualifier))
#deal with usgs pcodes
dat2 <- dat2 %>% mutate(source_methodqualifier = ifelse(is.na(source_methodqualifier) & USGSPCode==32209,"corrected",source_methodqualifier))
dat2 <- dat2 %>% mutate(source_methodqualifier = ifelse(is.na(source_methodqualifier) & USGSPCode==70953,"uncorrected",source_methodqualifier))
dat2 <- dat2 %>% mutate(source_methodqualifier = ifelse(is.na(source_methodqualifier) & USGSPCode==32217,"uncorrected",source_methodqualifier))
dat2 <- dat2 %>% mutate(source_methodqualifier = ifelse(is.na(source_methodqualifier) & USGSPCode==32230,"uncorrected",source_methodqualifier))
dat2 <- dat2 %>% mutate(source_methodqualifier = ifelse(is.na(source_methodqualifier) & USGSPCode==32234,"uncorrected",source_methodqualifier))

#look at what is left
temp <- dat2 %>% filter(source_parameter=="Chlorophyll a" & is.na(source_methodqualifier))
unique(temp$source_labmethodinfo)
unique(temp$source_labmethodname)
unique(temp$ResultAnalyticalMethod.MethodIdentifier)
unique(temp$ResultAnalyticalMethod.MethodIdentifierContext)

#notes
#USEPA Method 445 provides equations for both corrected and uncorrected

#assign indeterminate to unknown
dat2 <- dat2 %>% mutate(source_methodqualifier = ifelse(is.na(source_methodqualifier) & source_parameter=="Chlorophyll a","unknown",source_methodqualifier))

#check to make sure all chlorophylls have been assigned
temp <- dat2 %>% filter(source_parameter=="Chlorophyll a" & is.na(source_methodqualifier))





#merge in variable id values
lagos_variables <- dbGetQuery(con,'select * from limno.lagosvariables_us', stringsAsFactors = FALSE) %>% 
    select(variableid_lagos,variablename,variableunitsid)

units <- dbGetQuery(con,'select * from limno.units', stringsAsFactors = FALSE) %>% 
    select(variableunitsid,unitsabbreviation)

dat2 <- dat2 %>% left_join(lagos_variables,by=c("CharacteristicName" = "variablename")) %>% 
    left_join(units)
    
rm(lagos_variables)
rm(dat)

out.file <- data.frame(
    obs_id = dat2$Obs_Id,
    lagoslakeid = dat2$lagoslakeid,
    sampledate = dat2$sampledate,
    lagos_variableid = dat2$variableid_lagos,
    lagos_variablename = dat2$CharacteristicName,
    datavalue = dat2$datavalue,
    datavalue_unit = dat2$unitsabbreviation,
    detectionlimit_value = dat2$DetectionQuantitationLimitMeasure.MeasureValue,
    datavalue_conversion = dat2$Conversion,
    detectionlimitvalue_conversion = dat2$Conversion_dl,
    lagos_comments = NA,
    lagos_sampledepth = dat2$sampledepth_m_lagos,
    lagos_sampleposition = dat2$sampleposition_lagos,
    lagos_sampletype = dat2$sampletype_lagos,
    organization_id = dat2$OrganizationIdentifier,
    organization_name = dat2$OrganizationFormalName,
    source_activityid = dat2$ActivityIdentifier,
    source_comments = dat2$source_comments,
    source_detectionlimit_value = dat2$detectionlimit_legacy,
    source_detectionlimit_unit = dat2$detectionlimit_unit_legacy,
    source_labmethoddescription = NA,
    source_labmethodid = dat2$source_labmethodid,
    source_labemethod_name = NA,
    source_parameter = dat2$source_parameter,
    source_sampledepth = dat2$sampledepth_m_legacy,
    source_sampleposition = dat2$sampleposition_legacy,
    source_samplesiteid = dat2$source_samplesiteid,
    source_sampletype = dat2$sampletype_legacy,
    source_unit = dat2$source_unit,
    source_value = dat2$source_value,
    source_methodqualifier = dat2$source_methodqualifier
)

str(out.file)

out.file <- out.file %>% 
    mutate(lagoslakeid = as.integer(lagoslakeid),
           sampledate = ymd(sampledate),
           datavalue = as.numeric(datavalue),
           detectionlimit_value = as.numeric(detectionlimit_value),
           datavalue_conversion = as.numeric(datavalue_conversion),
           detectionlimitvalue_conversion = as.numeric(detectionlimitvalue_conversion),
           lagos_sampledepth = as.numeric(lagos_sampledepth),
           source_sampledepth = as.numeric(source_sampledepth)
           )

out.file <- out.file %>% filter(source_unit != "mg/kg") %>% 
    filter(source_unit != "mg/g") %>% 
    filter(source_unit != "mg/m2") %>% 
    filter(source_unit != "ng/g")

write_csv(out.file, "db_import.csv")

# 
# #create columns needed for the postgres table
# dat2$qualifier_legacy_full <- dat2$comments_legacy
# dat2 <- dat2 %>% 
#     mutate(programtableid_lagos = NA,
#            comments_lagos = NA, # set to null
#            censorcode_flag_lagos = NA, #set to null
#            samplesiteid_lagos_pre_cluster = NA, #set to null
#            lakeid_legacy = NA, #set to null
#            version_comment_lagos = NA, #set to null
#            flag_lagos = NA, #set to null
#            varshortname_comment_lagos = NA, #set to null
#            lakeid_nhdid = NA, #set to null
#            depth_comment_lagos = NA, #set to null
#            primarysamplesite_flag_lagos = NA, #set to null
#            valueid_lagos = NA, #set to null
#            sitecordid_lagos = NA, #set to null
#            samplesiteid_lagos = NA, #set to null
#            programid_lagos_us = NA, #set to null
#            qualifier_legacy_full_cleaned = NA, #set to null
#            qualifier_detect_info = NA, #set to null
#            lagos_min_depth = NA, #set to null
#            legacy_min_depth = NA, #set to null
#            sample_delta = NA, #set to null
#            epi_depth = NA, #set to null
#            lagos_epi_assignment = NA, #set ot null
#            exported_epi_new = NA, #set to null
#            exported_eventid_epi = NA #set to null)
#     )
# 
# dat3 <- dat2 %>% 
#     select(sampledate, #sample date
#            variableid_lagos, #2 digit number for variable in lagos data base
#            State, #state observation was made
#            CharacteristicName, # parameter name from lagos
#            sampledepth_m_legacy, #sample depth reported by collection agency
#            datavalue, #result value in lagos units
#            programtableid_lagos, #nothing
#            sampletype_legacy, #how the sample was collected, grab, integrated, depth 
#            sampledepth_m_lagos, #lagos assigned depth
#            samplelayer_lagos, #lagos assigned sample later, EPI/OTHER
#            sampletype_lagos, #lagos assigned type
#            comments_legacy, #all comments/flags associated with data value
#            comments_lagos, #comments we add to data
#            censorcode_flag_lagos, #censor flags we add
#            samplesiteid_legacy, #sample site reported by agancy
#            lagoslakeid, #lagoslakeid
#            samplesiteid_lagos_pre_cluster, #lagos assigned 
#            labmethodname_legacy, #method name if available
#            lakeid_legacy, #not sure
#            labmethodinfo_legacy, #not sue
#            methodqualifier_legacy, #not sure
#            version_comment_lagos, #not sure
#            flag_lagos,
#            varshortname_comment_lagos,
#            lakeid_nhdid, #nhdid
#            depth_comment_lagos,
#            primarysamplesite_flag_lagos,
#            valueid_lagos, #assigned in postgres
#            sitecordid_lagos,
#            samplesiteid_lagos,
#            programid_lagos_us, #nothing right now
#            source_value, #value from source provider
#            source_unit, #units from source provider
#            source_parameter, #parameter name from source provider
#            Obs_Id, #unique id we assign at import
#            qualifier_legacy,
#            qualifier_name_legacy, #type of detection limit, method, reporting
#            detectionlimit_legacy, #detection limit value
#            detectionlimit_unit_legacy, #units of detection limit
#            qualifier_legacy_full,
#            qualifier_legacy_full_cleaned,
#            qualifier_detect_info,
#            lagos_min_depth,
#            legacy_min_depth,
#            sample_delta,
#            epi_depth,
#            lagos_epi_assignment,
#            exported_epi_new,
#            exported_eventid_epi)
# 
# write_csv(dat3,paste0(data_dir,"processed/alldata_processed.csv"))
# 
# #dat3 would be the processed file that gets loaded into postgres
# #yet todo is add observation id to each based on values already loaded in postgres and push file to postgres
# 


variables <- read_csv("lagos_variable.csv")

#create histograms
options(scipen=999)
pdf(file="graphics/histograms.pdf",width=8.5,height=11)
par(mfrow=c(3,2),mar=c(4,10,4,4))

for(i in 1:length(params)) {
    temp <- (as.numeric(out.file$datavalue))[which(out.file$lagos_variablename==variables$variablename[i])]
    if(length(temp)>25) {
        adjbox(x = temp ,main=variables$variablename[i],notch=TRUE,las=1)
        if(!is.na(variables$limit[i])) {abline(h=variables$limit[i],col="red")}
        mtext(side=3,adj=0.1,paste("n=",length(temp))) 
    } else {next()}
}
dev.off()


temp <- out.file %>% filter(lagos_variablename=="Calcium, dissolved") %>% 
    filter(datavalue >20000)

out.file <- out.file %>% left_join(variables %>% select(variableid_lagos,limit), by = c("lagos_variableid" = "variableid_lagos"))
out.file <- out.file %>% filter(datavalue < limit)
out.file <- out.file %>% select(-limit)
out.file <- out.file %>% filter(datavalue >=0)



pdf(file="graphics/distributions.pdf",width=8.5,height=11)
par(mfrow=c(3,2))

for(i in 1:length(params)) {
    temp <- log10(as.numeric(out.file$datavalue)+1)[which(out.file$lagos_variablename==variables$variablename[i])]
    if(length(temp)>25) 
    {
        d <- density(x = temp,na.rm=T)
        plot(d, main=variables$variablename[i])
    } else 
    {next()}
}
dev.off()

