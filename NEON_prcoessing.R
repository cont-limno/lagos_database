library(dplyr)
library(data.table)
library(neonUtilities)
library(tidyverse)

# Set global option to NOT convert all character variables to factors
options(stringsAsFactors=F)


#stackByTable("./NEON/NEON_chem-peri-ses-phyto.zip")


#chem data
chem_data_raw<-read.csv("./NEON_chemandmethods_data.csv")


names(chem_data_raw)[names(chem_data_raw) == "uid"] <- "source_activityid"
names(chem_data_raw)[names(chem_data_raw) == "namedLocation"] <- "source_samplesiteid"
names(chem_data_raw)[names(chem_data_raw) == "collectDate"] <- "sampledate"
names(chem_data_raw)[names(chem_data_raw) == "analyte"] <- "source_parameter"
names(chem_data_raw)[names(chem_data_raw) == "analyteUnits"] <- "source_unit"
names(chem_data_raw)[names(chem_data_raw) == "analyteConcentration"] <- "source_value"
names(chem_data_raw)[names(chem_data_raw) == "remarks"] <- "flag1"
names(chem_data_raw)[names(chem_data_raw) == "externalLabDataQF"] <- "flag2"
names(chem_data_raw)[names(chem_data_raw) == "shipmentWarmQF"] <- "flag3"
names(chem_data_raw)[names(chem_data_raw) == "method"] <- "source_labmethodname"
names(chem_data_raw)[names(chem_data_raw) == "methodDetectionLimit"] <- "source_detectionlimit_value"


chem_data_raw$source_sampledepth<-c("0")

chem_data_raw1<-chem_data_raw[, c('source_activityid',"sampledate","source_samplesiteid",
                           "source_value","source_sampledepth","flag1","source_parameter",
                           "source_unit","flag2","flag3","source_labmethodname","source_detectionlimit_value")]


chem_data_raw1$source_sampletype<-c("INFERRED")

#chla data
chla_raw<-read.csv("./NEON/NEON_chem-peri-ses-phyto/stackedFiles/alg_algaeExternalLabDataPerSample.csv")

names(chla_raw)[names(chla_raw) == "uid"] <- "source_activityid"
names(chla_raw)[names(chla_raw) == "namedLocation"] <- "source_samplesiteid"
names(chla_raw)[names(chla_raw) == "collectDate"] <- "sampledate"
names(chla_raw)[names(chla_raw) == "analyte"] <- "source_parameter"
names(chla_raw)[names(chla_raw) == "plantAlgaeLabUnits"] <- "source_unit"
names(chla_raw)[names(chla_raw) == "analyteConcentration"] <- "source_value"
names(chla_raw)[names(chla_raw) == "externalRemarks"] <- "flag1"
names(chla_raw)[names(chla_raw) == "externalLabDataQF"] <- "flag2"

chla_raw$flag3<-c('')
chla_raw$source_labmethodname<-c('')
chla_raw$source_detectionlimit_value<-c('')


chla_raw$source_sampledepth<-c("0")

chla_raw1<-chla_raw[, c('source_activityid',"sampledate","source_samplesiteid",
                                  "source_value","source_sampledepth","flag1","source_parameter",
                                  "source_unit","flag2","flag3","source_labmethodname","source_detectionlimit_value")]


chla_raw1$source_sampletype<-c("INFERRED")



#secchi data
secchi_raw<-read.csv("./NEON/NEON_depth-secchi/stackedFiles/dep_secchi.csv")
names(secchi_raw)[names(secchi_raw) == "uid"] <- "source_activityid"
names(secchi_raw)[names(secchi_raw) == "date"] <- "sampledate"
names(secchi_raw)[names(secchi_raw) == "namedLocation"] <- "source_samplesiteid"
names(secchi_raw)[names(secchi_raw) == "secchiMeanDepth"] <- "source_value"
names(secchi_raw)[names(secchi_raw) == "remarks"] <- "flag1"
names(secchi_raw)[names(secchi_raw) == "dataQF"] <- "flag2"
secchi_raw$source_parameter<-c("secchiMeanDepth")
secchi_raw$source_unit<-c("m")
secchi_raw$flag3<-c("")
secchi_raw$source_sampledepth<-c(0)
secchi_raw$source_labmethodname<-c("")
secchi_raw$source_detectionlimit_value<-c("")

secchi_raw<-secchi_raw[, c('source_activityid',"sampledate","source_samplesiteid",
                                   "source_value","source_sampledepth","flag1","source_parameter",
                                   "source_unit","flag2","flag3","source_labmethodname","source_detectionlimit_value")]
secchi_raw$source_sampletype<-c("INFERRED")


# #depth data
# profile_raw_temp<-read.csv("./NEON/NEON_depth-profiles/stackedFiles/dep_profileData.csv")
# names(profile_raw_temp)[names(profile_raw_temp) == "uid"] <- "source_activityid"
# names(profile_raw_temp)[names(profile_raw_temp) == "date"] <- "sampledate"
# names(profile_raw_temp)[names(profile_raw_temp) == "eventID"] <- "source_samplesiteid"
# names(profile_raw_temp)[names(profile_raw_temp) == "waterTemp"] <- "source_value"
# names(profile_raw_temp)[names(profile_raw_temp) == "sampleDepth"] <- "source_sampledepth"
# names(profile_raw_temp)[names(profile_raw_temp) == "dataQF"] <- "flag1"
# profile_raw_temp$source_parameter<-c("waterTemp")
# profile_raw_temp$source_unit<-c("celsius")
# profile_raw_temp$flag2<-c("")
# profile_raw_temp$flag3<-c("")
# 
# profile_raw_temp<-profile_raw_temp[, c('source_activityid',"sampledate","source_samplesiteid",
#                                        "source_value","source_sampledepth","flag1","source_parameter",
#                                        "source_unit","flag2","flag3")]
# 
# profile_raw_cond<-read.csv("./NEON/NEON_depth-profiles/stackedFiles/dep_profileData.csv")
# names(profile_raw_cond)[names(profile_raw_cond) == "uid"] <- "source_activityid"
# names(profile_raw_cond)[names(profile_raw_cond) == "date"] <- "sampledate"
# names(profile_raw_cond)[names(profile_raw_cond) == "eventID"] <- "source_samplesiteid"
# names(profile_raw_cond)[names(profile_raw_cond) == "specificConductance"] <- "source_value"
# names(profile_raw_cond)[names(profile_raw_cond) == "sampleDepth"] <- "source_sampledepth"
# names(profile_raw_cond)[names(profile_raw_cond) == "dataQF"] <- "flag1"
# profile_raw_cond$source_parameter<-c("specificConductance")
# profile_raw_cond$source_unit<-c("microsiemensPerCentimeter")
# profile_raw_cond$flag2<-c("")
# profile_raw_cond$flag3<-c("")
# 
# profile_raw_cond<-profile_raw_cond[, c('source_activityid',"sampledate","source_samplesiteid",
#                                        "source_value","source_sampledepth","flag1","source_parameter",
#                                        "source_unit","flag2","flag3")]
# 
# profile_raw_o2<-read.csv("./NEON/NEON_depth-profiles/stackedFiles/dep_profileData.csv")
# names(profile_raw_o2)[names(profile_raw_o2) == "uid"] <- "source_activityid"
# names(profile_raw_o2)[names(profile_raw_o2) == "date"] <- "sampledate"
# names(profile_raw_o2)[names(profile_raw_o2) == "eventID"] <- "source_samplesiteid"
# names(profile_raw_o2)[names(profile_raw_o2) == "dissolvedOxygen"] <- "source_value"
# names(profile_raw_o2)[names(profile_raw_o2) == "sampleDepth"] <- "source_sampledepth"
# names(profile_raw_o2)[names(profile_raw_o2) == "dataQF"] <- "flag1"
# profile_raw_o2$source_parameter<-c("dissolvedOxygen")
# profile_raw_o2$source_unit<-c("milligramsPerLiter")
# profile_raw_o2$flag2<-c("")
# profile_raw_o2$flag3<-c("")
# 
# profile_raw_o2<-profile_raw_o2[, c('source_activityid',"sampledate","source_samplesiteid",
#                                        "source_value","source_sampledepth","flag1","source_parameter",
#                                        "source_unit","flag2","flag3")]
# 
# profile_rbind<-rbind(profile_raw_temp,profile_raw_cond,profile_raw_o2)
# profile_rbind$source_sampletype<-c("DEPTH")
# profile_rbind$source_labmethodname<-c("")
# profile_rbind$source_detectionlimit_value<-c("")
#################

#Bind all data

all_data<-rbind(chem_data_raw1,secchi_raw,chla_raw1)

##adding columns


all_data$source_sampleposition <-"INFERRED"
all_data$obs_id <- paste("NEON",seq.int(nrow(all_data)),sep='-')
all_data$lagoslakeid <- c('')
all_data$source_comments <- paste(all_data$flag1,all_data$flag2,all_data$flag3,sep='$')
all_data$organization_id <- c('NEON')
all_data$organization_name <- c('National Ecological Observatory Network')
all_data$detectionlimit_unit<-all_data$source_unit
all_data$lagos_sampledepth<-all_data$source_sampledepth
all_data$lagos_sampleposition<-all_data$source_sampleposition
all_data$lagos_sampletype<-all_data$source_sampletype

table<-read.csv("./NEON/lagos_variable.csv")

merge_data<-merge(table,all_data,by=c("source_parameter"))
unique(merge_data$source_parameter)

merge_data$lagos_variableid<-merge_data$variableid_lagos
merge_data$datavalue_unit<-merge_data$unitsabbreviation

merge_data$source_parameter<-as.character(merge_data$source_parameter)
merge_data$source_unit<-as.character(merge_data$source_unit)
merge_data$datavalue_unit<-as.character(merge_data$datavalue_unit)


convert<-read.csv("./neon_conversion.csv")


merge_data2<-merge(merge_data,convert,by=c("source_parameter","source_unit","datavalue_unit"),all.x=T)
merge_data2$datavalue<-c(merge_data2$source_value*merge_data2$datavalue_conversion)
merge_data2$source_methodqualifier<-ifelse(merge_data2$lagos_variablename =="Chlorophyll a",
                                       "corrected", NA)

unique(merge_data2$lagos_variablename)
merge_data2$detectionlimit_value<-c(as.numeric(merge_data2$source_detectionlimit_value)*
                                        as.numeric(merge_data2$datavalue_conversion))
merge_data2$valueid<-c('')
merge_data2$source_labmethodid<-merge_data2$source_labmethodname
merge_data2$source_labmethoddescription<-c('')
merge_data2$source_labmethoddescription<-c('')
merge_data2$lagos_comments<-c('')
merge_data2$detectionlimitvalue_conversion<-merge_data2$datavalue_conversion
merge_data2$source_labmethodname<-c('')


final_check<- merge_data2[, c('valueid','obs_id','lagoslakeid', 'sampledate','lagos_variableid',
                          'lagos_variablename', 'datavalue', 'datavalue_unit', 'detectionlimit_value',
                          'datavalue_conversion', 'detectionlimitvalue_conversion', 'lagos_comments',
                          'lagos_sampledepth', 'lagos_sampleposition','lagos_sampletype', 'organization_id',
                          'organization_name','source_activityid', 'source_comments', 'source_detectionlimit_value',
                          'source_labmethoddescription', 'source_labmethodid', 'source_labmethodname', 'source_parameter',
                          'source_sampledepth', 'source_sampleposition', 'source_samplesiteid', 'source_sampletype',
                          'source_unit','source_value',  'source_methodqualifier')]


#unchanged: lagoslakeid, lagos_comments, source_activityid, source_comments, source_detectionlimit_value, 

final_check<-final_check %>% rename(sample_id = obs_id, 
                             sample_date = sampledate,
                             parameter_id = lagos_variableid,
                             parameter_name = lagos_variablename,
                             parameter_value = datavalue,
                             parameter_unit = datavalue_unit,
                             parameter_detectionlimit_value = detectionlimit_value,
                             parameter_conversionfactor = datavalue_conversion,
                             parameter_detectionlimit_conversionfactor = detectionlimitvalue_conversion,
                             sample_depth_m = lagos_sampledepth,
                             sample_depth_flag = lagos_sampleposition,
                             sample_type = lagos_sampletype,
                             source_id = organization_id,
                             source_name = organization_name,
                             source_labmethod_description = source_labmethoddescription,
                             source_labmethod_id = source_labmethodid,
                             source_labmethod_name = source_labmethodname,
                             source_parameter_name = source_parameter,
                             source_sample_depth_m = source_sampledepth,
                             source_sample_position = source_sampleposition,
                             source_sample_siteid = source_samplesiteid,
                             source_sample_type = source_sampletype,
                             source_parameter_unit = source_unit,
                             source_parameter_value = source_value,
                             source_labmethod_qualifier = source_methodqualifier)


final_check<-final_check %>% 
    mutate(source_activityorg_name = "National Ecological Observatory Network",
           source_value_qualifiercode = NA,
           source_detectionlimit_unit = NA,
           source_detectionlimit_condition = NA,
           source_detectionlimit_type = NA,
           source_labmethod_usgspcode = NA)



temp<-final_check

temp<- temp %>% 
    select(sample_id,
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
       source_parameter_value,
       source_parameter_unit,
       source_value_qualifiercode,
       parameter_conversionfactor,
       source_detectionlimit_value,
       source_detectionlimit_unit,
       parameter_detectionlimit_conversionfactor,
       source_detectionlimit_condition,
       source_detectionlimit_type,
       source_labmethod_usgspcode,
       source_labmethod_description,
       source_labmethod_id,
       source_labmethod_name,
       source_labmethod_qualifier,
       source_sample_type
)


final_check<-final_check %>%
    select(sample_id,
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
           source_parameter_value,
           source_parameter_unit,
           source_value_qualifiercode,
           parameter_conversionfactor,
           source_detectionlimit_value,
           source_detectionlimit_unit,
           parameter_detectionlimit_conversionfactor,
           source_detectionlimit_condition,
           source_detectionlimit_type,
           source_labmethod_usgspcode,
           source_labmethod_description,
           source_labmethod_id,
           source_labmethod_name,
           source_labmethod_qualifier,
           source_sample_type
    )



final_check$lagoslakeid<-NA
final_check$source_labmethod_description<-NA
final_check$source_labmethod_name<-NA

final_check$source_detectionlimit_value<-ifelse(final_check$source_detectionlimit_value == "", NA, final_check$source_detectionlimit_value)
temp <- final_check %>% filter(grepl('buoy',source_sample_siteid)) 
temp <- temp %>% mutate(source_sample_siteid = substr(source_sample_siteid,1,4))

# nla<-read.csv("~/GitHub/lagos_database/nla_final.csv")
# names(nla)
#names(temp)


write_csv(final_check,"Final_NEON_data.csv")



