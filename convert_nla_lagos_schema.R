library(tidyverse)
library(lubridate)
dat <- read_csv("Final_NLA_data.csv")

nla_sites <- dat %>% select(source_samplesiteid,LAT_DD,LON_DD) %>% 
    distinct() %>% 
    group_by(source_samplesiteid) %>% 
    mutate(n=n())

dat <- dat %>% 
    rename(sample_id = obs_id,
           sample_date = sampledate,
           parameter_id = lagos_variableid,
           parameter_name = lagos_variablename,
           parameter_value = datavalue,
           parameter_detectionlimit_value = detectionlimit_value,
           sample_depth_flag = lagos_sampleposition,
           sample_depth_m = lagos_sampledepth,
           source_id = organization_id,
           source_name = organization_name,
           source_activityid = source_activityid,
           # source_activityorg_name = ActivityConductingOrganizationText,
           source_sample_siteid = source_samplesiteid,
           source_parameter_name = source_parameter,
           source_parameter_value = source_value,
           source_parameter_unit = source_unit,
           parameter_conversionfactor = datavalue_conversion,
           source_detectionlimit_value = source_detectionlimit_value,
           # source_detectionlimit_unit = detectionlimit_unit_legacy,
           parameter_detectionlimit_conversionfactor = detectionlimitvalue_conversion,
           # source_detectionlimit_condition = ResultDetectionConditionText,
           # source_detectionlimit_type = DetectionQuantitationLimitTypeName,
           # source_value_qualifiercode = MeasureQualifierCode,
           source_labmethod_description = source_labmethoddescription,
           source_labmethod_id = source_labmethodid,
           source_labmethod_name = source_labmethodname,
           source_labmethod_qualifier = source_methodqualifier
           # source_labmethod_usgspcode = USGSPCode
           )

dat <- dat %>% 
    mutate(sample_date = mdy(sample_date),
           source_id = "nla_wqx",
           source_name = "National Lake Assessment",
           source_activityorg_name = "EPA",
           source_detectionlimit_unit = NA,
           source_detectionlimit_condition = NA,
           source_detectionlimit_type = NA,
           source_value_qualifiercode = NA,
           source_labmethod_usgspcode = NA)

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
                           source_labmethod_qualifier
)

out.file <- out.file %>% 
    mutate(parameter_value = ifelse(parameter_name=="Alkalinity",parameter_value * 0.05005,parameter_value),
           sample_depth_flag = ifelse(is.na(sample_depth_m),"INTEGRATED",sample_depth_flag),
           sample_depth_m = ifelse(is.na(sample_depth_m),2,sample_depth_m)) %>% 
    select(-lagoslakeid)

# load in lagos lakeid linking
# join data and add lagos lake id
# drop data with no linking (examine to see how much)
# 
#use script below to organize data and save rds

out.file <- out.file %>% select(sample_id,
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
                           source_labmethod_qualifier
)

saveRDS(out.file,"nla_final.rds")