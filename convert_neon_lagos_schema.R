library(tidyverse)
library(lubridate)
dat <- read_csv("Final_NEON_data.csv")
names(dat)

sites <- read_csv("NEON/kait_neon_sites.csv") %>% 
    select(lagoslakeid,field_site_id)

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
           # source_activityorg_name = source_,
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
    mutate(source_detectionlimit_unit = NA,
           source_detectionlimit_condition = NA,
           source_detectionlimit_type = NA,
           source_value_qualifiercode = NA,
           source_labmethod_usgspcode = NA,
           source_activityorg_name = "NEON",
           field_site_id = substr(source_sample_siteid,1,4)) %>% 
    select(-lagoslakeid) %>% 
    left_join(sites) %>% 
    drop_na(lagoslakeid)

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

saveRDS(out.file,"NEON_Final.rds")
