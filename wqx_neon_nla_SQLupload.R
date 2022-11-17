library(tidyverse)
wqx_final<-readRDS("WQX_Final.rds")
str(wqx_final)
write_csv(wqx_final, "wqx_final.csv")


neon_final<-readRDS("NEON_Final.rds")
write.csv(neon_final, "neon_final.csv")

###################################################################################################################################################

nla_final<-readRDS("nla_final.rds")

#clean data - postgres gave error that there are string values in a double column

temp<-nla_final %>% filter(source_detectionlimit_value == "N/A")
install.packages("naniar")
library(naniar)
temp<-temp %>% replace_with_na(replace = list(source_detectionlimit_value = "N/A"))
temp<-temp %>% filter(source_detectionlimit_value == "N/A")
nla_final<-nla_final %>% replace_with_na(replace = list(source_detectionlimit_value = "N/A"))

temp<-nla_final %>% filter(source_detectionlimit_value == "CALC")
nla_final<-nla_final %>% replace_with_na(replace = list(source_detectionlimit_value = "CALC"))

#add in source_sampletype column
nla_final<-nla_final %>% mutate(source_sample_type = "integrated")




write.csv(nla_final, "nla_final.csv", row.names = FALSE)


wqx_final_1a<-wqx_final[1:3112635,]
wqx_final_1b<-wqx_final[3112636:6225270,]

write_csv(wqx_final_1a, "wqx_final_1a.csv")
write_csv(wqx_final_1b, "wqx_final_1b.csv")
if (condition) {
    
}

getwd()

library(tidyverse)

# temp<-wqx_final %>% replace_na(list(lagoslakeid = "NULL", 
#                                     parameter_id = "NULL",
#                                     parameter_value = "NULL", 
#                                     parameter_detectionlimit_value = "NULL", 
#                                     sample_depth_m = "NULL", 
#                                     parameter_conversionfactor = "NULL", 
#                                     source_detectionlimit_value = "NULL", 
#                                     parameter_detectionlimit_conversionfactor = "NULL", 
#                                     source_labmethod_usgspcode = "NULL"))



temp<-temp %>% mutate_at(c("lagoslakeid", "parameter_id", "parameter_value"), ~ replace_na(.,""))
                         
str(temp)

temp$parameter_value<-

wqx_final$lagoslakeid<-wqx_final$lagoslakeid %>% replace_na("NULL")

wqx_final$parameter_id<-wqx_final$parameter_id %>% replace_na("NULL")

wqx_final$parameter_value<-wqx_final$parameter_value %>% replace_na("NULL")

wqx_final$parameter_detectionlimit_value<-wqx_final$parameter_detectionlimit_value %>% replace_na("NULL")

wqx_final$sample_depth_m<-wqx_final$sample_depth_m %>% replace_na("NULL")

wqx_final$parameter_conversionfactor<-wqx_final$parameter_conversionfactor %>% replace_na("NULL")

wqx_final$source_detectionlimit_value<-wqx_final$source_detectionlimit_value %>% replace_na("NULL")

wqx_final$parameter_detectionlimit_conversionfactor<-wqx_final$parameter_detectionlimit_conversionfactor %>% replace_na("NULL")

wqx_final$source_labmethod_usgspcode<-wqx_final$source_labmethod_usgspcode %>% replace_na("NULL")

str(wqx_final)


#####################################################################################################################################################
#solving ghost value issue

wqx_final<-readRDS("WQX_Final.rds")
write_csv(wqx_final, "WQX_FINAL.csv")

unique(wqx_final$parameter_detectionlimit_conversionfactor)
temp<-wqx_final %>% filter(parameter_detectionlimit_conversionfactor == "ug/l")


str(wqx_final)

unique(wqx_final$source_detectionlimit_unit)

unique(wqx_final$parameter_detectionlimit_value)
sum(is.na(wqx_final$parameter_detectionlimit_value))

write_tsv(wqx_final, "WQX_FINAL.tsv")

wqx_final_test<-wqx_final %>% slice(1:10)
write_csv(wqx_final_test, "wqx_test.csv")
temp<-wqx_final_test %>% filter(parameter_detectionlimit_conversionfactor == "ug/l")

temp<-wqx_final_test %>% relocate(wqp_parameter_usgspcode, .after = source_detectionlimit_type)
wqx_final_test<-wqx_final_test %>% relocate(wqp_parameter_usgspcode, .after = source_detectionlimit_type)

wqx_final<-wqx_final %>% relocate(wqp_parameter_usgspcode, .after = source_detectionlimit_type)

#####################################################################################################################################################
#oct 18, 2022
#Going to slice wqx into 4 parts and try to upload into postgres
#test
wqx<-read.csv("~/GitHub/lagos_database/wqx_final.csv")
wqx1<-wqx %>% slice(1:50)
write_csv(wqx1, "wqx_test.csv")
#worked

wqx1<-wqx %>% slice(1:1560881) %>% 
    mutate(source_comments = ifelse(sample_id == "CO-181232", gsub("'","",wqx$source_comments), source_comments))
    
write_csv(wqx1, "wqx_final1.csv")

#temp<-wqx1 %>% filter(sample_id == "CO-181232")
#temp$source_comments<-gsub("'","",temp$source_comments)
#temp<-wqx1 %>% filter(sample_id == "CO-181218")


temp<-wqx1
temp$source_comments<-gsub("'","",temp$source_comments) #works - removes apostrophes for all values in this column
temp<-temp %>% filter(sample_id == "CO-181218")

wqx1$source_comments<-gsub("'","",wqx1$source_comments)
write_csv(wqx1, "wqx_final1.csv")

#table #2
wqx2<- wqx %>%  slice(1560882:3121762)
wqx2$source_comments<-gsub("'","",wqx2$source_comments)
write_csv(wqx2, "wqx_final2.csv")


#table#3
wqx3<- wqx %>%  slice(3121763:4682644)
wqx3$source_comments<-gsub("'","",wqx3$source_comments)
write_csv(wqx3, "wqx_final3.csv")

#table#4
wqx4<- wqx %>%  slice(4682645:6243527)
wqx4$source_comments<-gsub("'","",wqx4$source_comments)
write_csv(wqx4, "wqx_final4.csv")

####################################################################################################################################################

#building a final cleaned dataset (same as the one in postgres) 

wqx_final<-rbind(wqx1, wqx2, wqx3, wqx4)
neon_final<-read.csv("~/GitHub/lagos_database/Final_NEON_data.csv")
nla_final <-read.csv("~/GitHub/lagos_database/nla_final.csv")

wqx_neon<-rbind(wqx_final, neon_final) # did not work

setdiff(wqx_final, neon_final)
#Error in `setdiff()`:
#! `x` and `y` are not compatible.
#Cols in `y` but not `x`: `source_parameter_unit`, `source_labmethod_usgspcode`.
#Cols in `x` but not `y`: `source_parameter_units`, `wqp_parameter_usgspcode`.

#fix source_parameter_units to unit in wqx dataset
#update name from 'wqp_parameter_usgspcode' to "source_labmethod_usgspcode" in wqx dataset

wqx_final<-wqx_final %>% rename(source_parameter_unit = source_parameter_units)
wqx_final<-wqx_final %>% rename(source_labmethod_usgspcode = wqp_parameter_usgspcode)

#now try to rbind again - will try to rbind all 3 again since the errors were in wqx dataset names and neon and nla datatset column names seem consisitent

wqx_neon_nla<-rbind(wqx_final, neon_final, nla_final) 


saveRDS(wqx_neon_nla, file = "wqx_us_final.rds")




####################################################################################################################################################

#epi assignment script Nov8th 2022

#pull our required data

epi_data<-wqx_neon_nla %>% select(
    sample_id,
    lagoslakeid,
    parameter_name,
    source_sample_siteid,
    source_sample_type,
    sample_depth_m,
)

#split dataset - create 1 for secchi and one without secchi

secchi_data <- epi_data %>% filter(parameter_name == "Secchi")
epi_data_nosecchi<- epi_data %>% filter(parameter_name != "Secchi")

#split epi_data_nosecchi - 1 dataset for specified - 1 dataset for integragrated or inferred -- these are in sample type column

epi_data_nosecchi_integ_infer<-epi_data_nosecchi %>% filter(source_sample_type == "integrated" | source_sample_type == "INFERRED")
unique(epi_data_nosecchi_integ_infer$source_sample_type)

epi_data_nosecchi_specified<-epi_data_nosecchi %>% filter(source_sample_type == "SPECIFIED")


#create a column in integ_infer table - cal it lagos stratum - fill with 0's
epi_data_nosecchi_integ_infer<-epi_data_nosecchi_integ_infer %>% mutate(
    lagos_stratum = "0"
)


# IDENTIFY EPI INFORMATION

# This is done by lake (one epi depth for the entire lake)
# lagos_min_depth based on lowest 10th percentile of sampledepth information to ensure that the shallowest sampledepth with the most data is used
# legacy_min_depth just shows the min depth of the data
# 2.5 meter buffer provided below min depth value for epi sample identification (add 2.5 meters to min depth to get max epi depth)
data_quant_min_depth <- epi_data_nosecchi_specified %>% 
    group_by(lagoslakeid) %>%
    mutate(lagos_min_depth = quantile(sample_depth_m, probs = .1 ), legacy_min_depth = min(sample_depth_m)) %>%
    ungroup()

#HERE WE DEFINE POTENTIAL SAMPLES THAT COULD GO INTO EPI

# LINE 47: if sampledelta is less than 2.5m, epi depth might be identified as 0m. Just to make sure, add 2m to the min depth so that epi depth is non zero.
#These numbers (2.5m and 2m) were selected by the lagos team as a best fit for the data. They are subject to change depending on further decisions, but are used concretely for now


dat <- data_quant_min_depth %>% 
    mutate(sample_delta = lagos_min_depth - legacy_min_depth) %>% 
    mutate(epi_depth = ifelse(sample_delta <= 2.5, lagos_min_depth + 2, legacy_min_depth + 2)) %>%  
    mutate(epi_depth = ceiling(epi_depth)) %>% #round epi depth to the nearest greater integer
    mutate(epi_depth = ifelse(epi_depth <= 10, epi_depth, NA)) ##remove anything greater than 10m epi depth 

##assign EPI criteria based off new epi_depth column 
dat$lagos_epi_assignment <- NA

##if the sampledepth is smaler than the epi_depth identify the record as an EPI sample (noted by a 1)
dat$lagos_epi_assignment <- ifelse(dat$sample_depth_m <= dat$epi_depth, 1, 0) 

#done creating epi assingment for epi data no secchi specified samples
#now going to create epi assginment column in epi data no secchi _infer_integ and secchi data - fill in values of 1 for both

epi_data_nosecchi_integ_infer<- epi_data_nosecchi_integ_infer %>% mutate(
    lagos_epi_assignment = "1"
)

epi_data_nosecchi_integ_infer<-epi_data_nosecchi_integ_infer %>% select(-lagos_stratum)

secchi_data<-secchi_data %>% mutate(
    lagos_epi_assignment = "1"
)

#select columns we want from dat - make dat1 - then rbind the 3 datasets

dat1<-dat %>% select(sample_id, lagoslakeid, parameter_name, source_sample_siteid, source_sample_type, sample_depth_m, lagos_epi_assignment)


fulldata<-rbind(epi_data_nosecchi_integ_infer, secchi_data, dat1)
saveRDS(fulldata, file = "wqx_us_epi_assignment.rds")

####################################################################################################################################################
#Nov 14 2022
#adding gps coords to epi assignment dataset

#subset data into wqx, neon, nla


neon<-wqx_us_epi_assignment %>% filter(str_detect(sample_id, "NEON")) #created neon dataset

temp1<-wqx_us_epi_assignment %>% 
    filter(
        !str_detect(sample_id, "NEON")) #created NLA + WQX dataset

nla<-temp1 %>% filter(
    str_detect(sample_id, "NLA") #created NLA dataset
)

wqx<-temp1 %>% filter(
    !str_detect(sample_id, "NLA") #created WQX dataset
)


#get distinct values for source_sample_siteid for all 3 datasets
#Data may have same: source_sample_siteid but a diff sample_id – therefore we distinct on source_sample_id to get only unique values

neon<-neon %>% distinct(source_sample_siteid, .keep_all = TRUE)
nla<-nla %>% distinct(source_sample_siteid, .keep_all = TRUE)
wqx<-wqx %>% distinct(source_sample_siteid, .keep_all = TRUE)


#load in files with gps coordinates

wqx_coords<-read.csv("~/GitHub/lagos_database/station.csv")
nla_coords<-read.csv("~/GitHub/lagos_database/NLA/NLA_LakeLink_FINAL_26SE22.csv") #sept26 in nla folder in github #3666 obs

#for neon - we need to manually get the gps coords - ASK NOAH

#join gps coords to datasets

temp<-left_join(nla, nla_coords, by = "lagoslakeid") #results in 5995 obs
temp<-temp %>% distinct(source_sample_siteid, .keep_all = TRUE) #3209 obs - same as in nla dataset

temp1<-nla_coords %>% rename(source_sample_siteid = SITE_ID)



temp2<-temp1 %>% 
    select(source_sample_siteid, INDEX_LAT, INDEX_LON) %>% 
    left_join(nla, temp1, by = "source_sample_siteid") %>% 
    distinct(source_sample_siteid, .keep_all = TRUE) #3377 obs - wrong


temp2<-temp1 %>% 
    select(source_sample_siteid, INDEX_LAT, INDEX_LON) %>% 
    right_join(nla, temp1, by = "source_sample_siteid") %>% 
    distinct(source_sample_siteid, .keep_all = TRUE) #3209 obs - right

temp2<-temp1 %>% 
    select(source_sample_siteid, INDEX_LAT, INDEX_LON) %>% 
    inner_join(nla, temp1, by = "source_sample_siteid") #3486 obs





temp2<-left_join(nla, temp1, by = "source_sample_siteid")#3486 obs
temp2<-temp2 %>% distinct(source_sample_siteid, .keep_all = TRUE) #3209 obs - same as in nla dataset

