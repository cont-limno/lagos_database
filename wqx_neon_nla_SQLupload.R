library(tidyverse)
setwd("C:/Users/Arnab/Documents/GitHub/lagos_database")
getwd()
wqx_final<-readRDS("WQX_Final.rds")
str(wqx_final)
write_csv(wqx_final, "wqx_final.csv")


neon_final<-readRDS("NEON_Final.rds")
write.csv(neon_final, "neon_final.csv")

temp<-epi_export_daily %>% filter(lagoslakeid == "196835")

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
wqx1<- read.csv("~/GitHub/lagos_database/wqx_final1.csv") 
wqx2<-read.csv("~/GitHub/lagos_database/wqx_final2.csv")
wqx3<-read.csv("~/GitHub/lagos_database/wqx_final3.csv")
wqx4<-read.csv("~/GitHub/lagos_database/wqx_final4.csv")

wqx_final<-rbind(wqx1, wqx2, wqx3, wqx4)
neon_final<-read.csv("~/GitHub/lagos_database/Final_NEON_data.csv") #updates dec 19th 2022
nla_final <-read.csv("~/GitHub/lagos_database/nla_final.csv")

# wqx_neon<-rbind(wqx_final, neon_final) # did not work
# 
# setdiff(wqx_final, neon_final)
#Error in `setdiff()`:
#! `x` and `y` are not compatible.
#Cols in `y` but not `x`: `source_parameter_unit`, `source_labmethod_usgspcode`.
#Cols in `x` but not `y`: `source_parameter_units`, `wqp_parameter_usgspcode`.

#fix source_parameter_units to unit in wqx dataset
#update name from 'wqp_parameter_usgspcode' to "source_labmethod_usgspcode" in wqx dataset

wqx_final<-wqx_final %>% rename(source_parameter_unit = source_parameter_units)
wqx_final<-wqx_final %>% rename(source_labmethod_usgspcode = wqp_parameter_usgspcode)

#fix neon lagoslakeids
neon_final<-neon_final %>%
    mutate(lagoslakeid = case_when(source_sample_siteid == "BARC" ~ '186598',
                                   source_sample_siteid == "TOOK" ~ 'NA',
                                   source_sample_siteid == "CRAM" ~ '96686',
                                   source_sample_siteid == "SUGG" ~ '188788',
                                   source_sample_siteid == "PRLA" ~ '349775',
                                   source_sample_siteid == "LIRO" ~ '495',
                                   TRUE ~ 'NA' ))

neon_final<-neon_final %>% filter(source_sample_siteid != "TOOK") #dropped took - in alaska

#now try to rbind again - will try to rbind all 3 again since the errors were in wqx dataset names and neon and nla datatset column names seem consisitent

wqx_neon_nla<-rbind(wqx_final, neon_final, nla_final) 
unique(wqx_neon_nla$lagoslakeid)

str(wqx_neon_nla)
#convert lagoslakeid to numeric
wqx_neon_nla$lagoslakeid = as.numeric(as.character(wqx_neon_nla$lagoslakeid)) 
str(wqx_neon_nla)



dat <- wqx_neon_nla %>%
    
    filter(parameter_name == "Nitrogen, total") %>%
    
    filter(source_id == "nla_wqx") %>%
    
    filter(source_parameter_unit == "ug/l") %>%
    
    filter(parameter_conversionfactor == 1000)



dat <- dat %>%
    
    mutate(parameter_value = source_parameter_value,
           
           parameter_detectionlimit_value = source_detectionlimit_value,
           
           parameter_conversionfactor = 1,
           
           parameter_detectionlimit_conversionfactor = 1)



wqx_neon_nla <- wqx_neon_nla %>% filter(!sample_id %in% dat$sample_id)

wqx_neon_nla <- rbind(wqx_neon_nla,dat)


#fixed - save RDS
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

#split epi_data_nosecchi - 1 dataset for specified - 1 dataset for integrated or inferred -- these are in sample type column

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

wqx_us_epi_assignment<-read_rds("wqx_us_epi_assignment.rds")
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
#nla<-nla %>% distinct(source_sample_siteid, .keep_all = TRUE)
nla<-nla %>% select(lagoslakeid, source_sample_siteid) %>% distinct()
wqx<-wqx %>% distinct(source_sample_siteid, .keep_all = TRUE)


#load in files with gps coordinates

wqx_coords<-read.csv("~/GitHub/lagos_database/station.csv")
nla_coords<-read.csv("~/GitHub/lagos_database/NLA/NLA_LakeLink_FINAL_26SE22.csv") #sept26 in nla folder in github #3666 obs

#for neon - we need to manually get the gps coords - ASK NOAH

#join gps coords to datasets

#temp<-left_join(nla, nla_coords, by = "lagoslakeid") #results in 5995 obs
#temp<-temp %>% distinct(source_sample_siteid, .keep_all = TRUE) #3209 obs - same as in nla dataset - the number we want

temp1<-nla_coords %>% rename(source_sample_siteid = SITE_ID) %>%     
    select(source_sample_siteid, INDEX_LAT, INDEX_LON) %>% distinct() %>% group_by(source_sample_siteid) %>% 
    mutate(INDEX_LAT = mean(INDEX_LAT), INDEX_LON = mean(INDEX_LON)) %>% ungroup() %>% distinct()


temp2<-nla %>% left_join(temp1)

#works - now rejoin with nla
nla<-nla %>% left_join(temp1)



#JOIN GPS COORDS WITH WQX DATA

#reduce datatable to vars we want from wqx_coords

wqx_coords<- wqx_coords %>% select(MonitoringLocationIdentifier, LatitudeMeasure, LongitudeMeasure) %>% 
    rename(source_sample_siteid = MonitoringLocationIdentifier)

temp1<-wqx_coords %>%  select(source_sample_siteid, LatitudeMeasure, LongitudeMeasure) %>% distinct() %>% group_by(source_sample_siteid) %>% 
    mutate(LatitudeMeasure = mean(LatitudeMeasure), LongitudeMeasure = mean(LongitudeMeasure)) %>% ungroup() %>% distinct()


temp2<-wqx %>% left_join(temp1) %>% select(lagoslakeid, source_sample_siteid, LatitudeMeasure, LongitudeMeasure)

#works so join with wqx dataset

wqx<-wqx %>% left_join(temp1 )%>% select(lagoslakeid, source_sample_siteid, LatitudeMeasure, LongitudeMeasure)


# we need to assign lat lon values to neon manually
#first keep only first part of name in source_sample_siteid
#get lat lon for each of the source sample site ids from the internet
#manually assign them

# temp<-neon
# 
# 
# temp$source_sample_siteid<- str_replace(temp$source_sample_siteid,"\\..*","")
# unique(temp$source_sample_siteid)
# 
# 
# temp<-temp %>%
#     mutate(Lat = case_when(source_sample_siteid == "BARC" ~ '29.675982',
#                            source_sample_siteid == "TOOK" ~ '68.630692',
#                            source_sample_siteid == "CRAM" ~ '46.209675',
#                            source_sample_siteid == "SUGG" ~ '29.68778',
#                            source_sample_siteid == "PRLA" ~ '47.15909',
#                            source_sample_siteid == "LIRO" ~ '45.998269',
#                            TRUE ~ 'NA' ))
# 
# 
# temp<-temp %>%
#     mutate(Lon = case_when(source_sample_siteid == "BARC" ~ '-82.008414',
#                            source_sample_siteid == "TOOK" ~ '-149.61064',
#                            source_sample_siteid == "CRAM" ~ '-89.473688',
#                            source_sample_siteid == "SUGG" ~ '-82.017745',
#                            source_sample_siteid == "PRLA" ~ '-99.11388',
#                            source_sample_siteid == "LIRO" ~ '-89.704767',
#                            TRUE ~ 'NA' ))

# 
# neon$source_sample_siteid<- str_replace(neon$source_sample_siteid,"\\..*","")
# unique(neon$source_sample_siteid)
# 
# 
# neon<-neon %>%
#     mutate(Lat = case_when(source_sample_siteid == "BARC" ~ '29.675982',
#                            source_sample_siteid == "TOOK" ~ '68.630692',
#                            source_sample_siteid == "CRAM" ~ '46.209675',
#                            source_sample_siteid == "SUGG" ~ '29.68778',
#                            source_sample_siteid == "PRLA" ~ '47.15909',
#                            source_sample_siteid == "LIRO" ~ '45.998269',
#                            TRUE ~ 'NA' ))
# 
# 
# neon<-neon %>%
#     mutate(Lon = case_when(source_sample_siteid == "BARC" ~ '-82.008414',
#                            source_sample_siteid == "TOOK" ~ '-149.61064',
#                            source_sample_siteid == "CRAM" ~ '-89.473688',
#                            source_sample_siteid == "SUGG" ~ '-82.017745',
#                            source_sample_siteid == "PRLA" ~ '-99.11388',
#                            source_sample_siteid == "LIRO" ~ '-89.704767',
#                            TRUE ~ 'NA' ))
# 
# #select only columns we want
# 
# neon<-neon %>% select(lagoslakeid, source_sample_siteid, Lat, Lon)


#rename lat, lon columns in nla and wqx to keep everything uniform

wqx<-wqx %>% rename(Lat = LatitudeMeasure, Lon = LongitudeMeasure)
nla<-nla %>% rename(Lat = INDEX_LAT, Lon = INDEX_LON)


# names(nla)
# names(wqx)
# names(neon)

#combine nla, wqx, neon

# Us_final_lat_lon <- rbind(wqx, nla, neon)
# write_csv(Us_final_lat_lon, "US_final_lat_lon.csv")


#nov 30th
# updated_neon<-read.csv("~/GitHub/lagos_database/Final_NEON_data.csv") #updated neon data from NOAH - 11k obs
# 
# updated_neon<-updated_neon %>% select(lagoslakeid, source_sample_siteid) %>% distinct() #6 unique sites

#adding lat lons
neon<-neon %>%
    mutate(Lat = case_when(source_sample_siteid == "BARC" ~ '29.675982',
                           source_sample_siteid == "CRAM" ~ '46.209675',
                           source_sample_siteid == "SUGG" ~ '29.68778',
                           source_sample_siteid == "PRLA" ~ '47.15909',
                           source_sample_siteid == "LIRO" ~ '45.998269',
                           TRUE ~ 'NA' ))


neon<-neon %>%
    mutate(Lon = case_when(source_sample_siteid == "BARC" ~ '-82.008414',
                           source_sample_siteid == "CRAM" ~ '-89.473688',
                           source_sample_siteid == "SUGG" ~ '-82.017745',
                           source_sample_siteid == "PRLA" ~ '-99.11388',
                           source_sample_siteid == "LIRO" ~ '-89.704767',
                           TRUE ~ 'NA' ))    

neon<-neon %>% select(lagoslakeid, source_sample_siteid, Lat, Lon)

# updated_neon<-updated_neon %>%
#     mutate(lagoslakeid = case_when(source_sample_siteid == "BARC" ~ '186598',
#                            source_sample_siteid == "TOOK" ~ 'NA',
#                            source_sample_siteid == "CRAM" ~ '96686',
#                            source_sample_siteid == "SUGG" ~ '188788',
#                            source_sample_siteid == "PRLA" ~ '349775',
#                            source_sample_siteid == "LIRO" ~ '495',
#                            TRUE ~ 'NA' ))


#links used to find lagos lakeid and lat lons
# https://cont-limno.github.io/lagos-map/
#https://portal.edirepository.org/nis/mapbrowse?packageid=edi.854.1 #lake_information file
#https://www.neonscience.org/field-sites/prla #neon sites website

#join neon data to wqx and nla
Us_final_lat_lon<-rbind(wqx, nla, neon)
#write csv
write_csv(Us_final_lat_lon, "US_final_lat_lon.csv")


####################################################################################################################################################
#modifying austins scripts to create lagos US epi export _ DEC 12
setwd("C:/Users/Arnab/Documents/GitHub/lagos_database")
getwd()
US_final<-read_rds("wqx_us_final.rds")
us_final_epi<-read_rds("wqx_us_epi_assignment.rds")
us_lat_lon_final<-read.csv("~/GitHub/lagos_database/Us_final_lat_lon.csv")

#create 1 dataset that includes epi and lat_lon
US_final<-US_final %>% left_join(us_final_epi)
US_final<-US_final %>% left_join(us_lat_lon_final)


temp<-US_final %>% filter(sample_depth_m < 0)
US_final<-US_final %>% filter(sample_depth_m > -99) %>% 
    mutate(sample_depth_m = abs(sample_depth_m))


rm(us_final_epi)
rm(us_lat_lon_final)

temp<-US_final %>% select(parameter_name, parameter_id) %>% 
    distinct() %>% 
    group_by(parameter_id) %>% 
    mutate(n=n()) %>% 
    filter(n>1) %>% 
    ungroup()

US_final<-US_final %>% 
    mutate(parameter_name = if_else(parameter_name == "pH, field or closed", "pH, field or lab", parameter_name))

US_final<-US_final %>% 
    mutate(parameter_name = if_else(parameter_name == "Phosphorus, soluable reactive orthophosphate", "Phosphorus, soluble reactive orthophosphate", parameter_name))

temp2<-US_final %>% 
    filter(parameter_name == "Alkalinity, bicarbonate" & parameter_id == 2)

US_final<-US_final %>% 
    filter(!sample_id %in% temp2$sample_id)

temp2<-temp2 %>% 
    mutate(parameter_value = source_parameter_value) %>% 
    mutate(parameter_id = 1) %>% 
    mutate(parameter_name = "Alkalinity") %>% 
    mutate(parameter_conversionfactor = 1) %>% 
    mutate(parameter_detectionlimit_conversionfactor = 1) %>% 
    mutate(parameter_detectionlimit_value = source_detectionlimit_value)

US_final<-rbind(US_final, temp2)

temp2<-US_final %>% 
    filter(parameter_name == "ANC")

US_final<-US_final %>% 
    filter(!sample_id %in% temp2$sample_id)

temp2<-temp2 %>% 
    mutate(parameter_id = 1) %>% 
    mutate(parameter_name = "Alkalinity")
    

US_final<-rbind(US_final, temp2)

#remove NA's in NLA dataset
temp2<-US_final %>% filter(str_detect(sample_id, "NLA")) %>% 
    filter(is.na(parameter_value)) %>% 
    filter(is.na(parameter_detectionlimit_value))
US_final<-US_final %>% 
    filter(!sample_id %in% temp2$sample_id)


#EPI WORK STARTS HERE

#check for duplicates
US_final<-US_final %>% 
    group_by(sample_date, source_sample_siteid, sample_depth_m, parameter_name, source_id) %>% 
    slice_sample(n=1) %>% ungroup() #randomly selecting value to keep. EG. might have 4 duplicates - will keep 1 at random - demoves duplicates

gc()


clustersites<-read.csv("~/GitHub/lagos_database/siteclusters_7DEC22.csv") %>% 
    select(lagoslakeid, source_sample_siteid, cl_name, clus_lat, clus_lon)

US_final<-US_final %>% left_join(clustersites)

#censorcodes of US_FINAL
#2023/05/24



#Censorcodes - 
str(US_final)
temp<-US_final %>% 
    filter(!is.na(parameter_value)) %>% 
    filter(is.na(as.numeric(parameter_value))) #checked for non numerics

US_final<-US_final %>% mutate(parameter_value = as.numeric(parameter_value))



LE5<-US_final %>% filter(is.na(parameter_value) & !is.na(parameter_detectionlimit_value))

LE5<-LE5 %>% mutate(parameter_value = parameter_detectionlimit_value, censorcode = "LE5") %>% 
    group_by(parameter_name) %>% 
    mutate(upper = boxplot.stats(LE5$parameter_detectionlimit_value, coef = 4)$stats[5]) %>% 
    ungroup() 

LE5.discard<-LE5 %>% filter(parameter_detectionlimit_value > upper)
LE5<-LE5 %>% filter(!sample_id %in% LE5.discard$sample_id)
LE5<-LE5 %>% select(-upper)

lagos_variable<-read.csv("~/GitHub/lagos_database/lagos_variable.csv") %>% 
    select(variableid_lagos, limit_high) %>% 
    rename(parameter_id = variableid_lagos)

LE5<-LE5 %>% left_join(lagos_variable)

LE5.discard2<-LE5 %>% filter(parameter_value >= limit_high)
LE5<-LE5 %>% filter(!sample_id %in% LE5.discard2$sample_id)

discards<-c(LE5.discard$sample_id, LE5.discard2$sample_id)
    
US_final<-US_final %>% filter(!sample_id %in% discards)    
    
censorcode<-LE5
censorcode<-censorcode %>% select(-limit_high)

LE6<-US_final %>% 
    filter(!sample_id %in% censorcode$sample_id) %>% 
    filter(parameter_value == 0 & is.na(parameter_detectionlimit_value) & is.na(source_detectionlimit_condition)) 
LE6<-LE6 %>% mutate(censorcode = "LE6")
censorcode<-rbind(censorcode, LE6)

NC1<-US_final %>% 
    filter(!sample_id %in% censorcode$sample_id) %>% 
    filter(parameter_value > parameter_detectionlimit_value & !is.na(source_detectionlimit_condition))
NC1<-NC1 %>% mutate(censorcode = "NC1")
censorcode<-rbind(censorcode, NC1)

NC2<-US_final %>% 
    filter(!sample_id %in% censorcode$sample_id) %>% 
    filter(parameter_value > parameter_detectionlimit_value & is.na(source_detectionlimit_condition))
NC2<-NC2 %>% mutate(censorcode = "NC2")
censorcode<-rbind(censorcode, NC2)

NC3<-US_final %>% 
    filter(!sample_id %in% censorcode$sample_id) %>% 
    filter(is.na(parameter_detectionlimit_value) & !is.na(source_detectionlimit_condition))
NC3<-NC3 %>% mutate(censorcode = "NC3")
censorcode<-rbind(censorcode, NC3)

NC4<-US_final %>% 
    filter(!sample_id %in% censorcode$sample_id) %>% 
    filter(is.na(parameter_detectionlimit_value) & is.na(source_detectionlimit_condition))
NC4<-NC4 %>% mutate(censorcode = "NC4")
censorcode<-rbind(censorcode, NC4)




#have detectionlimit, datavalue < detectionlimit, with qualifier or comments

LE1<-US_final %>% 
    filter(!sample_id %in% censorcode$sample_id) %>% 
    filter(parameter_detectionlimit_value >= parameter_value & !is.na(source_detectionlimit_condition))
LE1<-LE1 %>% mutate(censorcode = "LE1")
censorcode<-rbind(censorcode, LE1)

#have detectionlimit, datavalue < detectionlimit, no qualifier or comments

LE2<-US_final %>% 
    filter(!sample_id %in% censorcode$sample_id) %>% 
    filter(parameter_detectionlimit_value >= parameter_value & is.na(source_detectionlimit_condition))
LE2<-LE2 %>% mutate(censorcode = "LE2")
censorcode<-rbind(censorcode, LE2)

gc()

temp<-US_final %>% filter(!sample_id %in% censorcode$sample_id)

temp<-censorcode %>% 
    filter(parameter_name == "secchi_m") %>% 
    select(censorcode) %>% 
    distinct()

censorcode<-censorcode %>% 
    mutate(censorcode = if_else(parameter_name == "secchi_m" & censorcode == "LE6", "NC4", censorcode))

US_final<-censorcode

#censorcoded added


names(US_final)
#rename vars
#US_final<-US_final %>% rename(parameter_name = variableshortname)
US_final<-US_final %>% rename(cluster_id = cl_name)
US_final<-US_final %>% rename(cluster_lat_dd = clus_lat)
US_final<-US_final %>% rename(cluster_lon_dd = clus_lon)
US_final<-US_final %>% rename(source_samplesite_lat_dd = Lat)
US_final<-US_final %>% rename(source_samplesite_lon_dd = Lon)

US_final<-US_final %>% select(-sample_depth_flag)

US_final<-rbind(US_final, temp5)
temp<-US_final %>% group_by(sample_id) %>% 
    summarise(n=n()) %>% 
    filter(n>1) %>% 
    ungroup()

saveRDS(US_final, "US_final_2023_05_24.rds")

###############

#US_epi checks

US_epi<-US_final %>% filter(lagos_epi_assignment == 1)
temp<-US_epi %>% filter(is.na(cluster_id))
US_epi<-US_epi %>% drop_na(cluster_id)


##select the shallowest event for a given parameter, location, date

US_epi<-US_epi %>% group_by(parameter_name, source_sample_siteid, sample_date, source_id) %>%
    slice_min(sample_depth_m, with_ties = FALSE, n = 1) %>% 
    ungroup()

gc()    

#pull out nla data

NLA_epi_clusters<-US_epi %>% filter(str_detect(sample_id, "NLA"))
US_epi <- US_epi %>% filter(!str_detect(sample_id, "NLA"))
#%>% 
    #select(lagoslakeid, cl_name) %>% distinct()
#Multiple_NLA_epi_clusters_in_same_lake<-NLA_epi_clusters %>% group_by(lagoslakeid) %>% 
    #summarise(n=n()) %>% filter(n>1) #NLA epi data with more than 1 clusters - send to kath to investigate

#temp<-Multiple_NLA_epi_clusters_in_same_lake %>% right_join(US_epi)
# %>%select(lagoslakeid, sample_id, sample_date, source_sample_siteid, Lat, Lon, cl_name, clus_lat, clus_lon) %>% distinct()

#temp<-Multiple_NLA_epi_clusters_in_same_lake %>% 
    #filter(lagoslakeid %in% US_epi)


#write_csv(Mulitple_NLA_epi_clusters_in_same_lake, "Mulitple_NLA_epi_clusters_in_same_lake.csv")
# Mulitple_NLA_epi_clusters_in_same_lake<-US_epi %>% filter(str_detect(sample_id, "NLA")) %>% 
#     select(sample_id, lagoslakeid, sample_date, source_sample_siteid, Lat, Lon, cl_name, clus_lat, clus_lon) %>% distinct()
# temp<-Mulitple_NLA_epi_clusters_in_same_lake %>% group_by(lagoslakeid) %>% 
#     summarise(n=n()) %>% filter(n>1)
# 
# 
# Mulitple_NLA_epi_clusters_in_same_lake<-US_epi %>% filter(str_detect(sample_id, "NLA")) %>% 
#     select(lagoslakeid, sample_date, Lat, Lon, cl_name, clus_lat, clus_lon) %>% distinct()
# temp<-Mulitple_NLA_epi_clusters_in_same_lake %>% group_by(lagoslakeid) %>% 
#     summarise(n=n()) %>% filter(n>1)

#pull out neon and wqx data

#US_epi_clusters<-US_epi %>% filter(!lagoslakeid %in% NLA_epi_clusters$lagoslakeid) 


#pull out neon
NEON_epi_clusters<-US_epi %>% filter(str_detect(sample_id, "NEON")) #%>% 
US_epi <- US_epi %>% filter(!str_detect(sample_id, "NEON"))


# US_epi_clusters<-US_epi_clusters %>% filter(!lagoslakeid %in% NEON_epi_clusters$lagoslakeid) 
# 
# 
# NLA_NEON_clusters<-rbind(NLA_epi_clusters, NEON_epi_clusters)
# 
# temp<-NLA_NEON_clusters %>% left_join(US_epi)
# 
# US_epi<-US_epi %>% filter(!lagoslakeid %in% temp$lagoslakeid)
# US_epi<-rbind(US_epi, temp)
# 
# rm(NLA_epi_clusters)
# rm(NEON_epi_clusters)
# rm(US_epi_clusters)
# rm(temp)

gc()
##identify primary site within each lake
US_epi<-US_epi %>% group_by(lagoslakeid, sample_date, cluster_id, parameter_name) %>% 
    slice_sample(n = 1) %>% 
    ungroup() #on a given day, cluster, and lake, we want to ensure that we only take 1 sample per parameter
saveRDS(US_epi, "US_epi_before_bestcluster_selection.rds")

samples<-US_epi %>% group_by(sample_date, lagoslakeid, cluster_id) %>% 
    summarise(total_samples_count = n(), 
              primary_samples_count = sum(parameter_id == 6 | parameter_id == 27 | parameter_id == 9)) # in approx 20k lakes - they were sampled 1,536402 times for these 3 vars

temp<-samples %>% group_by(lagoslakeid, sample_date, cluster_id) %>% 
    summarise(n=n()) %>% 
    filter(n>1) %>% 
    ungroup() # should return 0. It did

#cluster selection by day to maximize the amount of data selected - diff cluster throughout time
primary_cluster_tbl<-samples %>% 
    group_by(lagoslakeid, sample_date) %>% 
    slice_max(primary_samples_count, with_ties = TRUE) %>% #select the primary sites (cluster) with most data for TP, CHL, DOC 
    slice_max(total_samples_count, with_ties = TRUE) %>% #select the site (cluster) that has the most TP, CHL, DOC and all other params
    slice_sample(n=1) %>% #if still same amount of data in site(cluster) select one at random
    ungroup()

primary_cluster_tbl<-primary_cluster_tbl %>% select(-primary_samples_count, -total_samples_count) %>% 
    mutate(keep = 1)


US_epi<-US_epi %>% left_join(primary_cluster_tbl) %>% 
    filter(keep == 1) %>% 
    select(-keep)

# select best cluster sites to use (samplesiteid_lagos) - same cluster throughout time

# primary_cluster_tbl <- samples %>% 
#     group_by(lagoslakeid, cl_name) %>% 
#     summarise(n = sum(primary_samples_count), n2 = mean(total_samples_count)) %>% #sum samples through time
#     ungroup() %>% 
#     group_by(lagoslakeid) %>% 
#     #arrange(desc(n, n2)) %>% 
#     slice_max(n, with_ties = TRUE) %>%
#     slice_max(n2, with_ties = TRUE) %>% 
#     slice_sample(n=1) %>% 
#     select(-n, -n2) %>% distinct() %>% 
#     mutate(keep = 1)
# 
# US_epi<-US_epi %>% left_join(primary_cluster_tbl) %>% 
#     filter(keep == 1) %>% 
#     select(-keep)

gc()
#cluster selection done

#work on from here
US_epi<-rbind(US_epi, NLA_epi_clusters, NEON_epi_clusters)

temp<-US_epi %>% select(lagoslakeid, sample_date, parameter_name, source_sample_siteid) %>%  
    group_by(lagoslakeid, sample_date, parameter_name, source_sample_siteid) %>% 
    mutate(n=n()) %>% 
    ungroup() %>% 
    filter(n>1) #no repeat observations - what we wanted - on a given date on a sample site 

US_epi_backup<-US_epi #create a backup so don't need to run from the top

temp<-US_epi %>% group_by(lagoslakeid, sample_date, parameter_name) %>% 
    mutate(n=n()) %>% 
    ungroup() %>% 
    filter(n>1) #check again without source_sample_siteid - 3000-4000 obs depending on cluster method selected- open and check - might be NLA or NEON - years all 2007, 2012, 2017 - most likely NLA

US_epi<-US_epi %>% filter(!sample_id %in% temp$sample_id) 
temp<-temp %>% arrange(lagoslakeid, sample_date, parameter_name) #check to see if same pattern (wqx_nla, and state program sample)
temp<-temp %>% group_by(lagoslakeid, sample_date, parameter_name) %>% 
    mutate(n=sum(source_id == "nla_wqx")) #double check to see if each group has NLA and pulling them out- would come out as value of 0. This was not the case 
temp3<-temp %>% group_by(lagoslakeid, sample_date, parameter_name) %>% 
    filter(source_id == 'nla_wqx') #double check for repliecates
temp3<-temp3 %>% group_by(lagoslakeid, sample_date, parameter_name) %>% 
    slice_sample(n=1) %>% 
    select(-n)

US_epi<-rbind(US_epi, temp3)

temp<-US_epi %>% select(lagoslakeid, sample_date, parameter_name, source_id) %>% 
    group_by(lagoslakeid, sample_date, parameter_name) %>% 
    mutate(n=n()) %>% 
    filter(n>1) %>% 
    ungroup() #double check to see if we have any n>1 (replicate obs) - we shouldn't 


epi_export<-US_epi
gc()

#secchi work
    
temp<-epi_export %>% filter(parameter_name == "Secchi") %>% 
    select(lagoslakeid, sample_date) %>% distinct() #what lake and what date we have secchi data for - this is what we want
temp2<-US_final %>% filter(parameter_name == "Secchi") #got all secchi data available
temp3<-temp %>% left_join(temp2)    #identify all of the secchi data that comes from a lake and date. Lake could be sampled multiple times in a day 
temp2<-temp2 %>% filter(!sample_id %in% temp3$sample_id) #deletes the secchi data for those lake date combinations
temp2<- temp2 %>% group_by(lagoslakeid, sample_date) %>% 
    slice_sample(n=1) %>% ungroup() #what we're left with is lake date combos that weren't in our existing dataset
#temp2<-temp2 %>% left_join(US_epi %>% select(sample_id, eventidb_beta))  
epi_export<-rbind(epi_export, temp2) #bind data we have with data we're missing 

temp<-epi_export %>% select(lagoslakeid, sample_date, parameter_name, source_id) %>% 
    group_by(lagoslakeid, sample_date, parameter_name) %>% 
    mutate(n=n()) %>% 
    filter(n>1) %>% 
    ungroup() #double check to see if we have any n>1 - we shouldn't 

temp<-epi_export %>% select(source_id) %>% 
    group_by(source_id) %>% 
    mutate(n=n()) %>% 
    distinct() %>% 
    ungroup() #double check to see if the right amount of data is in NLA and NEON and others


# unique(temp2$lagos_epi_assignment)    
# temp3<-temp2 %>% filter(!lagoslakeid %in%epi_export$lagoslakeid)   
# temp3<-US_epi %>% filter(parameter_name == "Secchi")


gc()

lagos_variable<-read.csv("~/GitHub/lagos_database/lagos_variable.csv") %>% 
    select(variableid_lagos, variableshortname) %>% 
    rename(parameter_id = variableid_lagos)

str(epi_export)
unique(epi_export$parameter_id)
epi_export<-epi_export %>% mutate_at(c("parameter_id"), as.numeric)
str(epi_export)


epi_export<-epi_export %>% left_join(lagos_variable) %>% 
    select(-parameter_name)


#write_csv(epi_export, "epi_export.csv")
#saveRDS(epi_export, file = "epi_export.rds")
        
#QAQC_epi_export<-epi_export %>% sample_n(100000)
#write_csv(QAQC_epi_export, "QAQC_epi_export.csv")
#################################################################################################################################################

#Censorcodes - 
# temp<-epi_export %>% 
#     filter(!is.na(parameter_value)) %>% 
#     filter(is.na(as.numeric(parameter_value))) #checked for non numerics
# 
# epi_export<-epi_export %>% mutate(parameter_value = as.numeric(parameter_value))
# 
# 
# 
# LE5<-epi_export %>% filter(is.na(parameter_value) & !is.na(parameter_detectionlimit_value))
# 
# LE5<-LE5 %>% mutate(parameter_value = parameter_detectionlimit_value, 
#                     censorcode = "LE5")
# censorcode<-LE5
# 
# LE6<-epi_export %>% 
#     filter(!sample_id %in% censorcode$sample_id) %>% 
#     filter(parameter_value == 0 & is.na(parameter_detectionlimit_value) & is.na(source_detectionlimit_condition)) 
# LE6<-LE6 %>% mutate(censorcode = "LE6")
# censorcode<-rbind(censorcode, LE6)
# 
# NC1<-epi_export %>% 
#     filter(!sample_id %in% censorcode$sample_id) %>% 
#     filter(parameter_value >= parameter_detectionlimit_value & !is.na(source_detectionlimit_condition))
# NC1<-NC1 %>% mutate(censorcode = "NC1")
# censorcode<-rbind(censorcode, NC1)
# 
# NC2<-epi_export %>% 
#     filter(!sample_id %in% censorcode$sample_id) %>% 
#     filter(parameter_value >= parameter_detectionlimit_value & is.na(source_detectionlimit_condition))
# NC2<-NC2 %>% mutate(censorcode = "NC2")
# censorcode<-rbind(censorcode, NC2)
# 
# NC3<-epi_export %>% 
#     filter(!sample_id %in% censorcode$sample_id) %>% 
#     filter(is.na(parameter_detectionlimit_value) & !is.na(source_detectionlimit_condition))
# NC3<-NC3 %>% mutate(censorcode = "NC3")
# censorcode<-rbind(censorcode, NC3)
# 
# NC4<-epi_export %>% 
#     filter(!sample_id %in% censorcode$sample_id) %>% 
#     filter(is.na(parameter_detectionlimit_value) & is.na(source_detectionlimit_condition))
# NC4<-NC4 %>% mutate(censorcode = "NC4")
# censorcode<-rbind(censorcode, NC4)
# 
# 
# 
# 
# #have detectionlimit, datavalue < detectionlimit, with qualifier or comments
# 
# LE1<-epi_export %>% 
#     filter(!sample_id %in% censorcode$sample_id) %>% 
#     filter(parameter_detectionlimit_value > parameter_value & !is.na(source_detectionlimit_condition))
# LE1<-LE1 %>% mutate(censorcode = "LE1")
# censorcode<-rbind(censorcode, LE1)
# 
# #have detectionlimit, datavalue < detectionlimit, no qualifier or comments
# 
# LE2<-epi_export %>% 
#     filter(!sample_id %in% censorcode$sample_id) %>% 
#     filter(parameter_detectionlimit_value > parameter_value & is.na(source_detectionlimit_condition))
# LE2<-LE2 %>% mutate(censorcode = "LE2")
# censorcode<-rbind(censorcode, LE2)
# 
# gc()
# 
# temp<-epi_export %>% filter(!sample_id %in% censorcode$sample_id)
# 
# temp<-censorcode %>% 
#     filter(variableshortname == "secchi_m") %>% 
#     select(censorcode) %>% 
#     distinct()
# 
# censorcode<-censorcode %>% 
#     mutate(censorcode = if_else(variableshortname == "secchi_m" & censorcode == "LE6", "NC4", censorcode))
# 
# epi_export<-censorcode

#eventID 

eventidb_table <- epi_export %>%
    distinct(lagoslakeid, sample_date)

eventidb_table$eventidb_beta <- seq.int(nrow(eventidb_table))

epi_export<-epi_export %>% left_join(eventidb_table)


names(epi_export)
#rename vars
epi_export<-epi_export %>% rename(eventid_epi = eventidb_beta)
epi_export<-epi_export %>% rename(parameter_name = variableshortname)
# epi_export<-epi_export %>% rename(cluster_lat_dd = clus_lat)
# epi_export<-epi_export %>% rename(cluster_lon_dd = clus_lon)
# epi_export<-epi_export %>% rename(source_samplesite_lat_dd = Lat)
# epi_export<-epi_export %>% rename(source_samplesite_lon_dd = Lon)

#rename params

param_names_short<-read.csv("~/GitHub/lagos_database/LIMNO_file structure - parameter_description_limno.csv") %>% 
    select(parameter_id, parameter_name_short)

epi_export<-left_join(epi_export, param_names_short)
epi_export<-epi_export %>% select(-parameter_name)
epi_export<-epi_export %>% rename(parameter_name = parameter_name_short)

#change integrated to INTEGRATED in source_sample_type

# temp<-epi_export
# unique(temp$source_sample_type)
# temp<-temp %>%
#     mutate(source_sample_type = str_replace(source_sample_type, "integrated", "INTEGRATED"))

epi_export<-epi_export %>% 
    mutate(source_sample_type = str_replace(source_sample_type, "integrated", "INTEGRATED"))
unique(epi_export$source_sample_type)

gc()

write_csv(epi_export, "epi_export.csv")
saveRDS(epi_export, file = "epi_export.rds")

#################################################################################################################################################
#creating custom tables 

#EXPORT_LIMNO
#chemistry_limno

chemistry_limno<-epi_export %>% 
    select(
        eventid_epi,
        lagoslakeid,
        sample_date,
        sample_id,
        parameter_id,
        parameter_name,
        parameter_value,
        parameter_detectionlimit_value,
        censorcode,
        sample_depth_m,
        source_sample_type,
        cluster_id,
        cluster_lat_dd,
        cluster_lon_dd,
        source_id,
        source_sample_siteid
        
    )
    
write_csv(chemistry_limno, "chemistry_limno.csv")
saveRDS(chemistry_limno, file = "chemistry_limno.rds")


#sourcedata_limno

sourcedata_limno<-epi_export %>% 
    select(
        lagoslakeid,
        sample_id,
        parameter_id,
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
        source_samplesite_lat_dd,
        source_samplesite_lon_dd
    )

write_csv(sourcedata_limno, "sourcedata_limno.csv")
saveRDS(sourcedata_limno, file = "sourcedata_limno.rds")





#cluster_information_limno
# FILES NEEDE: epi_export, clustersites, LOCUS_for_LIMNO_clusterinfotable

clustersites<-read.csv("~/GitHub/lagos_database/siteclusters_7DEC22.csv")
epi_export<-epi_export %>% left_join(clustersites)
epi_export<-epi_export %>% left_join(LOCUS_for_LIMNO_clusterinfotable_14FEB23)

epi_export<-epi_export %>% rename(cluster_meandistance_m = cl_mndis)
epi_export<-epi_export %>% rename(cluster_maxdistance_m = max_dist)
epi_export<-epi_export %>% rename(cluster_sitedistance_m = site_cldis)


cluster_information_limno<-epi_export %>% 
    select(
        lagoslakeid,
        cluster_id,
        cluster_lat_dd,
        cluster_lon_dd,
        source_sample_siteid,
        source_samplesite_lat_dd,
        source_samplesite_lon_dd,
        lake_waterarea_ha,
        lake_states,
        lake_namelagos,
        cluster_meandistance_m,
        cluster_maxdistance_m,
        cluster_sitedistance_m
)

write_csv(cluster_information_limno, "cluster_information_limno.csv")
saveRDS(cluster_information_limno, file = "cluster_information_limno.rds")


#EXPORT:allsites_alldepths_limno

# #rename vars
# US_final<-US_final %>% rename(cluster_id = cl_name)
# US_final<-US_final %>% rename(cluster_lat_dd = clus_lat)
# US_final<-US_final %>% rename(cluster_lon_dd = clus_lon)
# US_final<-US_final %>% rename(source_samplesite_lat_dd = Lat)
# US_final<-US_final %>% rename(source_samplesite_lon_dd = Lon)

# #rename params
# 
# param_names_short<-read.csv("~/GitHub/lagos_database/LIMNO_file structure - parameter_description_limno.csv") %>% 
#     select(parameter_id, parameter_name_short)
# 
# US_final<-left_join(US_final, param_names_short)
# US_final<-US_final %>% select(-parameter_name)
# US_final<-US_final %>% rename(parameter_name = parameter_name_short)
# 
# #change integrated to INTEGRATED
# US_final<-US_final %>% 
#     mutate(source_sample_type = str_replace(source_sample_type, "integrated", "INTEGRATED"))
# unique(US_final$source_sample_type)
# 
# write_csv(US_final, "US_final.csv")
# saveRDS(US_final, file = "US_final.rds")

#fixed naming and param issues - now creating allsites_alldepths_limno

allsites_alldepths_limno<-US_final %>% 
    select(
        lagoslakeid,
        sample_date,
        sample_id,
        parameter_id,
        parameter_name,
        parameter_value,
        parameter_detectionlimit_value,
        censorcode,
        sample_depth_m,
        source_sample_type,
        cluster_id,
        cluster_lat_dd,
        cluster_lon_dd,
        source_id,
        source_sample_siteid,
        source_name,
        source_activityorg_name,
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
        source_samplesite_lat_dd,
        source_samplesite_lon_dd,
        lagos_epi_assignment
    )

#make parameterid numeric
str(allsites_alldepths_limno) 
allsites_alldepths_limno<-allsites_alldepths_limno %>% mutate_at(c("parameter_id"), as.numeric)
str(allsites_alldepths_limno) 


#change parameter names to short names

param_names_short<-read.csv("~/GitHub/lagos_database/LIMNO_file structure - parameter_description_limno.csv") %>% 
    select(parameter_id, parameter_name_short)

allsites_alldepths_limno<-left_join(allsites_alldepths_limno, param_names_short)
allsites_alldepths_limno<-allsites_alldepths_limno %>% select(-parameter_name)
allsites_alldepths_limno<-allsites_alldepths_limno %>% rename(parameter_name = parameter_name_short)

write_csv(allsites_alldepths_limno, "allsites_alldepths_limno.csv")
saveRDS(allsites_alldepths_limno, "allsites_alldepths_limno.rds")


#temp<-US_final %>% 
    #select(which(!(colnames(US_final) %in% colnames(allsites_alldepths_limno)))) #missing source_sample_type - deleted var


#clean epi export - neon data has seconds and minutes in the sample date column - june 12th 2023

temp<-epi_export %>% filter(source_id == "NEON")
epi_export<-epi_export %>% mutate(across(c(sample_date), ~str_split(., " ", simplify=T)[,1]))
temp<-epi_export %>% filter(source_id == "NEON")


write_csv(epi_export, "epi_export.csv")
saveRDS(epi_export, file = "epi_export.rds")

temp<-allsites_alldepths_limno %>% filter(source_id == "NEON")
allsites_alldepths_limno<-allsites_alldepths_limno %>% mutate(across(c(sample_date), ~str_split(., " ", simplify=T)[,1]))
temp<-allsites_alldepths_limno %>% filter(source_id == "NEON")

write_csv(allsites_alldepths_limno, "allsites_alldepths_limno.csv")
saveRDS(allsites_alldepths_limno, "allsites_alldepths_limno.rds")

temp<-chemistry_limno %>% filter(source_id == "NEON")
chemistry_limno<-chemistry_limno %>% mutate(across(c(sample_date), ~str_split(., " ", simplify=T)[,1]))
temp<-chemistry_limno %>% filter(source_id == "NEON")

write_csv(chemistry_limno, "chemistry_limno.csv")
saveRDS(chemistry_limno, file = "chemistry_limno.rds")

#fixed

#################################################################################################################################################
#preliminary viz
#2023/04/12
#boxplots for all vars from all data (6.5 mil) for LE5's

temp<-allsites_alldepths_limno %>% 
    filter(censorcode == "LE5") %>% 
    group_by(parameter_name) %>% 
    mutate(upper = boxplot.stats(temp$parameter_detectionlimit_value, coef = 4)$stats[5]) %>% 
    ungroup() %>% 
    filter(parameter_detectionlimit_value <= upper)

    

unique(temp$parameter_name)


p2 <- ggplot(temp, aes(x=parameter_name, y= log10(parameter_detectionlimit_value +1))) +
    geom_boxplot() +
    facet_wrap(~parameter_name, scale="free")
p2

temp2<-temp %>% filter(parameter_name == "al_diss_ugl") %>% 
    mutate(upper = boxplot.stats(temp$parameter_detectionlimit_value, coef = 4)$stats[5]) %>% 
    filter(parameter_detectionlimit_value <= upper)

stats<-boxplot.stats(temp$parameter_detectionlimit_value, coef = 4)
    
unique(temp$parameter_name)
p3<- temp %>% filter(parameter_name == "spcond_uscm") 

# %>% 
#     ggplot(temp, aes(x=parameter_name, y=parameter_detectionlimit_value)) +
#     geom_boxplot()

p4 <- ggplot(p3, aes(x=parameter_name, y=parameter_detectionlimit_value)) + 
    geom_boxplot() + coord_cartesian(ylim = c(3,10))

p4




p5<-temp %>% filter(parameter_name == "spcond_uscm")
summary(p5)
p6 <- ggplot(p5, aes(x=parameter_name, y=parameter_detectionlimit_value)) + 
    geom_boxplot() + coord_cartesian(ylim = c(10,1000))
p6

str(temp)
##########################################################################################################################


names(allsites_alldepths_limno)


readRDS("epi_export.RDS")

library(sf)
install.packages("mapview")
library(mapview)
mapdata<- epi_export %>% select(lagoslakeid, Lat, Lon, clus_lat, clus_lon)
unique_mapdata<-mapdata %>% distinct(lagoslakeid, .keep_all = TRUE)

mapview(unique_mapdata, xcol = "Lon", ycol = "Lat", crs = 4326, grid = FALSE) #didn't work cuz NA's

temp<-unique_mapdata %>% summarise_all(~ sum(is.na(.))) #check for NA's
temp<-unique_mapdata %>% na.omit(unique_mapdata) #remove na's

unique_mapdata<-unique_mapdata %>% na.omit(unique_mapdata) #remove na's
mapview(unique_mapdata, xcol = "Lon", ycol = "Lat", crs = 4326, grid = FALSE) 





#############################################################################################################################################################
#install.packages("rgeos")
library(rgeos)
install.packages("maptools")
library(maptools)
library(sf)
install.packages("scales")
library(scales)
library(grid)
#install.packages("ggpubr")
library(ggpubr)
#install.packages("LAGOSUS")
#library(LAGOSUS)
library(viridis)
library(tidyverse)

install.packages("rlang")
library(rlang)
install.packages("tidyverse")
library(tidyverse)

install.packages("pillar")
library(pillar)

lake_info<-read.csv("~/GitHub/lagos_database/lake_information (1).csv") %>% 
    select(lagoslakeid, lake_lat_decdeg, lake_lon_decdeg, state_zoneid)


#statezoneids for lakes



state_id <- lake_info %>% 
    select(lagoslakeid, state_zoneid) %>% 
    distinct()
summary(state_id)

file_gdb <- file.path("C:/Users/Arnab/Documents/gis_geo_v1.0.gdb")

state <- st_read(file_gdb, 'simple_state') %>% 
    st_zm(drop = TRUE) %>% 
    st_cast('POLYGON') %>%
    as("Spatial") %>% 
    fortify(region = 'state_zoneid') 


# 1. Calculate the total number of LAGOS lakes in each state.
#overall pct and n in each state
state_n <- lake_info %>% 
    group_by(state_zoneid) %>% 
    summarise(n_state = n()) %>% 
    ungroup()



# 2. Calculate the pct of lakes with values for each param by state
# calculate percent of lakes by cluster in each neon zone


epi_export1<-left_join(epi_export, lake_info)
epi_export1<-left_join(epi_export1, state_n)

cl_pct <- epi_export1 %>%   #neon_mdat would be your chemistry data file -- you want summaries by wq param
    group_by(lagoslakeid, state_zoneid, parameter_id) %>%   # cl16 replaced by parameter_id?
    summarise(n_state_cl=n()) 

#%>% 
    #mutate(pct_state_cl = n_state_cl * 100/n_state)




(p_neon_clusters_all <- neon %>%   # This is the shapefile for neon == replace with state
        mutate(neon_zoneid = id) %>% 
        right_join(cl_pct, by="neon_zoneid") %>%  # This would be the data with pct of lakes in a state with data
        ggplot()+
        geom_polygon(mapping=aes(x=long, y=lat, group=group, fill=pct_neon_cl),
                     color="black", size=0.6) + 
        labs(x= "", y="")+  
        facet_wrap(vars(cl16), ncol=3) +
        scale_fill_viridis("TITLE", option ='plasma', direction = -1, limits=c(0, 80), breaks = c(0, 20, 40, 60, 80)) + 
        ggtitle("TITLE") +
        coord_equal() +
        theme_bw() +
        thmap2 +
        theme (
            #legend.key.height= unit(1, 'cm'),
            #legend.key.width= unit(1, 'cm'),
            #legend.text = element_text(size=16),
            legend.title = element_text(size=18)
        )
)






#read in lakeinfo to get state data
#state zoneids for lakes
state_id<-read.csv("~/GitHub/lagos_database/lake_information (1).csv") %>% 
    select(lagoslakeid, state_zoneid) %>% 
    distinct()
summary(state_id)


unique_mapdata<-unique_mapdata %>% left_join(lake_info)

#Calculate the total number of LAGOS lakes in each state.
#overall pct and n in each neon
states_n <- unique_mapdata %>% 
    group_by(state_zoneid) %>% 
    mutate(n_states = n()) %>% 
    ungroup()

# Calculate the pct of lakes with values for each param by state

cl_pct <- epi_export %>%   #neon_mdat would be your chemistry data file -- you want summaries by wq param
    group_by(lagoslakeid, parameter_id) %>%   # cl16 replaced by parameter_id?
    summarise(n_params=n()) %>% 
    left_join(states_n) %>% 
    mutate(pct_states_cl = n_params * 100/n_states)


# temp<-epi_export %>% select(source_id) %>% 
#     filter(str_detect(source_id,"nla"))
# 
# unique(temp$source_id)

names(epi_export_daily)
names(US_final)
unique(epi_export_daily$source_parameter_name)
unique(US_final$parameter_name)
