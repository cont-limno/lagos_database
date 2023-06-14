depths<-US_final %>% select(source_activityid, sample_depth_m) %>% 
  distinct() %>% rename(ActivityIdentifier = source_activityid)

atr_depth<-atrazine_data %>% left_join(depths) %>% 
  drop_na(sample_depth_m) %>% 
  group_by(ActivityIdentifier) %>% 
  mutate(n=length(unique(sample_depth_m))) %>% 
  ungroup() %>% 
  filter(n==1)

temp<-atr_depth %>% 
  group_by(ActivityStartDate, MonitoringLocationIdentifier, sample_depth_m, CharacteristicName, OrganizationIdentifier) %>% 
  slice_sample(n=1) %>% ungroup() #randomly selecting value to keep. EG. might have 4 duplicates - will keep 1 at random - removes duplicates



clustersites<-read.csv("~/GitHub/lagos_database/siteclusters_7DEC22.csv") %>% 
  select(lagoslakeid, source_sample_siteid, cl_name, clus_lat, clus_lon)

names(temp)
names(US_final)

temp <- temp %>% 
  rename(sample_id = Obs_Id,
         parameter_name = CharacteristicName,
         parameter_value = ResultMeasureValue,
         parameter_detectionlimit_value = DetectionQuantitationLimitMeasure.MeasureValue,
         source_id = OrganizationIdentifier,
         source_name = OrganizationFormalName,
         source_activityid = ActivityIdentifier,
         source_activityorg_name = ActivityConductingOrganizationText,
         source_sample_siteid = MonitoringLocationIdentifier,
         source_parameter_name = source_parameter,
         source_parameter_value = source_value,
         source_parameter_units = source_unit,
         parameter_conversionfactor = Conversion,
         source_detectionlimit_value = detectionlimit_legacy,
         source_detectionlimit_unit = detectionlimit_unit_legacy,
         parameter_detectionlimit_conversionfactor = Conversion_dl,
         source_detectionlimit_condition = ResultDetectionConditionText,
         source_detectionlimit_type = DetectionQuantitationLimitTypeName,
         source_value_qualifiercode = MeasureQualifierCode,
         source_labmethod_description = MethodDescriptionText,
         source_labmethod_id = Method_Id,
         source_labmethod_name = ResultAnalyticalMethod.MethodName,
         wqp_parameter_usgspcode = USGSPCode)


temp<-temp %>% left_join(clustersites) 


#censorcodes of US_FINAL

#Censorcodes - 
str(temp)
temp1<-temp %>% 
  filter(!is.na(parameter_value)) %>% 
  filter(is.na(as.numeric(parameter_value))) #checked for non numerics

temp<-temp %>% mutate(parameter_value = as.numeric(parameter_value))


str(LE5)
LE5<-temp %>% filter(is.na(parameter_value) & !is.na(parameter_detectionlimit_value))

LE5<-LE5 %>% mutate(parameter_value = parameter_detectionlimit_value, censorcode = "LE5") %>%
group_by(parameter_name) %>%
mutate(upper = boxplot.stats(temp$parameter_detectionlimit_value, coef = 4)$stats[5]) %>%
ungroup() 
# %>%
# filter(parameter_detectionlimit_value <= upper)
LE5.discard<-LE5 %>% filter(parameter_detectionlimit_value > upper)
temp<-temp %>% filter(!sample_id %in% LE5.discard$sample_id)
LE5<-LE5 %>% filter(parameter_detectionlimit_value <= upper)
LE5<-LE5 %>% select(-upper)
censorcode<-LE5

LE6<-temp %>% 
  filter(!sample_id %in% censorcode$sample_id) %>% 
  filter(parameter_value == 0 & is.na(parameter_detectionlimit_value) & is.na(source_detectionlimit_condition)) 
LE6<-LE6 %>% mutate(censorcode = "LE6")
censorcode<-rbind(censorcode, LE6)

NC1<-temp %>% 
  filter(!sample_id %in% censorcode$sample_id) %>% 
  filter(parameter_value > parameter_detectionlimit_value & !is.na(source_detectionlimit_condition))
NC1<-NC1 %>% mutate(censorcode = "NC1")
censorcode<-rbind(censorcode, NC1)

NC2<-temp %>% 
  filter(!sample_id %in% censorcode$sample_id) %>% 
  filter(parameter_value > parameter_detectionlimit_value & is.na(source_detectionlimit_condition))
NC2<-NC2 %>% mutate(censorcode = "NC2")
censorcode<-rbind(censorcode, NC2)

NC3<-temp %>% 
  filter(!sample_id %in% censorcode$sample_id) %>% 
  filter(is.na(parameter_detectionlimit_value) & !is.na(source_detectionlimit_condition))
NC3<-NC3 %>% mutate(censorcode = "NC3")
censorcode<-rbind(censorcode, NC3)

NC4<-temp %>% 
  filter(!sample_id %in% censorcode$sample_id) %>% 
  filter(is.na(parameter_detectionlimit_value) & is.na(source_detectionlimit_condition))
NC4<-NC4 %>% mutate(censorcode = "NC4")
censorcode<-rbind(censorcode, NC4)




#have detectionlimit, datavalue < detectionlimit, with qualifier or comments

LE1<-temp %>% 
  filter(!sample_id %in% censorcode$sample_id) %>% 
  filter(parameter_detectionlimit_value >= parameter_value & !is.na(source_detectionlimit_condition))
LE1<-LE1 %>% mutate(censorcode = "LE1")
censorcode<-rbind(censorcode, LE1)

#have detectionlimit, datavalue < detectionlimit, no qualifier or comments

LE2<-temp %>% 
  filter(!sample_id %in% censorcode$sample_id) %>% 
  filter(parameter_detectionlimit_value >= parameter_value & is.na(source_detectionlimit_condition))
LE2<-LE2 %>% mutate(censorcode = "LE2")
censorcode<-rbind(censorcode, LE2)

gc()

temp2<-temp %>% filter(!sample_id %in% censorcode$sample_id)

temp3<-censorcode %>% 
  filter(parameter_name == "secchi_m") %>% 
  select(censorcode) %>% 
  distinct()

censorcode<-censorcode %>% 
  mutate(censorcode = if_else(parameter_name == "secchi_m" & censorcode == "LE6", "NC4", censorcode))

temp<-censorcode

#censorcoded added


#add epi assingment
temp<-temp %>% mutate(lagos_epi_assingment = 1) #can do this because already selected out epi samples

#add gps coords
wqx_coords<-read.csv("~/GitHub/lagos_database/station.csv")

wqx_coords<- wqx_coords %>% select(MonitoringLocationIdentifier, LatitudeMeasure, LongitudeMeasure) %>% 
  rename(source_sample_siteid = MonitoringLocationIdentifier)

temp1<-wqx_coords %>%  select(source_sample_siteid, LatitudeMeasure, LongitudeMeasure) %>% distinct() %>% group_by(source_sample_siteid) %>% 
  mutate(LatitudeMeasure = mean(LatitudeMeasure), LongitudeMeasure = mean(LongitudeMeasure)) %>% ungroup() %>% distinct()


temp2<-temp %>% left_join(temp1) %>% select(lagoslakeid, source_sample_siteid, LatitudeMeasure, LongitudeMeasure)

#works so join with atz dataset

temp<-temp %>% left_join(temp1 )

#fix parameter name column

#param_names_short<-read.csv("~/GitHub/lagos_database/LIMNO_file structure - parameter_description_limno.csv") %>% 
  #select(parameter_id, parameter_name_short) #used this for a check - not needed

unique(temp$parameter_name)

temp<-temp %>% mutate(parameter_id = if_else(ResultSampleFractionText == "Total", "38", "39"))
temp<-temp %>% mutate(parameter_name_short = if_else(ResultSampleFractionText == "Total", "atz_tot_ugl", "atz_diss_ugl"))

#fix spelling for consistency
atz_sourcesampletype<-allsites_alldepths_limno %>% select(source_activityid, source_sample_type) %>% distinct()
temp<-temp %>% left_join(atz_sourcesampletype)

temp<-temp %>% 
  mutate(source_sample_type = str_replace(source_sample_type, "integrated", "INTEGRATED"))
unique(epi_export$source_sample_type)

#adding cluster info 
clustersites<-read.csv("~/GitHub/lagos_database/siteclusters_7DEC22.csv")
temp<-temp %>% left_join(clustersites)
temp<-temp %>% left_join(LOCUS_for_LIMNO_clusterinfotable_14FEB23)
 
temp<-temp %>% rename(cluster_meandistance_m = cl_mndis)
temp<-temp %>% rename(cluster_maxdistance_m = max_dist)
temp<-temp %>% rename(cluster_sitedistance_m = site_cldis)


#names(US_final)
#rename vars


temp<-temp %>% rename(source_samplesite_lat_dd = Lat)
temp<-temp %>% rename(source_samplesite_lon_dd = Lon)
temp<-temp %>% rename(cluster_id = cl_name)
temp<-temp %>% rename(cluster_lat_dd = clus_lat)
temp<-temp %>% rename(cluster_lon_dd = clus_lon)

temp<-temp %>% rename(sample_date = ActivityStartDate)
temp<-temp %>% rename(source_comments = ActivityCommentText)
temp<-temp %>% rename(source_parameter_unit = source_parameter_units)
temp<-temp %>% rename(source_labmethod_usgspcode = wqp_parameter_usgspcode)
temp<-temp %>% rename(source_labmethod_qualifier = source_value_qualifiercode)
temp<-temp %>% mutate(source_value_qualifiercode = "NA")
temp<-temp %>% rename(lagos_epi_assignment = lagos_epi_assingment)


#attempt to create table

temp2<-US_final %>% select(source_sample_type, source_activityid) %>%
  distinct() %>% 
  right_join(temp)


temp5<-temp2 %>% 
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
    cluster_id,
    cluster_lat_dd,
    cluster_lon_dd,
    source_id,
    source_sample_type,
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


temp5 <- temp5 %>% filter(source_activityid != "21IOWA-SAMP01-TMDL-2002-0")

write_csv(temp5, "atrz_all_data.csv") #artrazine data ready to go - need to rbind with US_final

#US_final<-US_final %>% select(-sample_depth_flag) #discussed with noah - dropped - same info in source_sample_type

US_final<-US_final %>% filter(parameter_name != "Atrazine, total") %>% 
  filter(parameter_name != "Atrazine, dissolved")
unique(US_final$parameter_name)


atz_check<-US_final %>% filter(sample_id %in% temp5$sample_id)
temp5<-temp5 %>% filter(!sample_id %in% atz_check$sample_id)

unique(temp5$parameter_name)

temp6<-temp5 
temp5$sample_date = as.character(temp5$sample_date)  
str(temp5)  

US_final<-rbind(US_final, temp5)
gc()
atz_check<-US_final %>% filter(parameter_name == "Atrazine, total" | parameter_name == "Atrazine, dissolved")
US_final<-US_final %>% mutate(parameter_name = if_else(parameter_name == "Atrazine, total", "atz_tot_ugl", parameter_name))
US_final<-US_final %>% mutate(parameter_name = if_else(parameter_name == "Atrazine, dissolved", "atz_diss_ugl", parameter_name))
unique(US_final$parameter_name)

saveRDS(US_final, "US_final_2023_05_19.rds")
