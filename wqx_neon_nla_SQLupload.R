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

