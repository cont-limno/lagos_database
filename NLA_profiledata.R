library(dplyr)
library(data.table)

#2007 data

dat07_profile<-read.csv("./NLA/nla2007_profile_20091008.csv") %>%  
    select(-YEAR, -METALIMNION, -VISIT_NO, - SAMPLED_PROFILE) %>% 
    mutate(profile = "integrated") %>% 
    rename(source_sample_siteid = SITE_ID, 
           sample_date = DATE_PROFILE, 
           sample_depth_m = DEPTH,
           temp_degc = TEMP_FIELD,
           do_mgl = DO_FIELD,
           ph = PH_FIELD,
           spcond_uscm = COND_FIELD,
           source_value_qualifiercode = FLAG_PROFILE,
           source_comments = COMMENT_PROFILE)




#########################################################################################
#2012 data


dat12_siteinfo<-read.csv("./NLA/nla2012_wide_siteinfo_08232016.csv")

dat12_profile<-read.csv("./NLA/nla2012_wide_profile_08232016.csv") %>% filter(SAMPLE_TYPE != "CALIB") %>% 
    select(SITE_ID, DATE_COL, DEPTH, TEMPERATURE, OXYGEN, PH, CONDUCTIVITY, SAMPLE_TYPE) %>% mutate(flag = NA, comment = NA) %>% 
    rename(source_sample_siteid = SITE_ID, 
           sample_date = DATE_COL, 
           sample_depth_m = DEPTH,
           temp_degc = TEMPERATURE,
           do_mgl = OXYGEN,
           ph = PH,
           spcond_uscm = CONDUCTIVITY,
           source_value_qualifiercode = flag,
           source_comments = comment, 
           profile = SAMPLE_TYPE)

#reorder columns
dat12_profile<-dat12_profile %>% 
    select(source_sample_siteid,
           sample_date,
           sample_depth_m,
           temp_degc,
           do_mgl,
           ph,
           spcond_uscm,
           source_value_qualifiercode,
           source_comments,
           profile)

#set value in profile to integrated
dat12_profile$profile<-"integrated"

#####################################
#2017 data

dat17_siteinfo<-read.csv("./NLA/nla_2017_site_information-data.csv")

dat17_profile<-read.csv("./NLA/nla_2017_profile-data.csv") %>% 
    select(SITE_ID, DATE_COL, DEPTH, TEMPERATURE, OXYGEN, PH, CONDUCTIVITY, SAMPLE_TYPE) %>% mutate(flag = NA, comment = NA) %>% 
    rename(source_sample_siteid = SITE_ID, 
           sample_date = DATE_COL, 
           sample_depth_m = DEPTH,
           temp_degc = TEMPERATURE,
           do_mgl = OXYGEN,
           ph = PH,
           spcond_uscm = CONDUCTIVITY,
           source_value_qualifiercode = flag,
           source_comments = comment,
           profile = SAMPLE_TYPE)


#reorder columns
dat17_profile<-dat17_profile %>% 
    select(source_sample_siteid,
           sample_date,
           sample_depth_m,
           temp_degc,
           do_mgl,
           ph,
           spcond_uscm,
           source_value_qualifiercode,
           source_comments,
           profile)

#set value in profile to integrated
dat17_profile$profile<-"integrated"


NLA_link <- read_csv("NLA/NLA_LakeLink_FINAL_26SE22.csv") %>% distinct(SITE_ID,lagoslakeid) %>% drop_na(lagoslakeid) %>% 
    rename(source_sample_siteid = SITE_ID)
dat<-rbind(dat07_profile, dat12_profile, dat17_profile) %>% left_join(NLA_link) %>% drop_na(lagoslakeid) %>%
    filter(!if_all(c(temp_degc, do_mgl, ph, spcond_uscm), is.na))

dat<-dat %>% select(lagoslakeid, everything())

write_csv(dat, "NLA_profile.csv")




