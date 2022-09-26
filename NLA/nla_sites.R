library(tidyverse)
nla_2017_site <- read_csv("NLA/nla_2017_site_information-data.csv") %>% 
    select(UID,SITE_ID,VISIT_NO)
nla_2017 <- read_csv("2017gps.csv") %>% 
    left_join(nla_2017_site)
nla_2007 <- read_csv("NLA/nla2007_sampledlakeinformation_20091113.csv") %>% 
    select(SITE_ID,VISIT_NO,FLD_LAT_DD,FLD_LON_DD,FLD_SRC) %>% 
    filter(FLD_SRC=="Index_site") %>% 
    drop_na() %>% 
    select(-FLD_SRC) %>% 
    distinct()  %>% 
    mutate(UID=NA) %>% 
    rename(INDEX_LAT_DD = FLD_LAT_DD,
           INDEX_LON_DD = FLD_LON_DD)
nla_2012_site <- read_csv("NLA/nla2012_wide_siteinfo_08232016.csv") %>% 
    select(UID,SITE_ID,VISIT_NO)
nla_2012 <- read_csv("NLA/nla2012_wide_profile_08232016.csv") %>% 
    select(UID,INDEX_LAT_DD,INDEX_LON_DD) %>% 
    drop_na() %>% 
    distinct() %>% 
    left_join(nla_2012_site) %>% 
    drop_na()

nla_sites <- rbind(nla_2007,nla_2012,nla_2017)
write_csv(nla_sites,"nla_sites.csv")
