library(dplyr)
library(data.table)

#2007 data

dat07_siteinfo<-read.csv("./NLA/nla2007_sampledlakeinformation_20091113.csv")
dat07_profile<-read.csv("./NLA/nla2007_profile_20091008.csv")
dat07_secchi<-read.csv("./NLA/nla2007_secchi_20091008.csv")
dat07_chem<-read.csv("./NLA/nla2007_chemical_conditionestimates_20091123 (1).csv")

dat07_site<-
    dat07_siteinfo %>%
    select(c('SITE_ID',"VISIT_NO",'DATE_COL','LAT_DD','LON_DD','ST',
             'CNTYNAME','NHDNAME','LAKENAME'))

dat07_temp <-
    dat07_profile %>%
    select(c('SITE_ID',"VISIT_NO",'DEPTH','TEMP_FIELD',))
names(dat07_temp)[names(dat07_temp) == "TEMP_FIELD"] <- "SAMPLE_RESULT"
dat07_temp$SAMPLE_NAME<-c('TEMP_FIELD')
dat07_temp$SAMPLE_TYPE<-c('DEPTH')
dat07_temp$SAMPLE_UNITS<-c('C')
dat07_temp$SAMPLE_FLAG<-c('')
dat07_temp$SAMPLE_QA_FLAG<-c('')
dat07_temp$SAMPLE_LAB_FLAG<-c('')
dat07_temp$SAMPLE_MDL<-c('')
dat07_temp$UID<-c('')


dat07_DO <-
    dat07_profile %>%
    select(c('SITE_ID',"VISIT_NO",'DEPTH','DO_FIELD',))
names(dat07_DO)[names(dat07_DO) == "DO_FIELD"] <- "SAMPLE_RESULT"
dat07_DO$SAMPLE_NAME<-c('DO_FIELD')
dat07_DO$SAMPLE_TYPE<-c('DEPTH')
dat07_DO$SAMPLE_UNITS<-c('mgL')
dat07_DO$SAMPLE_FLAG<-c('')
dat07_DO$SAMPLE_QA_FLAG<-c('')
dat07_DO$SAMPLE_LAB_FLAG<-c('')
dat07_DO$SAMPLE_MDL<-c('')
dat07_DO$UID<-c('')

dat07_PH <-
    dat07_profile %>%
    select(c('SITE_ID',"VISIT_NO",'DEPTH','PH_FIELD',))
names(dat07_PH)[names(dat07_PH) == "PH_FIELD"] <- "SAMPLE_RESULT"
dat07_PH$SAMPLE_NAME<-c('PH_FIELD')
dat07_PH$SAMPLE_TYPE<-c('DEPTH')
dat07_PH$SAMPLE_UNITS<-c('')
dat07_PH$SAMPLE_FLAG<-c('')
dat07_PH$SAMPLE_QA_FLAG<-c('')
dat07_PH$SAMPLE_LAB_FLAG<-c('')
dat07_PH$SAMPLE_MDL<-c('')
dat07_PH$UID<-c('')

dat07_COND <-
    dat07_profile %>%
    select(c('SITE_ID',"VISIT_NO",'DEPTH','COND_FIELD',))
names(dat07_COND)[names(dat07_COND) == "COND_FIELD"] <- "SAMPLE_RESULT"
dat07_COND$SAMPLE_NAME<-c('COND_FIELD')
dat07_COND$SAMPLE_TYPE<-c('DEPTH')
dat07_COND$SAMPLE_UNITS<-c('uS/cm')
dat07_COND$SAMPLE_FLAG<-c('')
dat07_COND$SAMPLE_QA_FLAG<-c('')
dat07_COND$SAMPLE_LAB_FLAG<-c('')
dat07_COND$SAMPLE_MDL<-c('')
dat07_COND$UID<-c('')

dat07_profile<-rbind(dat07_COND,dat07_DO,dat07_PH,dat07_temp)

Profile_data07 <- dat07_profile[, c('UID','SAMPLE_UNITS','SAMPLE_RESULT','SAMPLE_FLAG','SAMPLE_QA_FLAG','SAMPLE_LAB_FLAG',
                                 'SAMPLE_TYPE','SAMPLE_MDL','SAMPLE_NAME','DEPTH','SITE_ID','VISIT_NO')]


names1<-c('UID','SAMPLE_UNITS','SAMPLE_RESULTS','SAMPLE_FLAG1','SAMPLE_FLAG2','SAMPLE_FLAG3',
          'SAMPLE_TYPE','SAMPLE_MDL','SAMPLE_NAME','DEPTH','SITE_ID','VISIT_NO')

setnames(Profile_data07, names1)


###########
#chem data

dat07_chem1 <-
    dat07_chem %>%
    select(c('SITE_ID',"VISIT_NO","PTL"))
names(dat07_chem1)[names(dat07_chem1) == "PTL"] <- "SAMPLE_RESULT"
dat07_chem1$SAMPLE_NAME<-c('PTL')
dat07_chem1$SAMPLE_TYPE<-c('INTEGRATED')
dat07_chem1$SAMPLE_UNITS<-c('ug/L')
dat07_chem1$SAMPLE_FLAG<-c('')
dat07_chem1$SAMPLE_QA_FLAG<-c('')
dat07_chem1$SAMPLE_LAB_FLAG<-c('')
dat07_chem1$SAMPLE_MDL<-c('')
dat07_chem1$UID<-c('')

dat07_chem2 <-
    dat07_chem %>%
    select(c('SITE_ID',"VISIT_NO","NTL"))
names(dat07_chem2)[names(dat07_chem2) == "NTL"] <- "SAMPLE_RESULT"
dat07_chem2$SAMPLE_NAME<-c('NTL')
dat07_chem2$SAMPLE_TYPE<-c('INTEGRATED')
dat07_chem2$SAMPLE_UNITS<-c('ug/L')
dat07_chem2$SAMPLE_FLAG<-c('')
dat07_chem2$SAMPLE_QA_FLAG<-c('')
dat07_chem2$SAMPLE_LAB_FLAG<-c('')
dat07_chem2$SAMPLE_MDL<-c('')
dat07_chem2$UID<-c('')

dat07_chem3 <-
    dat07_chem %>%
    select(c('SITE_ID',"VISIT_NO","TURB"))
names(dat07_chem3)[names(dat07_chem3) == "TURB"] <- "SAMPLE_RESULT"
dat07_chem3$SAMPLE_NAME<-c('TURB')
dat07_chem3$SAMPLE_TYPE<-c('INTEGRATED')
dat07_chem3$SAMPLE_UNITS<-c('NTU')
dat07_chem3$SAMPLE_FLAG<-c('')
dat07_chem3$SAMPLE_QA_FLAG<-c('')
dat07_chem3$SAMPLE_LAB_FLAG<-c('')
dat07_chem3$SAMPLE_MDL<-c('')
dat07_chem3$UID<-c('')

dat07_chem4 <-
    dat07_chem %>%
    select(c('SITE_ID',"VISIT_NO","ANC"))
names(dat07_chem4)[names(dat07_chem4) == "ANC"] <- "SAMPLE_RESULT"
dat07_chem4$SAMPLE_NAME<-c('ANC')
dat07_chem4$SAMPLE_TYPE<-c('INTEGRATED')
dat07_chem4$SAMPLE_UNITS<-c('ueq/L')
dat07_chem4$SAMPLE_FLAG<-c('')
dat07_chem4$SAMPLE_QA_FLAG<-c('')
dat07_chem4$SAMPLE_LAB_FLAG<-c('')
dat07_chem4$SAMPLE_MDL<-c('')
dat07_chem4$UID<-c('')

dat07_chem5 <-
    dat07_chem %>%
    select(c('SITE_ID',"VISIT_NO","DOC"))
names(dat07_chem5)[names(dat07_chem5) == "DOC"] <- "SAMPLE_RESULT"
dat07_chem5$SAMPLE_NAME<-c('DOC')
dat07_chem5$SAMPLE_TYPE<-c('INTEGRATED')
dat07_chem5$SAMPLE_UNITS<-c('mg/L')
dat07_chem5$SAMPLE_FLAG<-c('')
dat07_chem5$SAMPLE_QA_FLAG<-c('')
dat07_chem5$SAMPLE_LAB_FLAG<-c('')
dat07_chem5$SAMPLE_MDL<-c('')
dat07_chem5$UID<-c('')

dat07_chem6 <-
    dat07_chem %>%
    select(c('SITE_ID',"VISIT_NO","COND"))
names(dat07_chem6)[names(dat07_chem6) == "COND"] <- "SAMPLE_RESULT"
dat07_chem6$SAMPLE_NAME<-c('COND')
dat07_chem6$SAMPLE_TYPE<-c('INTEGRATED')
dat07_chem6$SAMPLE_UNITS<-c('uS/cm @ 25C')
dat07_chem6$SAMPLE_FLAG<-c('')
dat07_chem6$SAMPLE_QA_FLAG<-c('')
dat07_chem6$SAMPLE_LAB_FLAG<-c('')
dat07_chem6$SAMPLE_MDL<-c('')
dat07_chem6$UID<-c('')

dat07_chem7 <-
    dat07_chem %>%
    select(c('SITE_ID',"VISIT_NO","CHLA"))
names(dat07_chem7)[names(dat07_chem7) == "CHLA"] <- "SAMPLE_RESULT"
dat07_chem7$SAMPLE_NAME<-c('CHLA')
dat07_chem7$SAMPLE_TYPE<-c('INTEGRATED')
dat07_chem7$SAMPLE_UNITS<-c('ug/L')
dat07_chem7$SAMPLE_FLAG<-c('')
dat07_chem7$SAMPLE_QA_FLAG<-c('')
dat07_chem7$SAMPLE_LAB_FLAG<-c('')
dat07_chem7$SAMPLE_MDL<-c('')
dat07_chem7$UID<-c('')

dat07_chem8 <-
    dat07_secchi %>%
    select(c('SITE_ID',"VISIT_NO",'SECMEAN'))
names(dat07_chem8)[names(dat07_chem8) == "SECMEAN"] <- "SAMPLE_RESULT"
dat07_chem8$SAMPLE_NAME<-c('SECMEAN')
dat07_chem8$SAMPLE_TYPE<-c('GRAB')
dat07_chem8$SAMPLE_UNITS<-c('ug/L')
dat07_chem8$SAMPLE_FLAG<-c('')
dat07_chem8$SAMPLE_QA_FLAG<-c('')
dat07_chem8$SAMPLE_LAB_FLAG<-c('')
dat07_chem8$SAMPLE_MDL<-c('')
dat07_chem8$UID<-c('')
dat07_chem8$DEPTH<-c(0)

Discrete_data07<-rbind(dat07_chem1,
                       dat07_chem2,
                       dat07_chem3,
                       dat07_chem4,
                       dat07_chem5,
                       dat07_chem6,
                       dat07_chem7)
Discrete_data07$DEPTH<-c("")
Discrete_data07<-rbind(Discrete_data07$DEPTH,dat07_chem8)

Discrete_data07 <- Discrete_data07[, c('UID','SAMPLE_UNITS','SAMPLE_RESULT','SAMPLE_FLAG','SAMPLE_QA_FLAG','SAMPLE_LAB_FLAG',
                                    'SAMPLE_TYPE','SAMPLE_MDL','SAMPLE_NAME','DEPTH','SITE_ID','VISIT_NO')]


names1<-c('UID','SAMPLE_UNITS','SAMPLE_RESULTS','SAMPLE_FLAG1','SAMPLE_FLAG2','SAMPLE_FLAG3',
          'SAMPLE_TYPE','SAMPLE_MDL','SAMPLE_NAME','DEPTH','SITE_ID','VISIT_NO')

setnames(Discrete_data07, names1)

###############

all_data07<-rbind(Discrete_data07, Profile_data07)

##
#Site info merge

all_data07_complete<-merge(dat07_site,all_data07,by=c('SITE_ID','VISIT_NO'),keep=T)
all_data07_complete$NAME3<-c('')


#########################################################################################
#2012 data


dat12_siteinfo<-read.csv("./NLA/nla2012_wide_siteinfo_08232016.csv")

dat12_profile<-read.csv("./NLA/nla2012_wide_profile_08232016.csv")

dat12_chem<-read.csv("./NLA/nla2012_waterchem_wide.csv")

dat12_atrazine<-read.csv("./NLA/nla2012_atrazine_08192016.csv")

dat12_secchi<-read.csv("./NLA/nla2012_secchi_08232016.csv")

dat12_chla<-read.csv("./NLA/nla2012_chla_wide.csv")

dat12_toxin<-read.csv("./NLA/nla2012_algaltoxins_08192016.csv")


names<-c('UID','SAMPLE_UNITS','SAMPLE_RESULTS','SAMPLE_FLAG1','SAMPLE_FLAG2','SAMPLE_FLAG3',
         'SAMPLE_TYPE','SAMPLE_MDL','SAMPLE_NAME')


dat12_site<-
    dat12_siteinfo %>%
    select(c('UID','SITE_ID',"VISIT_NO",'DATE_COL','LAT_DD83','LON_DD83',
             'CNTYNAME','STATE','GNIS_ID','GNIS_NAME','NARS_NAME'))


dat12_temp <-
    dat12_profile %>%
    select(c('UID','DEPTH','TEMPERATURE'))
names(dat12_temp)[names(dat12_temp) == "TEMPERATURE"] <- "SAMPLE_RESULT"
dat12_temp$SAMPLE_NAME<-c('TEMPERATURE')
dat12_temp$SAMPLE_TYPE<-c('DEPTH')
dat12_temp$SAMPLE_UNITS<-c('C')
dat12_temp$SAMPLE_FLAG<-c('')
dat12_temp$SAMPLE_QA_FLAG<-c('')
dat12_temp$SAMPLE_LAB_FLAG<-c('')
dat12_temp$SAMPLE_MDL<-c('')

dat12_oxygen <-
    dat12_profile %>%
    select(c('UID','DEPTH','OXYGEN',))
names(dat12_oxygen)[names(dat12_oxygen) == "OXYGEN"] <- "SAMPLE_RESULT"
dat12_oxygen$SAMPLE_NAME<-c('OXYGEN')
dat12_oxygen$SAMPLE_TYPE<-c('DEPTH')
dat12_oxygen$SAMPLE_UNITS<-c('mgL')
dat12_oxygen$SAMPLE_FLAG<-c('')
dat12_oxygen$SAMPLE_QA_FLAG<-c('')
dat12_oxygen$SAMPLE_LAB_FLAG<-c('')
dat12_oxygen$SAMPLE_MDL<-c('')

dat12_ph <-
    dat12_profile %>%
    select(c('UID','DEPTH','PH'))
names(dat12_ph)[names(dat12_ph) == "PH"] <- "SAMPLE_RESULT"
dat12_ph$SAMPLE_NAME<-c('PH')
dat12_ph$SAMPLE_TYPE<-c('DEPTH')
dat12_ph$SAMPLE_UNITS<-c('')
dat12_ph$SAMPLE_FLAG<-c('')
dat12_ph$SAMPLE_QA_FLAG<-c('')
dat12_ph$SAMPLE_LAB_FLAG<-c('')
dat12_ph$SAMPLE_MDL<-c('')

dat12_conductivity <-
    dat12_profile %>%
    select(c('UID','DEPTH','CONDUCTIVITY'))
names(dat12_conductivity)[names(dat12_conductivity) == "CONDUCTIVITY"] <- "SAMPLE_RESULT"
dat12_conductivity$SAMPLE_NAME<-c('CONDUCTIVITY')
dat12_conductivity$SAMPLE_TYPE<-c('DEPTH')
dat12_conductivity$SAMPLE_UNITS<-c('uS/cm')
dat12_conductivity$SAMPLE_FLAG<-c('')
dat12_conductivity$SAMPLE_QA_FLAG<-c('')
dat12_conductivity$SAMPLE_LAB_FLAG<-c('')
dat12_conductivity$SAMPLE_MDL<-c('')


Profile_data12<-rbind(dat12_conductivity,dat12_oxygen,dat12_ph,dat12_temp)
Profile_data12 <- Profile_data12[, c('UID','SAMPLE_UNITS','SAMPLE_RESULT','SAMPLE_FLAG','SAMPLE_QA_FLAG','SAMPLE_LAB_FLAG',
                                 'SAMPLE_TYPE','SAMPLE_MDL','SAMPLE_NAME','DEPTH')]

names1<-c('UID','SAMPLE_UNITS','SAMPLE_RESULTS','SAMPLE_FLAG1','SAMPLE_FLAG2','SAMPLE_FLAG3',
         'SAMPLE_TYPE','SAMPLE_MDL','SAMPLE_NAME','DEPTH')
setnames(Profile_data12, names1)

###########

dat12_chem1 <-
    dat12_chem %>%
    select(c('UID',
             'COND_UNITS','COND_RESULT','COND_FLAG','COND_QA_FLAG','COND_LAB_FLAG'))
dat12_chem1$SAMPLE_TYPE<-c('INTEGRATED')
dat12_chem1$SAMPLE_MDL<-dat12_chem$COND_MDL
dat12_chem1$SAMPLE_NAME<-c('COND')

setnames(dat12_chem1,names)


dat12_chem2 <-
    dat12_chem %>%
    select(c('UID',
             'ANC_UNITS','ANC_RESULT','ANC_FLAG','ANC_QA_FLAG','ANC_LAB_FLAG'))
dat12_chem2$SAMPLE_TYPE<-c('INTEGRATED')
dat12_chem2$ANC_MDL<-c('')
dat12_chem2$SAMPLE_NAME<-c('ANC')


setnames(dat12_chem2,names)

dat12_chem4 <-
    dat12_atrazine %>%
    select(c('UID','ATRAZINE_UNITS','ATRAZINE_RESULT','ATRAZINE_FLAG','ATRAZINE_QA_FLAG'))
dat12_chem4$SAMPLE_FLAG3<-c('')
dat12_chem4$SAMPLE_TYPE<-c('INTEGRATED')
dat12_chem4$SAMPLE_MDL<-c('')
dat12_chem4$SAMPLE_NAME<-c('ATRAZINE')

setnames(dat12_chem4,names)

dat12_chem4 <-
    dat12_chla %>%
    select(c('UID','CHLX_UNITS','CHLX_RESULT','CHLX_QA_FLAG'))
dat12_chem4$SAMPLE_FLAG2<-c('')
dat12_chem4$SAMPLE_FLAG3<-c('')
dat12_chem4$SAMPLE_TYPE<-c('INTEGRATED')
dat12_chem4$SAMPLE_MDL<-dat12_chla$CHLX_MDL
dat12_chem4$SAMPLE_NAME<-c('CHLX')

setnames(dat12_chem4,names)

dat12_chem5 <-
    dat12_chem %>%
    select(c('UID',
             'PH_UNITS','PH_RESULT','PH_FLAG','PH_QA_FLAG','PH_LAB_FLAG'))
dat12_chem5$SAMPLE_TYPE<-c('INTEGRATED')
dat12_chem5$PH_MDL<-c('')
dat12_chem5$SAMPLE_NAME<-c('PH')


setnames(dat12_chem5,names)

dat12_chem6 <-
    dat12_chem %>%
    select(c('UID',
             'TURB_UNITS','TURB_RESULT','TURB_FLAG','TURB_QA_FLAG','TURB_LAB_FLAG'))
dat12_chem6$SAMPLE_TYPE<-c('INTEGRATED')
dat12_chem6$SAMPLE_MDL<-dat12_chem$TURB_MDL
dat12_chem6$SAMPLE_NAME<-c('TURB')

setnames(dat12_chem6,names)

dat12_chem7 <-
    dat12_chem %>%
    select(c('UID',
             'COLOR_UNITS','COLOR_RESULT','COLOR_FLAG','COLOR_QA_FLAG','COLOR_LAB_FLAG'))
dat12_chem7$SAMPLE_TYPE<-c('INTEGRATED')
dat12_chem7$SAMPLE_MDL<-dat12_chem$COLOR_MDL
dat12_chem7$SAMPLE_NAME<-c('COLOR')

setnames(dat12_chem7,names)

dat12_chem8 <-
    dat12_chem %>%
    select(c('UID',
             'CHLORIDE_UNITS','CHLORIDE_RESULT','CHLORIDE_FLAG','CHLORIDE_QA_FLAG','CHLORIDE_LAB_FLAG'))
dat12_chem8$SAMPLE_TYPE<-c('INTEGRATED')
dat12_chem8$SAMPLE_MDL<-dat12_chem$CHLORIDE_MDL
dat12_chem8$SAMPLE_NAME<-c('CHLORIDE')

setnames(dat12_chem8,names)

dat12_chem9 <-
    dat12_chem %>%
    select(c('UID',
             'NITRATE_NITRITE_N_UNITS','NITRATE_NITRITE_N_RESULT',
             'NITRATE_NITRITE_N_FLAG','NITRATE_NITRITE_N_QA_FLAG','NITRATE_NITRITE_N_LAB_FLAG'))
dat12_chem9$SAMPLE_TYPE<-c('INTEGRATED')
dat12_chem9$SAMPLE_MDL<-dat12_chem$NITRATE_NITRITE_N_MDL
dat12_chem9$SAMPLE_NAME<-c('NITRATE_NITRITE_N')

setnames(dat12_chem9,names)


dat12_chem10 <-
    dat12_chem %>%
    select(c('UID',
             'SILICA_UNITS','SILICA_RESULT','SILICA_FLAG','SILICA_QA_FLAG','SILICA_LAB_FLAG'))
dat12_chem10$SAMPLE_TYPE<-c('INTEGRATED')
dat12_chem10$SAMPLE_MDL<-dat12_chem$SILICA_MDL
dat12_chem10$SAMPLE_NAME<-c('SILICA')

setnames(dat12_chem10,names)

dat12_chem11 <-
    dat12_chem %>%
    select(c('UID',
             'MAGNESIUM_UNITS','MAGNESIUM_RESULT','MAGNESIUM_FLAG','MAGNESIUM_QA_FLAG','MAGNESIUM_LAB_FLAG'))
dat12_chem11$SAMPLE_TYPE<-c('INTEGRATED')
dat12_chem11$SAMPLE_MDL<-dat12_chem$MAGNESIUM_MDL
dat12_chem11$SAMPLE_NAME<-c('MAGNESIUM')

setnames(dat12_chem11,names)

dat12_chem12 <-
    dat12_chem %>%
    select(c('UID',
             'SULFATE_UNITS','SULFATE_RESULT','SULFATE_FLAG','SULFATE_QA_FLAG','SULFATE_LAB_FLAG'))
dat12_chem12$SAMPLE_TYPE<-c('INTEGRATED')
dat12_chem12$SAMPLE_MDL<-dat12_chem$SULFATE_MDL
dat12_chem12$SAMPLE_NAME<-c('SULFATE')

setnames(dat12_chem12,names)

dat12_chem13 <-
    dat12_chem %>%
    select(c('UID',
             'ALUMINUM_UNITS','ALUMINUM_RESULT','ALUMINUM_QA_FLAG','ALUMINUM_LAB_FLAG'))
dat12_chem13$SAMPLE_FLAG3<-c('')
dat12_chem13$SAMPLE_TYPE<-c('INTEGRATED')
dat12_chem13$SAMPLE_MDL<-dat12_chem$ALUMINUM_MDL
dat12_chem13$SAMPLE_NAME<-c('ALUMINUM')

setnames(dat12_chem13,names)

dat12_chem14 <-
    dat12_chem %>%
    select(c('UID',
             'CALCIUM_UNITS','CALCIUM_RESULT','CALCIUM_FLAG','CALCIUM_QA_FLAG','CALCIUM_LAB_FLAG'))
dat12_chem14$SAMPLE_TYPE<-c('INTEGRATED')
dat12_chem14$SAMPLE_MDL<-dat12_chem$CALCIUM_MDL
dat12_chem14$SAMPLE_NAME<-c('CALCIUM')

setnames(dat12_chem14,names)

dat12_chem15 <-
    dat12_chem %>%
    select(c('UID',
             'AMMONIA_N_UNITS','AMMONIA_N_RESULT','AMMONIA_N_QA_FLAG','AMMONIA_N_LAB_FLAG'))
dat12_chem15$SAMPLE_FLAG3<-c('')
dat12_chem15$SAMPLE_TYPE<-c('INTEGRATED')
dat12_chem15$SAMPLE_MDL<-dat12_chem$AMMONIA_N_MDL
dat12_chem15$SAMPLE_NAME<-c('AMMONIA')

setnames(dat12_chem15,names)


dat12_chem16 <-
    dat12_chem %>%
    select(c('UID',
             'DOC_UNITS','DOC_RESULT','DOC_FLAG','DOC_QA_FLAG','DOC_LAB_FLAG'))
dat12_chem16$SAMPLE_TYPE<-c('INTEGRATED')
dat12_chem16$SAMPLE_MDL<-dat12_chem$DOC_MDL
dat12_chem16$SAMPLE_NAME<-c('DOC')

setnames(dat12_chem16,names)

dat12_chem17 <-
    dat12_chem %>%
    select(c('UID',
             'SODIUM_UNITS','SODIUM_RESULT','SODIUM_QA_FLAG'))
dat12_chem17$SAMPLE_FLAG2<-c('')
dat12_chem17$SAMPLE_FLAG3<-c('')
dat12_chem17$SAMPLE_TYPE<-c('INTEGRATED')
dat12_chem17$SAMPLE_MDL<-dat12_chem$SODIUM_MDL
dat12_chem17$SAMPLE_NAME<-c('SODIUM')

setnames(dat12_chem17,names)

dat12_chem18 <-
    dat12_chem %>%
    select(c('UID',
             'TSS_UNITS','TSS_RESULT','TSS_FLAG','TSS_QA_FLAG'))
dat12_chem18$SAMPLE_FLAG3<-c('')
dat12_chem18$SAMPLE_TYPE<-c('INTEGRATED')
dat12_chem18$SAMPLE_MDL<-dat12_chem$TSS_MDL
dat12_chem18$SAMPLE_NAME<-c('TSS')

setnames(dat12_chem18,names)


dat12_chem19 <-
    dat12_chem %>%
    select(c('UID',
             'POTASSIUM_UNITS','POTASSIUM_RESULT','POTASSIUM_FLAG','POTASSIUM_QA_FLAG','POTASSIUM_LAB_FLAG'))
dat12_chem19$SAMPLE_TYPE<-c('INTEGRATED')
dat12_chem19$SAMPLE_MDL<-dat12_chem$POTASSIUM_MDL
dat12_chem19$SAMPLE_NAME<-c('POTASSIUM')

setnames(dat12_chem19,names)

dat12_chem20 <-
    dat12_chem %>%
    select(c('UID',
             'PTL_UNITS','PTL_RESULT','PTL_FLAG','PTL_QA_FLAG','PTL_LAB_FLAG'))
dat12_chem20$SAMPLE_TYPE<-c('INTEGRATED')
dat12_chem20$SAMPLE_MDL<-dat12_chem$PTL_MDL
dat12_chem20$SAMPLE_NAME<-c('PTL')

setnames(dat12_chem20,names)

dat12_chem21 <-
    dat12_chem %>%
    select(c('UID',
             'NTL_UNITS','NTL_RESULT','NTL_FLAG','NTL_QA_FLAG','NTL_LAB_FLAG'))
dat12_chem21$SAMPLE_TYPE<-c('INTEGRATED')
dat12_chem21$SAMPLE_MDL<-dat12_chem$NTL_MDL
dat12_chem21$SAMPLE_NAME<-c('NTL')

setnames(dat12_chem21,names)

dat12_chem22 <-
    dat12_chem %>%
    select(c('UID',
             'TOC_UNITS','TOC_RESULT'))
dat12_chem22$TOC_FLAG<-c('')
dat12_chem22$TOC_QA_FLAG<-c('')
dat12_chem22$TOC_LAB_FLAG<-c('')
dat12_chem22$SAMPLE_TYPE<-c('INTEGRATED')
dat12_chem22$TOC_MDL<-c('')
dat12_chem22$SAMPLE_NAME<-c('TOC')

setnames(dat12_chem22,names)

dat12_chem23 <-
    dat12_chem %>%
    select(c('UID',
             'NITRITE_N_UNITS','NITRITE_N_RESULT','NITRITE_N_QA_FLAG','NITRITE_N_LAB_FLAG'))
dat12_chem23$SAMPLE_FLAG3<-c('')
dat12_chem23$SAMPLE_TYPE<-c('INTEGRATED')
dat12_chem23$SAMPLE_MDL<-dat12_chem$NITRITE_N_MDL
dat12_chem23$SAMPLE_NAME<-c('NITRITE_N')

setnames(dat12_chem23,names)

dat12_chem24 <-
    dat12_toxin %>%
    select(c('UID',
             'MICX_UNITS','MICX_RESULT','MICX_FLAG','MICX_QA_FLAG'))
dat12_chem24$SAMPLE_FLAG3<-c('')
dat12_chem24$SAMPLE_TYPE<-c('INTEGRATED')
dat12_chem24$SAMPLE_MDL<-dat12_toxin$MICX_MDL
dat12_chem24$SAMPLE_NAME<-c('MICX')

setnames(dat12_chem24,names)


Discrete_data12<-rbind(dat12_chem1,
                    dat12_chem2,
                    dat12_chem4,
                    dat12_chem4,
                    dat12_chem5,
                    dat12_chem6,
                    dat12_chem7,
                    dat12_chem8,
                    dat12_chem9,
                    dat12_chem10,
                    dat12_chem11,
                    dat12_chem12,
                    dat12_chem13,
                    dat12_chem14,
                    dat12_chem15,
                    dat12_chem16,
                    dat12_chem17,
                    dat12_chem18,
                    dat12_chem19,
                    dat12_chem20,
                    dat12_chem21,
                    dat12_chem22,
                    dat12_chem23,
                    dat12_chem24)
Discrete_data12$DEPTH<-c('')



############# 

names2<-c('UID','SAMPLE_RESULTS','SAMPLE_UNITS','SAMPLE_FLAG1','SAMPLE_FLAG2','SAMPLE_FLAG3',
         'SAMPLE_TYPE','SAMPLE_MDL','SAMPLE_NAME','DEPTH')


dat12_secchi <-
    dat12_secchi %>%
    select(c('UID','SECCHI'))
names(dat12_secchi)[names(dat12_secchi) == "SECCHI"] <- "SAMPLE_RESULT"
dat12_secchi$SAMPLE_UNITS<-c('m')
dat12_secchi$SAMPLE_FLAG<-c('')
dat12_secchi$SAMPLE_QA_FLAG<-c('')
dat12_secchi$SAMPLE_LAB_FLAG<-c('')
dat12_secchi$SAMPLE_TYPE<-c('GRAB')
dat12_secchi$SAMPLE_MDL<-c('')
dat12_secchi$SAMPLE_NAME<-c('SECCHI')
dat12_secchi$DEPTH<-c('0')

setnames(dat12_secchi,names2)

all_data12<-rbind(dat12_secchi,Discrete_data12, Profile_data12)

##
#Site info merge


all_data12_complete<-merge(dat12_site,all_data12,by='UID',keep=T)

#####################################
#2017 data

dat17_siteinfo<-read.csv("./NLA/nla_2017_site_information-data.csv")

dat17_profile<-read.csv("./NLA/nla_2017_profile-data.csv")

dat17_chem<-read.csv("./NLA/nla_2017_water_chemistry_chla-data.csv")

dat17_atrazine<-read.csv("./NLA/nla_2017_atrazine-data_0.csv")

dat17_secchi<-read.csv("./NLA/nla_2017_secchi-data.csv")

dat17_toxin<-read.csv("./NLA/nla_2017_algal_toxin-data.csv")
dat17_tox<-dat17_toxin[dat17_toxin$ANALYTE=="MICX",]

dat17_ecoli<-read.csv("./NLA/nla_2017_e.coli-data.csv")


names<-c('UID','SAMPLE_UNITS','SAMPLE_RESULTS','SAMPLE_FLAG1','SAMPLE_FLAG2','SAMPLE_FLAG3',
         'SAMPLE_TYPE','SAMPLE_MDL','SAMPLE_NAME')


dat17_site<-
    dat17_siteinfo %>%
    select(c('UID','SITE_ID',"VISIT_NO",'DATE_COL','LAT_DD83','LON_DD83',
             'CNTYNAME','STATE_NM','GNIS_ID','GNIS_NAME','NARS_NAME'))



###
#Chem data

dat17_chem1 <-
    dat17_chem %>%
    select(c('UID','RESULT','RESULT_UNITS','MDL','NARS_FLAG','LAB_COMMENT'))
dat17_chem1$SAMPLE_TYPE<-c('INTEGRATED')
dat17_chem1$SAMPLE_NAME<-dat17_chem$ANALYTE
dat17_chem1$SAMPLE_FLAG3<-c('')
dat17_chem1$DEPTH<-c('')

dat17_chem1 <- dat17_chem1[, c('UID','RESULT_UNITS','RESULT','NARS_FLAG','LAB_COMMENT','SAMPLE_FLAG3',
                                     'SAMPLE_TYPE','MDL','SAMPLE_NAME','DEPTH')]

names3<-c('UID','SAMPLE_UNITS','SAMPLE_RESULTS','SAMPLE_FLAG1','SAMPLE_FLAG2','SAMPLE_FLAG3',
          'SAMPLE_TYPE','SAMPLE_MDL','SAMPLE_NAME','DEPTH')
setnames(dat17_chem1, names3)



dat17_chem2 <-
    dat17_toxin %>%
    select(c('UID','RESULT','RESULT_UNITS','MDL','NARS_FLAG',))
dat17_chem2$SAMPLE_TYPE<-c('INTEGRATED')
dat17_chem2$SAMPLE_NAME<-dat17_toxin$ANALYTE
dat17_chem2$SAMPLE_FLAG2<-c('')
dat17_chem2$SAMPLE_FLAG3<-c('')
dat17_chem2$DEPTH<-c('')

dat17_chem2 <- dat17_chem2[, c('UID','RESULT_UNITS','RESULT','NARS_FLAG','SAMPLE_FLAG2','SAMPLE_FLAG3',
                               'SAMPLE_TYPE','MDL','SAMPLE_NAME','DEPTH')]

names3<-c('UID','SAMPLE_UNITS','SAMPLE_RESULTS','SAMPLE_FLAG1','SAMPLE_FLAG2','SAMPLE_FLAG3',
          'SAMPLE_TYPE','SAMPLE_MDL','SAMPLE_NAME','DEPTH')
setnames(dat17_chem2, names3)


dat17_chem3 <-
    dat17_atrazine %>%
    select(c('UID','RESULT','RESULT_UNITS','MDL','NARS_FLAG'))
dat17_chem3$SAMPLE_TYPE<-c('INTEGRATED')
dat17_chem3$SAMPLE_NAME<-dat17_atrazine$ANALYTE
dat17_chem3$SAMPLE_FLAG2<-c('')
dat17_chem3$SAMPLE_FLAG3<-c('')
dat17_chem3$DEPTH<-c('')

dat17_chem3 <- dat17_chem3[, c('UID','RESULT_UNITS','RESULT','NARS_FLAG','SAMPLE_FLAG2','SAMPLE_FLAG3',
                               'SAMPLE_TYPE','MDL','SAMPLE_NAME','DEPTH')]

names3<-c('UID','SAMPLE_UNITS','SAMPLE_RESULTS','SAMPLE_FLAG1','SAMPLE_FLAG2','SAMPLE_FLAG3',
          'SAMPLE_TYPE','SAMPLE_MDL','SAMPLE_NAME','DEPTH')
setnames(dat17_chem3, names3)


dat17_sec <-
    dat17_secchi %>%
    select(c('UID','COMMENT'))
dat17_sec$SAMPLE_TYPE<-c('GRAB')
dat17_sec$RESULT<-(mean(c(as.numeric(dat17_secchi$DISAPPEARS),as.numeric(dat17_secchi$REAPPEARS))))
dat17_sec$RESULT_UNITS<-c('m')
dat17_sec$SAMPLE_NAME<-c('SECCHI')
dat17_sec$SAMPLE_FLAG2<-c('')
dat17_sec$SAMPLE_FLAG3<-c('')
dat17_sec$MDL<-c('')
dat17_sec$DEPTH<-c(0)

dat17_sec <- dat17_sec[, c('UID','RESULT_UNITS','RESULT','COMMENT','SAMPLE_FLAG2','SAMPLE_FLAG3',
                               'SAMPLE_TYPE','MDL','SAMPLE_NAME','DEPTH')]

names3<-c('UID','SAMPLE_UNITS','SAMPLE_RESULTS','SAMPLE_FLAG1','SAMPLE_FLAG2','SAMPLE_FLAG3',
          'SAMPLE_TYPE','SAMPLE_MDL','SAMPLE_NAME','DEPTH')
setnames(dat17_sec, names3)


dat17_e.coli <-
    dat17_ecoli %>%
    select(c('UID','E_COLI_RESULT','MDL','E_COLI_NARS_FLAG'))
dat17_e.coli$SAMPLE_FLAG1 <- paste(dat17_ecoli$E_COLI_IM_COMMENT, "$", dat17_ecoli$COND_COMMENT)
dat17_e.coli$SAMPLE_FLAG2 <- paste(dat17_ecoli$QC_COMMENT, "$", dat17_ecoli$HQ_NOTES)
dat17_e.coli$SAMPLE_TYPE<-c('GRAB')
dat17_e.coli$RESULT_UNITS<-c('Most Probable Number')
dat17_e.coli$SAMPLE_NAME<-dat17_ecoli$ANALYTE
dat17_e.coli$DEPTH<-c('')


dat17_e.coli <- dat17_e.coli[, c('UID','RESULT_UNITS','E_COLI_RESULT','SAMPLE_FLAG1','SAMPLE_FLAG2','E_COLI_NARS_FLAG',
                           'SAMPLE_TYPE','MDL','SAMPLE_NAME','DEPTH')]

names3<-c('UID','SAMPLE_UNITS','SAMPLE_RESULTS','SAMPLE_FLAG1','SAMPLE_FLAG2','SAMPLE_FLAG3',
          'SAMPLE_TYPE','SAMPLE_MDL','SAMPLE_NAME','DEPTH')

setnames(dat17_e.coli, names3)


Discrete_data17<-rbind(dat17_chem1,
                       dat17_chem2,
                       dat17_chem3,
                       dat17_e.coli,
                       dat17_sec)


################
##Profile data


dat17_temp <-
    dat17_profile %>%
    select(c('UID','DEPTH','TEMPERATURE'))
names(dat17_temp)[names(dat17_temp) == "TEMPERATURE"] <- "SAMPLE_RESULT"
dat17_temp$SAMPLE_NAME<-c('TEMPERATURE')
dat17_temp$SAMPLE_TYPE<-c('DEPTH')
dat17_temp$SAMPLE_UNITS<-c('C')
dat17_temp$SAMPLE_FLAG1<-dat17_profile$FLAG
dat17_temp$SAMPLE_FLAG2<-c('')
dat17_temp$SAMPLE_FLAG3<-c('')
dat17_temp$SAMPLE_MDL<-c('')

dat17_PH <-
    dat17_profile %>%
    select(c('UID','DEPTH','PH'))
names(dat17_PH)[names(dat17_PH) == "PH"] <- "SAMPLE_RESULT"
dat17_PH$SAMPLE_NAME<-c('PHERATURE')
dat17_PH$SAMPLE_TYPE<-c('DEPTH')
dat17_PH$SAMPLE_UNITS<-c('')
dat17_PH$SAMPLE_FLAG1<-dat17_profile$FLAG
dat17_PH$SAMPLE_FLAG2<-c('')
dat17_PH$SAMPLE_FLAG3<-c('')
dat17_PH$SAMPLE_MDL<-c('')

dat17_PH <-
    dat17_profile %>%
    select(c('UID','DEPTH','PH'))
names(dat17_PH)[names(dat17_PH) == "PH"] <- "SAMPLE_RESULT"
dat17_PH$SAMPLE_NAME<-c('PH')
dat17_PH$SAMPLE_TYPE<-c('DEPTH')
dat17_PH$SAMPLE_UNITS<-c('')
dat17_PH$SAMPLE_FLAG1<-dat17_profile$FLAG
dat17_PH$SAMPLE_FLAG2<-c('')
dat17_PH$SAMPLE_FLAG3<-c('')
dat17_PH$SAMPLE_MDL<-c('')

dat17_OXYGEN <-
    dat17_profile %>%
    select(c('UID','DEPTH','OXYGEN'))
names(dat17_OXYGEN)[names(dat17_OXYGEN) == "OXYGEN"] <- "SAMPLE_RESULT"
dat17_OXYGEN$SAMPLE_NAME<-c('OXYGEN')
dat17_OXYGEN$SAMPLE_TYPE<-c('DEPTH')
dat17_OXYGEN$SAMPLE_UNITS<-c('mg/L')
dat17_OXYGEN$SAMPLE_FLAG1<-dat17_profile$FLAG
dat17_OXYGEN$SAMPLE_FLAG2<-c('')
dat17_OXYGEN$SAMPLE_FLAG3<-c('')
dat17_OXYGEN$SAMPLE_MDL<-c('')

dat17_CONDUCTIVITY <-
    dat17_profile %>%
    select(c('UID','DEPTH','CONDUCTIVITY'))
names(dat17_CONDUCTIVITY)[names(dat17_CONDUCTIVITY) == "CONDUCTIVITY"] <- "SAMPLE_RESULT"
dat17_CONDUCTIVITY$SAMPLE_NAME<-c('CONDUCTIVITY')
dat17_CONDUCTIVITY$SAMPLE_TYPE<-c('DEPTH')
dat17_CONDUCTIVITY$SAMPLE_UNITS<-c('uS/cm')
dat17_CONDUCTIVITY$SAMPLE_FLAG1<-dat17_profile$FLAG
dat17_CONDUCTIVITY$SAMPLE_FLAG2<-c('')
dat17_CONDUCTIVITY$SAMPLE_FLAG3<-c('')
dat17_CONDUCTIVITY$SAMPLE_MDL<-c('')


Profile_data17<-rbind(dat17_CONDUCTIVITY,dat17_OXYGEN,dat17_PH,dat17_temp)
Profile_data17 <- Profile_data17[, c('UID','SAMPLE_UNITS','SAMPLE_RESULT','SAMPLE_FLAG1','SAMPLE_FLAG2','SAMPLE_FLAG3',
                                     'SAMPLE_TYPE','SAMPLE_MDL','SAMPLE_NAME','DEPTH')]

names4<-c('UID','SAMPLE_UNITS','SAMPLE_RESULTS','SAMPLE_FLAG1','SAMPLE_FLAG2','SAMPLE_FLAG3',
          'SAMPLE_TYPE','SAMPLE_MDL','SAMPLE_NAME','DEPTH')

setnames(Profile_data17, names4)


######################

all_data17<-rbind(Discrete_data17, Profile_data17)

##
#Site info merge


all_data17_complete<-merge(dat17_site,all_data17,by='UID',keep=T)

######################################
#Combining

names5<-c('UID','SITE_ID',"VISIT_NO",'DATE_COL','LAT_DD','LON_DD','STATE',
          'CNTYNAME','NAME1','NAME2','NAME3','SAMPLE_UNITS',
          'SAMPLE_RESULT','SAMPLE_FLAG1','SAMPLE_FLAG2','SAMPLE_FLAG3',
          'SAMPLE_TYPE','SAMPLE_MDL','SAMPLE_NAME','DEPTH')


all_data07_complete1 <- all_data07_complete[, c('UID','SITE_ID',"VISIT_NO",'DATE_COL','LAT_DD','LON_DD','ST',
                                               'CNTYNAME','NHDNAME','LAKENAME','NAME3','SAMPLE_UNITS',
                                               'SAMPLE_RESULTS','SAMPLE_FLAG1','SAMPLE_FLAG2','SAMPLE_FLAG3',
                                               'SAMPLE_TYPE','SAMPLE_MDL','SAMPLE_NAME','DEPTH')]

setnames(all_data07_complete1, names5)
all_data07_complete1$Program<-c('NLA2007')


all_data12_complete1 <- all_data12_complete[, c('UID','SITE_ID',"VISIT_NO",'DATE_COL','LAT_DD83','LON_DD83','STATE',
                                               'CNTYNAME','GNIS_ID','GNIS_NAME','NARS_NAME','SAMPLE_UNITS',
                                               'SAMPLE_RESULTS','SAMPLE_FLAG1','SAMPLE_FLAG2','SAMPLE_FLAG3',
                                               'SAMPLE_TYPE','SAMPLE_MDL','SAMPLE_NAME','DEPTH')]

setnames(all_data12_complete1, names5)
all_data12_complete1$Program<-c('NLA2012')


all_data17_complete1 <- all_data17_complete[, c('UID','SITE_ID',"VISIT_NO",'DATE_COL','LAT_DD83','LON_DD83','STATE_NM',
                                               'CNTYNAME','GNIS_ID','GNIS_NAME','NARS_NAME','SAMPLE_UNITS',
                                               'SAMPLE_RESULTS','SAMPLE_FLAG1','SAMPLE_FLAG2','SAMPLE_FLAG3',
                                               'SAMPLE_TYPE','SAMPLE_MDL','SAMPLE_NAME','DEPTH')]

setnames(all_data17_complete1, names5)
all_data17_complete1$Program<-c('NLA2017')

Compiled_data<-rbind(all_data07_complete1,all_data12_complete1,all_data17_complete1)

Compiled_data$NAMES <- paste(Compiled_data$NAME1,Compiled_data$NAME2,Compiled_data$NAME3,sep='$')
Compiled_data$source_comments <- paste(Compiled_data$SAMPLE_FLAG1,Compiled_data$SAMPLE_FLAG2,Compiled_data$SAMPLE_FLAG3,sep='$')
Compiled_data$row <- seq.int(nrow(Compiled_data))
Compiled_data$obs_id <- paste(Compiled_data$Program,Compiled_data$row,sep='-')
Compiled_data$organization_id <-c('NARS_WQX')
Compiled_data$organization_name <-c('EPA National Aquatic Resources Survey (NARS)')
Compiled_data$source_activityid <-c('')
Compiled_data$source_detectionlimit_unit <-c('')
Compiled_data$source_detectionlimit_value <-Compiled_data$SAMPLE_MDL
Compiled_data$source_labmethoddescription <-c('')
Compiled_data$source_labmethodid <-c('')
Compiled_data$source_labmethodname <-c('')
Compiled_data$source_parameter<-Compiled_data$SAMPLE_NAME
Compiled_data$source_sampledepth<-Compiled_data$DEPTH
Compiled_data$source_sampleposition<-c('')
Compiled_data$source_samplesiteid<-c('')
Compiled_data$source_sampletype<-Compiled_data$SAMPLE_TYPE
Compiled_data$source_unit<-Compiled_data$SAMPLE_UNITS
Compiled_data$source_value<-Compiled_data$SAMPLE_RESULT
Compiled_data$source_methodqualifier<-c('')

Compiled_data$valueid<-c('')
Compiled_data$lagoslakeid<-c('')
Compiled_data$lagoslakeid<-c('')
Compiled_data$sampledate<-Compiled_data$DATE_COL
Compiled_data$lagos_variableid<-c('')
Compiled_data$lagos_variablename<-c('')
Compiled_data$datavalue<-c('')
Compiled_data$datavalue_conversion<-c('')
Compiled_data$datavalue_unit<-c('')
Compiled_data$detectionlimit_value<-c('')
Compiled_data$detectionlimitvalue_conversion<-c('')
Compiled_data$lagos_comments<-c('')
Compiled_data$lagos_sampledepth<-c('')
Compiled_data$lagos_sampleposition<-c('')
Compiled_data$lagos_sampletype<-c('')


Final_NLA_data <- Compiled_data[, c('UID','SITE_ID','NAMES','LAT_DD','LON_DD','STATE','CNTYNAME',
                                   'valueid','obs_id','lagoslakeid', 'sampledate','lagos_variableid',
                                   'lagos_variablename', 'datavalue', 'datavalue_unit', 'detectionlimit_value',
                                   'datavalue_conversion', 'detectionlimitvalue_conversion', 'lagos_comments',
                                   'lagos_sampledepth', 'lagos_sampleposition','lagos_sampletype', 'organization_id',
                                   'organization_name','source_activityid', 'source_comments', 'source_detectionlimit_value',
                                   'source_labmethoddescription', 'source_labmethodid', 'source_labmethodname', 'source_parameter',
                                   'source_sampledepth', 'source_sampleposition', 'source_samplesiteid', 'source_sampletype',
                                   'source_unit','source_value',  'source_methodqualifier')]


library(tidyverse)

Sites<-
    Compiled_data %>% 
    select(NAMES,SITE_ID,LAT_DD,LON_DD) %>%
    distinct()

write.csv(Final_NLA_data,"PreFinal_NLA_data.csv")

write.csv(Sites,"Final_NLA_Sites.csv")

