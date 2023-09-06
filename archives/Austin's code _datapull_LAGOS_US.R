#install packages
install.packages("dataRetrieval")
install.packages("lubridate", dependencies = TRUE)
install.packages("httr")
library(dataRetrieval)
library(lubridate)
library(httr)
library(tidyverse)


############################********************************USGS DATA GRAB*************************************################################################

all_lakes <- whatWQPdata(siteType = "Lake, Reservoir, Impoundment")

org_name<- c("USGS Colorado Water Science Center") ##List for iterating over states one at a time (full list above and commented out)


###CANT GET WORK BUT CONTINUING ON FOR NOW, JUST LOOKING AT FILE'S INDIVIDUALLY FOR DATE INFORMATON###

all_lakes_reduced = all_lakes[which(all_lakes$OrganizationFormalName == org_name ),]
all_lakes_lakedata = readWQPdata(siteNumbers= all_lakes_reduced$MonitoringLocationIdentifier,startDate = "1975-01-01",endDate="2018-01-20") #pull in the data
#test_data <- subset(all_lakes_lakedata, all_lakes_lakedata$CharacteristicName %in% test_vars)

write.csv(all_lakes_lakedata, "CO_USGS_data.csv")



#DON't NEED - We are NOT pulling sites data 

#sites <- whatWQPsites(statecode="CO")

#CO_USGS_sites <-  subset(sites, sites$MonitoringLocationIdentifier %in% all_lakes_lakedata$MonitoringLocationIdentifier)

#write.csv(CO_USGS_sites, "CO_USGS_sites.csv")




##Clean USGS data

##Pull data from Dropbox
co_usgs <- read.csv("~/Lagos Limno/CO_USGS_data.csv", stringsAsFactors = FALSE, row.names = NULL)

co_na_non_detect <- co_usgs[(co_usgs$ResultDetectionConditionText == "Detected Not Quantified" | co_usgs$ResultDetectionConditionText == "Present Above Quantification Limit" | 
                               is.na(co_usgs$ResultDetectionConditionText)),]

CO_adjusted <- distinct(co_na_non_detect, ActivityStartDate, ActivityDepthHeightMeasure.MeasureValue, ActivityDepthHeightMeasure.MeasureUnitCode,
                        MonitoringLocationIdentifier, CharacteristicName, ResultMeasureValue, ResultSampleFractionText, USGSPCode, .keep_all = TRUE)

CO_adjusted$source_parameter <- CO_adjusted$CharacteristicName
CO_adjusted$source_unit <- CO_adjusted$ResultMeasure.MeasureUnitCode

##define dt cols (value and  unit)
CO_adjusted$detectionlimit_legacy <- CO_adjusted$DetectionQuantitationLimitMeasure.MeasureValue
CO_adjusted$detectionlimit_unit_legacy <- CO_adjusted$DetectionQuantitationLimitMeasure.MeasureUnitCode

dt_convert <- function(param_col,param_name,conversion, origin_unit, convert_unit){
  CO_adjusted$detectionlimit_legacy <- ifelse(CO_adjusted[,param_col]== param_name & CO_adjusted$detectionlimit_unit_legacy == origin_unit, 
                                              CO_adjusted$detectionlimit_legacy*conversion, CO_adjusted$detectionlimit_legacy)
  CO_adjusted$detectionlimit_unit_legacy <- ifelse(CO_adjusted[,param_col]== param_name,convert_unit, CO_adjusted$detectionlimit_unit_legacy)
  
  return(CO_adjusted)
}

dt_convert_w_pcode <- function(param_col,param_name,conversion, pcode, convert_unit){
  CO_adjusted$detectionlimit_legacy <- ifelse(CO_adjusted[,param_col]== param_name & CO_adjusted$USGSPCode == pcode, 
                                              CO_adjusted$detectionlimit_legacy*conversion, CO_adjusted$detectionlimit_legacy)
  CO_adjusted$detectionlimit_unit_legacy <- ifelse(CO_adjusted[,param_col]== param_name,convert_unit, CO_adjusted$detectionlimit_unit_legacy)
  
  return(CO_adjusted)
}

dt_unit_check <- CO_adjusted %>% 
  group_by(CharacteristicName) %>% 
  drop_na(detectionlimit_legacy) %>% 
  #distinct(variableid_lagos, detectionlimit_unit_legacy)
  summarise(dt_units_legacy = toString(unique(detectionlimit_unit_legacy))) %>% 
  ungroup()





# #test
# CO_adjusted$detectionlimit_legacy <- ifelse(CO_adjusted[,'CharacteristicName']== 'Ammonia and ammonium' & CO_adjusted$USGSPCode == '608', 
#                                             CO_adjusted$detectionlimit_legacy*1000, CO_adjusted$detectionlimit_legacy)
# CO_adjusted$detectionlimit_unit_legacy <- ifelse(CO_adjusted[,'CharacteristicName']== 'Ammonia and ammonium','ug/L', CO_adjusted$detectionlimit_unit_legacy)


##ALKALINITY
CO_adjusted <- CO_adjusted[!(CO_adjusted$CharacteristicName == "Alkalinity, total" & CO_adjusted$USGSPCode == '409'),]

CO_adjusted$ResultMeasureValue <- ifelse(CO_adjusted$CharacteristicName=='Alkalinity, total', CO_adjusted$ResultMeasureValue*19.98, CO_adjusted$ResultMeasureValue)

CO_adjusted$ResultMeasure.MeasureUnitCode <- ifelse(CO_adjusted$CharacteristicName=='Alkalinity, total', "ueq/L", CO_adjusted$ResultMeasure.MeasureUnitCode)


CO_adjusted <- dt_convert('CharacteristicName','Alkalinity, total',19.98,'mg/l CaCO3','ueq/L')


CO_adjusted$CharacteristicName <- ifelse(CO_adjusted$CharacteristicName == "Alkalinity, total", "Alkalinity", CO_adjusted$CharacteristicName)



#Alumninum 

CO_adjusted$ResultMeasure.MeasureUnitCode <- ifelse(CO_adjusted$CharacteristicName=='Aluminum, dissolved', "ug/L", CO_adjusted$ResultMeasure.MeasureUnitCode)
CO_adjusted$ResultMeasure.MeasureUnitCode <- ifelse(CO_adjusted$CharacteristicName=='Aluminum, total', "ug/L", CO_adjusted$ResultMeasure.MeasureUnitCode)


##Ammonia Nitrogen

CO_adjusted <- CO_adjusted[!(CO_adjusted$CharacteristicName == "Ammonia"),]

#Dissolved
CO_adjusted$ResultMeasureValue <- ifelse(CO_adjusted$CharacteristicName=='Ammonia'& CO_adjusted$USGSPCode == '619', 
                                         CO_adjusted$ResultMeasureValue*1000, CO_adjusted$ResultMeasureValue)

CO_adjusted$ResultMeasureValue <- ifelse(CO_adjusted$CharacteristicName=='Ammonia and ammonium'& CO_adjusted$USGSPCode == '608', 
                                         CO_adjusted$ResultMeasureValue*1000, CO_adjusted$ResultMeasureValue)

CO_adjusted$ResultMeasureValue <- ifelse(CO_adjusted$CharacteristicName=='Ammonia and ammonium' & CO_adjusted$USGSPCode == '71846', 
                                         CO_adjusted$ResultMeasureValue*776.5, CO_adjusted$ResultMeasureValue)

CO_adjusted <- dt_convert_w_pcode('CharacteristicName','Ammonia',1000,'619','ug/L')
CO_adjusted <- dt_convert_w_pcode('CharacteristicName','Ammonia and ammonium',1000,'608','ug/L')
CO_adjusted <- dt_convert_w_pcode('CharacteristicName','Ammonia and ammonium',776.5,'71846','ug/L')


CO_adjusted$ResultMeasure.MeasureUnitCode <- ifelse(CO_adjusted$CharacteristicName=='Ammonia and ammonium', "ug/L", CO_adjusted$ResultMeasure.MeasureUnitCode)

CO_adjusted$CharacteristicName <- ifelse(CO_adjusted$CharacteristicName == "Ammonia and ammonium", "Nitrogen, NH4", CO_adjusted$CharacteristicName)
CO_adjusted$CharacteristicName <- ifelse(CO_adjusted$CharacteristicName == "Ammonia", "Nitrogen, NH4", CO_adjusted$CharacteristicName)


###########Aresenic#####
##Nothing needed## except for unit renaming

CO_adjusted$ResultMeasure.MeasureUnitCode <- ifelse(CO_adjusted$CharacteristicName=='Arsenic, dissolved', "ug/L", CO_adjusted$ResultMeasure.MeasureUnitCode)

CO_adjusted$ResultMeasure.MeasureUnitCode <- ifelse(CO_adjusted$CharacteristicName=='Arsenic, total', "ug/L", CO_adjusted$ResultMeasure.MeasureUnitCode)


##One record of Atrazine. No units given, and pcode not recognized. Remove
CO_adjusted <- CO_adjusted[!(CO_adjusted$CharacteristicName == "Atrazine"),]



#calcium 
CO_adjusted$ResultMeasureValue <- ifelse(CO_adjusted$CharacteristicName=='Calcium, dissolved' & CO_adjusted$USGSPCode == '91051', 
                                         CO_adjusted$ResultMeasureValue/1000, CO_adjusted$ResultMeasureValue)

CO_adjusted$ResultMeasure.MeasureUnitCode <- ifelse(CO_adjusted$CharacteristicName=='Calcium, dissolved', "mg/L", CO_adjusted$ResultMeasure.MeasureUnitCode)

CO_adjusted <- dt_convert_w_pcode('CharacteristicName','Calcium, dissolved',.001,'91051','mg/L')


##Chloride##
CO_adjusted$ResultMeasureValue <- ifelse(CO_adjusted$CharacteristicName=='Chloride' & CO_adjusted$USGSPCode == '91001', 
                                         CO_adjusted$ResultMeasureValue/1000, CO_adjusted$ResultMeasureValue)

CO_adjusted$ResultMeasure.MeasureUnitCode <- ifelse(CO_adjusted$CharacteristicName=='Chloride', "mg/L", CO_adjusted$ResultMeasure.MeasureUnitCode)

CO_adjusted <- dt_convert_w_pcode('CharacteristicName','Chloride',.001,'91001','mg/L')


CO_adjusted$CharacteristicName <- ifelse(CO_adjusted$CharacteristicName == "Chloride", "Chloride, dissolved", CO_adjusted$CharacteristicName)



##Chlorophyll
#remove unwanted parameters 
CO_adjusted <- CO_adjusted[!(CO_adjusted$USGSPCode == "65231"), ]
CO_adjusted <- CO_adjusted[!(CO_adjusted$USGSPCode == "32230"), ]

CO_adjusted$ResultMeasure.MeasureUnitCode <- ifelse(CO_adjusted$CharacteristicName=='Chlorophyll a ', "ug/L", CO_adjusted$ResultMeasure.MeasureUnitCode)




##Depth
#remove unwanted parameter
CO_adjusted <- CO_adjusted[!(CO_adjusted$USGSPCode == '72001'), ] ##Depth of well, feet below land surface datum


##samplesite depth in feet
CO_adjusted$ResultMeasureValue <- ifelse(CO_adjusted$CharacteristicName=='Depth' & CO_adjusted$USGSPCode == '81903', CO_adjusted$ResultMeasureValue*0.3048, CO_adjusted$ResultMeasureValue)

CO_adjusted$ResultMeasure.MeasureUnitCode <- ifelse(CO_adjusted$CharacteristicName=='Depth' & CO_adjusted$USGSPCode == '81903', "m", CO_adjusted$ResultMeasure.MeasureUnitCode)

CO_adjusted$CharacteristicName <- ifelse(CO_adjusted$CharacteristicName == "Depth" & CO_adjusted$USGSPCode == '81903', "Depth, samplesite", CO_adjusted$CharacteristicName)


##samplesite depth in meters 
CO_adjusted$CharacteristicName <- ifelse(CO_adjusted$CharacteristicName == "Depth" & CO_adjusted$USGSPCode == '82903', "Depth, samplesite", CO_adjusted$CharacteristicName)


#max lakedepth in feet
CO_adjusted$ResultMeasureValue <- ifelse(CO_adjusted$CharacteristicName=='Depth' & CO_adjusted$USGSPCode == '82016', CO_adjusted$ResultMeasureValue*0.3048, CO_adjusted$ResultMeasureValue)

CO_adjusted$ResultMeasure.MeasureUnitCode <- ifelse(CO_adjusted$CharacteristicName=='Depth' & CO_adjusted$USGSPCode == '82016', "m", CO_adjusted$ResultMeasure.MeasureUnitCode)

CO_adjusted$CharacteristicName <- ifelse(CO_adjusted$CharacteristicName == "Depth" & CO_adjusted$USGSPCode == '82016', "Depth, maximum lake depth", CO_adjusted$CharacteristicName)



###Secchi####

##REMOVE DEPTH PARAMETER
CO_adjusted <- CO_adjusted[!(CO_adjusted$CharacteristicName == 'Depth'), ]
CO_adjusted <- CO_adjusted[!(CO_adjusted$CharacteristicName == 'Depth of pond or reservoir in feet'), ]


##unit change if needed
CO_adjusted$ResultMeasureValue <- ifelse(CO_adjusted$CharacteristicName=='Depth, Secchi disk depth' & CO_adjusted$USGSPCode == '77', 
                                         CO_adjusted$ResultMeasureValue*0.0254, CO_adjusted$ResultMeasureValue)

CO_adjusted$ResultMeasureValue <- ifelse(CO_adjusted$CharacteristicName=='Depth, Secchi disk depth' & CO_adjusted$USGSPCode == '49701', 
                                         CO_adjusted$ResultMeasureValue*0.3048, CO_adjusted$ResultMeasureValue)

CO_adjusted$ResultMeasure.MeasureUnitCode <- ifelse(CO_adjusted$CharacteristicName=='Depth, Secchi disk depth', "m", CO_adjusted$ResultMeasure.MeasureUnitCode)

CO_adjusted <- dt_convert_w_pcode('CharacteristicName','Depth, Secchi disk depth',0.0254,'77','m')
CO_adjusted <- dt_convert_w_pcode('CharacteristicName','Depth, Secchi disk depth',0.3048,'49701','m')


CO_adjusted$CharacteristicName <- ifelse(CO_adjusted$CharacteristicName == "Depth, Secchi disk depth", "Secchi", CO_adjusted$CharacteristicName)


##inorganic nitrogen
##DISSOLVED
CO_adjusted$ResultMeasureValue <- ifelse(CO_adjusted$CharacteristicName=='Inorganic nitrogen (nitrate and nitrite)' & CO_adjusted$USGSPCode == '631', 
                                         CO_adjusted$ResultMeasureValue*1000.0, CO_adjusted$ResultMeasureValue)

CO_adjusted$ResultMeasure.MeasureUnitCode <- ifelse(CO_adjusted$CharacteristicName=='Inorganic nitrogen (nitrate and nitrite)', "ug/L", CO_adjusted$ResultMeasure.MeasureUnitCode)

CO_adjusted <- dt_convert_w_pcode('CharacteristicName','Inorganic nitrogen (nitrate and nitrite)',1000,'631','ug/L')


CO_adjusted$CharacteristicName <- ifelse(CO_adjusted$CharacteristicName == "Inorganic nitrogen (nitrate and nitrite)", "Nitrogen, nitrite (NO2) + nitrate (NO3)",
                                         CO_adjusted$CharacteristicName)


#########Kjeldahl Nitrogen 
CO_adjusted$ResultMeasureValue <- ifelse(CO_adjusted$CharacteristicName=='Kjeldahl nitrogen' & CO_adjusted$USGSPCode == '625', CO_adjusted$ResultMeasureValue*1000,
                                         CO_adjusted$ResultMeasureValue)

CO_adjusted$ResultMeasure.MeasureUnitCode <- ifelse(CO_adjusted$CharacteristicName=='Kjeldahl nitrogen', "ug/L", CO_adjusted$ResultMeasure.MeasureUnitCode)

CO_adjusted <- dt_convert_w_pcode('CharacteristicName','Kjeldahl nitrogen',1000,'625','ug/L')

CO_adjusted$CharacteristicName <- ifelse(CO_adjusted$CharacteristicName == "Kjeldahl nitrogen", "Nitrogen, total Kjeldahl", CO_adjusted$CharacteristicName)


##Magnesium##
CO_adjusted$ResultMeasureValue <- ifelse(CO_adjusted$CharacteristicName=='Magnesium, dissolved' & CO_adjusted$USGSPCode == '91052', 
                                         CO_adjusted$ResultMeasureValue/1000, CO_adjusted$ResultMeasureValue)

CO_adjusted$ResultMeasure.MeasureUnitCode <- ifelse(CO_adjusted$CharacteristicName=='Magnesium, dissolved', "mg/L", CO_adjusted$ResultMeasure.MeasureUnitCode)

CO_adjusted <- dt_convert_w_pcode('CharacteristicName','Magnesium, dissolved',.001,'91052','mg/L')


##Mercury####
##Dont need to change names

#total
CO_adjusted$ResultMeasureValue <- ifelse(CO_adjusted$CharacteristicName=='Mercury, total' & CO_adjusted$USGSPCode == '71900', CO_adjusted$ResultMeasureValue*1000,
                                         CO_adjusted$ResultMeasureValue)

CO_adjusted$ResultMeasure.MeasureUnitCode <- ifelse(CO_adjusted$CharacteristicName=='Mercury, total', "ng/L", CO_adjusted$ResultMeasure.MeasureUnitCode)

###total dissolved
CO_adjusted$ResultMeasureValue <- ifelse(CO_adjusted$CharacteristicName=='Mercury, total dissolved' & CO_adjusted$USGSPCode == '71890', CO_adjusted$ResultMeasureValue*1000,
                                         CO_adjusted$ResultMeasureValue)

CO_adjusted$ResultMeasure.MeasureUnitCode <- ifelse(CO_adjusted$CharacteristicName=='Mercury, total dissolved', "ng/L", CO_adjusted$ResultMeasure.MeasureUnitCode)

CO_adjusted <- dt_convert_w_pcode('CharacteristicName','Mercury, total',1000,'71900','ng/L')
CO_adjusted <- dt_convert_w_pcode('CharacteristicName','Mercury, total dissolved',1000,'71890','ng/L')


##Nitrate
###Dissolved as NO3
CO_adjusted$ResultMeasureValue <- ifelse(CO_adjusted$CharacteristicName =='Nitrate' & CO_adjusted$USGSPCode == '71851', CO_adjusted$ResultMeasureValue*225.9,
                                         CO_adjusted$ResultMeasureValue)

CO_adjusted$ResultMeasure.MeasureUnitCode <- ifelse(CO_adjusted$CharacteristicName=='Nitrate' & CO_adjusted$USGSPCode == '71851', "ug/L", CO_adjusted$ResultMeasure.MeasureUnitCode)

CO_adjusted <- dt_convert_w_pcode('CharacteristicName','Nitrate',225.9,'71851','ug/L')


###Dissolved as N
CO_adjusted$ResultMeasureValue <- ifelse(CO_adjusted$CharacteristicName =='Nitrate' & CO_adjusted$USGSPCode == '618', CO_adjusted$ResultMeasureValue*1000,
                                         CO_adjusted$ResultMeasureValue)

CO_adjusted$ResultMeasure.MeasureUnitCode <- ifelse(CO_adjusted$CharacteristicName=='Nitrate' & CO_adjusted$USGSPCode == '618', "ug/L", CO_adjusted$ResultMeasure.MeasureUnitCode)

CO_adjusted <- dt_convert_w_pcode('CharacteristicName','Nitrate',1000,'618','ug/L')

CO_adjusted$CharacteristicName <- ifelse(CO_adjusted$CharacteristicName == "Nitrate", "Nitrogen, nitrite (NO2) + nitrate (NO3)", CO_adjusted$CharacteristicName) #REDUNDANT



###Organic Carbon##
##Just rename

CO_adjusted$ResultMeasure.MeasureUnitCode <- ifelse(CO_adjusted$CharacteristicName=='Organic carbon', "mg/L", CO_adjusted$ResultMeasure.MeasureUnitCode)

CO_adjusted$CharacteristicName <- ifelse(CO_adjusted$CharacteristicName == "Organic carbon", "Carbon, dissolved organic", CO_adjusted$CharacteristicName)



###Orthophosphate##

CO_adjusted$ResultMeasureValue <- ifelse(CO_adjusted$CharacteristicName =='Orthophosphate' & CO_adjusted$USGSPCode == '660', CO_adjusted$ResultMeasureValue*326.1,
                                         CO_adjusted$ResultMeasureValue)

CO_adjusted$ResultMeasureValue <- ifelse(CO_adjusted$CharacteristicName =='Orthophosphate' & CO_adjusted$USGSPCode == '671', CO_adjusted$ResultMeasureValue*1000,
                                         CO_adjusted$ResultMeasureValue)

CO_adjusted$ResultMeasure.MeasureUnitCode <- ifelse(CO_adjusted$CharacteristicName=='Orthophosphate', "ug/L", CO_adjusted$ResultMeasure.MeasureUnitCode)

CO_adjusted <- dt_convert_w_pcode('CharacteristicName','Orthophosphate',326.1,'660','ug/L')
CO_adjusted <- dt_convert_w_pcode('CharacteristicName','Orthophosphate',1000,'671','ug/L')

CO_adjusted$CharacteristicName <- ifelse(CO_adjusted$CharacteristicName == "Orthophosphate", "Phosphorus, soluable reactive orthophosphate", CO_adjusted$CharacteristicName)


##Oxygen##
##Just rename
CO_adjusted$CharacteristicName <- ifelse(CO_adjusted$CharacteristicName == "Oxygen", "Oxygen, dissolved", CO_adjusted$CharacteristicName)


##pH
CO_adjusted$CharacteristicName <- ifelse(CO_adjusted$CharacteristicName == "pH" & CO_adjusted$USGSPCode == '400', "pH, field or closed", CO_adjusted$CharacteristicName)

CO_adjusted$CharacteristicName <- ifelse(CO_adjusted$CharacteristicName == "pH" & CO_adjusted$USGSPCode == '403', "pH, equilibrated, lab", CO_adjusted$CharacteristicName)



######PHOSPHORUS

#AS P
CO_adjusted$ResultMeasureValue <- ifelse(CO_adjusted$CharacteristicName =='Phosphorus' & CO_adjusted$USGSPCode == '665', CO_adjusted$ResultMeasureValue*1000,
                                         CO_adjusted$ResultMeasureValue)
##AS PO4
CO_adjusted$ResultMeasureValue <- ifelse(CO_adjusted$CharacteristicName =='Phosphorus' & CO_adjusted$USGSPCode == '71886', CO_adjusted$ResultMeasureValue*326.1,
                                         CO_adjusted$ResultMeasureValue)


CO_adjusted$ResultMeasure.MeasureUnitCode <- ifelse(CO_adjusted$CharacteristicName=='Phosphorus', "ug/L", CO_adjusted$ResultMeasure.MeasureUnitCode)


CO_adjusted <- dt_convert_w_pcode('CharacteristicName','Phosphorus',1000,'665','ug/L')
CO_adjusted <- dt_convert_w_pcode('CharacteristicName','Phosphorus',326.1,'71886','ug/L')

CO_adjusted$CharacteristicName <- ifelse(CO_adjusted$CharacteristicName == "Phosphorus", "Phosphorus, total", CO_adjusted$CharacteristicName)



#####Selenium########
CO_adjusted$ResultMeasure.MeasureUnitCode <- ifelse(CO_adjusted$CharacteristicName=='Selenium, dissolved', "ug/L", CO_adjusted$ResultMeasure.MeasureUnitCode)
CO_adjusted$ResultMeasure.MeasureUnitCode <- ifelse(CO_adjusted$CharacteristicName=='Selenium, total', "ug/L", CO_adjusted$ResultMeasure.MeasureUnitCode)



#######Silica########
CO_adjusted$ResultMeasure.MeasureUnitCode <- ifelse(CO_adjusted$CharacteristicName=='Silica, dissolved', "mg/L", CO_adjusted$ResultMeasure.MeasureUnitCode)



###Specific Conductance##
##Name and unit change 

CO_adjusted$ResultMeasure.MeasureUnitCode <- ifelse(CO_adjusted$CharacteristicName=='Specific, conductance', "uS/cm", CO_adjusted$ResultMeasure.MeasureUnitCode)

CO_adjusted$CharacteristicName <- ifelse(CO_adjusted$CharacteristicName == "Specific conductance", "Conductivity", CO_adjusted$CharacteristicName)



##Sulfate
CO_adjusted$ResultMeasureValue <- ifelse(CO_adjusted$CharacteristicName =='Sulfate, dissolved' & CO_adjusted$USGSPCode == '91005', CO_adjusted$ResultMeasureValue/1000,
                                         CO_adjusted$ResultMeasureValue)
CO_adjusted$ResultMeasure.MeasureUnitCode <- ifelse(CO_adjusted$CharacteristicName=='Sulfate, dissolved', "mg/L", CO_adjusted$ResultMeasure.MeasureUnitCode)

CO_adjusted <- dt_convert_w_pcode('CharacteristicName','Sulfate, dissolved',.001,'91005','mg/L')

####Temperature, water######
CO_adjusted$ResultMeasure.MeasureUnitCode <- ifelse(CO_adjusted$CharacteristicName=='Temperature, water', "degC", CO_adjusted$ResultMeasure.MeasureUnitCode)

CO_adjusted$CharacteristicName <- ifelse(CO_adjusted$CharacteristicName == "Temperature, water", "Temperature", CO_adjusted$CharacteristicName)


#Total Suspended Solids
#Just name  and unit name change

CO_adjusted$ResultMeasure.MeasureUnitCode <- ifelse(CO_adjusted$CharacteristicName=='Total suspended solids', "mg/L", CO_adjusted$ResultMeasure.MeasureUnitCode)

CO_adjusted$CharacteristicName <- ifelse(CO_adjusted$CharacteristicName == "Total suspended solids", "Solids, total suspended", CO_adjusted$CharacteristicName)


#True Color
##Just rename 
CO_adjusted$CharacteristicName <- ifelse(CO_adjusted$CharacteristicName == "True color", "Color, true", CO_adjusted$CharacteristicName)



#Turbidity
#remove unwanted parameters
CO_adjusted <- CO_adjusted[!(CO_adjusted$USGSPCode == '63675'),]

CO_adjusted$ResultMeasure.MeasureUnitCode <- ifelse(CO_adjusted$CharacteristicName=='Turbidity', "NTU", CO_adjusted$ResultMeasure.MeasureUnitCode)


# 
# ###CEILING VALUE FILTER####
# 
# CO_adjusted <- CO_adjusted[!(CO_adjusted$CharacteristicName == "Secchi" & CO_adjusted$ResultMeasureValue >= 32), ]
# 
# CO_adjusted <- CO_adjusted[!(CO_adjusted$CharacteristicName == "Phosphorus, total" & CO_adjusted$ResultMeasureValue >= 10000), ]
# 
# CO_adjusted <- CO_adjusted[!(CO_adjusted$CharacteristicName == "Nitrogen, NH4" & CO_adjusted$ResultMeasureValue >= 40000), ]
# 
# CO_adjusted <- CO_adjusted[!(CO_adjusted$CharacteristicName == "Nitrogen, nitrite (NO2) + nitrate (NO3)" & CO_adjusted$ResultMeasureValue >= 40000), ]
# 
# CO_adjusted <- CO_adjusted[!(CO_adjusted$CharacteristicName == "Carbon, dissolved organic" & CO_adjusted$ResultMeasureValue >= 332), ]
# 
# CO_adjusted <- CO_adjusted[!(CO_adjusted$CharacteristicName == "Phosphorus, soluable reactive orthophosphate" & CO_adjusted$ResultMeasureValue >= 40000), ]
# 
# CO_adjusted <- CO_adjusted[!(CO_adjusted$CharacteristicName == "Nitrogen, total Kjeldahl" & CO_adjusted$ResultMeasureValue >= 40000), ]




##feet to meters
CO_adjusted$ActivityDepthHeightMeasure.MeasureValue <- ifelse(CO_adjusted$ActivityDepthHeightMeasure.MeasureUnitCode == "feet", CO_adjusted$ActivityDepthHeightMeasure.MeasureValue*0.3048, 
                                                              CO_adjusted$ActivityDepthHeightMeasure.MeasureValue)


CO_adjusted$ActivityDepthHeightMeasure.MeasureUnitCode <- ifelse(CO_adjusted$ActivityDepthHeightMeasure.MeasureUnitCode == 'feet', 'meters',
                                                                 CO_adjusted$ActivityDepthHeightMeasure.MeasureUnitCode)


##remove any na result na values 
CO_adjusted <- CO_adjusted[!is.na(CO_adjusted$ResultMeasureValue), ] 




##csv writing###

write.csv(CO_adjusted, "CO_USGS_data.csv", row.names=FALSE)


CO_sites <- whatWQPsites(statecode="CO")

CO_sites_updated <- subset(CO_sites, CO_sites$MonitoringLocationIdentifier %in% CO_adjusted$MonitoringLocationIdentifier)

write.csv(CO_sites_updated, "CO_USGS_sites.csv")





#--------------------------------CLEAN CO_CDPHE-----------------------------------------########
co_cdphe <- read.csv("C:\\Users\\addelany\\Dropbox\\CL_HUB_LIMNO\\LIMNO_METADATA\\A_CompletedMetadataFiles\\CO\\CO_CDPHE_v3a.csv", stringsAsFactors = FALSE, row.names = NULL)

CO_CDPHE_adjusted <- distinct(co_cdphe, Activity.Start.Date, Monitoring.Location.ID, Characteristic.Name, Result.Value, Result.Unit, .keep_all = TRUE)


CO_CDPHE_adjusted$source_parameter <- CO_CDPHE_adjusted$Characteristic.Name
CO_CDPHE_adjusted$source_unit <- CO_CDPHE_adjusted$Result.Unit 


dt_convert <- function(param_col,param_name,conversion,origin_unit,convert_unit){
  CO_CDPHE_adjusted$detectionlimit_legacy <- ifelse(CO_CDPHE_adjusted[,param_col]== param_name & CO_CDPHE_adjusted$detectionlimit_unit_legacy == origin_unit, 
                                                    CO_CDPHE_adjusted$detectionlimit_legacy*conversion, CO_CDPHE_adjusted$detectionlimit_legacy)
  CO_CDPHE_adjusted$detectionlimit_unit_legacy <- ifelse(CO_CDPHE_adjusted[,param_col]== param_name,convert_unit, CO_CDPHE_adjusted$detectionlimit_unit_legacy)
  
  return(CO_CDPHE_adjusted)
}

CO_CDPHE_adjusted$detectionlimit_legacy <- CO_CDPHE_adjusted$Detection.Limit.Value1
CO_CDPHE_adjusted$detectionlimit_unit_legacy <- CO_CDPHE_adjusted$Detection.Limit.Unit1


##Alkalinity
CO_CDPHE_adjusted$Result.Value <- ifelse(CO_CDPHE_adjusted$Characteristic.Name=='Alkalinity, total', CO_CDPHE_adjusted$Result.Value*19.98, CO_CDPHE_adjusted$Result.Value)

CO_CDPHE_adjusted <- dt_convert('Characteristic.Name','Alkalinity, total',19.98,'mg/l','ueq/L')

CO_CDPHE_adjusted$Result.Unit <- ifelse(CO_CDPHE_adjusted$Characteristic.Name=='Alkalinity, total', "ueq/L", CO_CDPHE_adjusted$Result.Unit)

CO_CDPHE_adjusted$Characteristic.Name <- ifelse(CO_CDPHE_adjusted$Characteristic.Name == "Alkalinity, total", "Alkalinity", CO_CDPHE_adjusted$Characteristic.Name)



##Aluminum
#total
CO_CDPHE_adjusted <- CO_CDPHE_adjusted %>% filter(!(Characteristic.Name == 'Aluminum, total')) ##drop this for now, methods issue needs to be investigated

# CO_CDPHE_adjusted <- CO_CDPHE_adjusted %>% filter(!(Characteristic.Name == 'Aluminum, total' & Analytical.Method.Name == 'Ammonia Nitrogen by Colorimetry'))
# 
# CO_CDPHE_adjusted$Result.Value <- ifelse(CO_CDPHE_adjusted$Characteristic.Name=='Aluminum, total', CO_CDPHE_adjusted$Result.Value*1000, CO_CDPHE_adjusted$Result.Value)
# CO_CDPHE_adjusted$Result.Unit <- ifelse(CO_CDPHE_adjusted$Characteristic.Name=='Aluminum, total', "ug/L", CO_CDPHE_adjusted$Result.Unit)
# CO_CDPHE_adjusted <- dt_convert('Characteristic.Name','Aluminum, total',1000,'mg/l','ug/L')

#dissolved
CO_CDPHE_adjusted$Result.Unit <- ifelse(CO_CDPHE_adjusted$Characteristic.Name=='Aluminum, dissolved', "ug/L", CO_CDPHE_adjusted$Result.Unit)
CO_CDPHE_adjusted <- dt_convert('Characteristic.Name','Aluminum, dissolved',1000,'mg/l','ug/L')



##Arsenic
#total
CO_CDPHE_adjusted$Result.Unit <- ifelse(CO_CDPHE_adjusted$Characteristic.Name=='Arsenic, total', "ug/L", CO_CDPHE_adjusted$Result.Unit)
CO_CDPHE_adjusted <- dt_convert('Characteristic.Name','Arsenic, total',1000,'mg/l','ug/L')

#dissolved
CO_CDPHE_adjusted$Result.Unit <- ifelse(CO_CDPHE_adjusted$Characteristic.Name=='Arsenic, dissolved', "ug/L", CO_CDPHE_adjusted$Result.Unit)
CO_CDPHE_adjusted <- dt_convert('Characteristic.Name','Arsenic, total',1000,'mg/l','ug/L')



##Calcium
CO_CDPHE_adjusted$Result.Value <- ifelse(CO_CDPHE_adjusted$Characteristic.Name=='Calcium', CO_CDPHE_adjusted$Result.Value/1000, CO_CDPHE_adjusted$Result.Value)

CO_CDPHE_adjusted <- dt_convert('Characteristic.Name','Calcium',.001,'ug/l','mg/L')

CO_CDPHE_adjusted$Result.Unit <- ifelse(CO_CDPHE_adjusted$Characteristic.Name=='Calcium', "mg/L", CO_CDPHE_adjusted$Result.Unit)

CO_CDPHE_adjusted$Characteristic.Name <- ifelse(CO_CDPHE_adjusted$Characteristic.Name == "Calcium", "Calcium, dissolved", CO_CDPHE_adjusted$Characteristic.Name)


##Chloride
CO_CDPHE_adjusted$Result.Unit <- ifelse(CO_CDPHE_adjusted$Characteristic.Name=='Chloride', "mg/L", CO_CDPHE_adjusted$Result.Unit)
CO_CDPHE_adjusted$Characteristic.Name <- ifelse(CO_CDPHE_adjusted$Characteristic.Name == "Chloride", "Chloride, dissolved", CO_CDPHE_adjusted$Characteristic.Name)


##DO
CO_CDPHE_adjusted$Result.Unit <- ifelse(CO_CDPHE_adjusted$Characteristic.Name=='Dissolved Oxygen (DO)', "mg/L", CO_CDPHE_adjusted$Result.Unit)

CO_CDPHE_adjusted$Characteristic.Name <- ifelse(CO_CDPHE_adjusted$Characteristic.Name == "Dissolved Oxygen (DO)", "Oxygen, dissolved", CO_CDPHE_adjusted$Characteristic.Name)


##Inorganic Nitrogen
CO_CDPHE_adjusted$Result.Value <- ifelse(CO_CDPHE_adjusted$Characteristic.Name=='Inorganic nitrogen (nitrate and nitrite)', CO_CDPHE_adjusted$Result.Value*1000, CO_CDPHE_adjusted$Result.Value)

CO_CDPHE_adjusted <- dt_convert('Characteristic.Name','Inorganic nitrogen (nitrate and nitrite)',1000,'mg/l','ug/L')

CO_CDPHE_adjusted$Result.Unit <- ifelse(CO_CDPHE_adjusted$Characteristic.Name=='Inorganic nitrogen (nitrate and nitrite)', "ug/L", CO_CDPHE_adjusted$Result.Unit)

CO_CDPHE_adjusted$Characteristic.Name <- ifelse(CO_CDPHE_adjusted$Characteristic.Name == "Inorganic nitrogen (nitrate and nitrite)", "Nitrogen, nitrite (NO2) + nitrate (NO3)", CO_CDPHE_adjusted$Characteristic.Name)


##Magnesium
CO_CDPHE_adjusted$Result.Value <- ifelse(CO_CDPHE_adjusted$Characteristic.Name=='Magnesium', CO_CDPHE_adjusted$Result.Value/1000, CO_CDPHE_adjusted$Result.Value)

CO_CDPHE_adjusted <- dt_convert('Characteristic.Name','Magnesium',.001,'ug/l','mg/L')

CO_CDPHE_adjusted$Result.Unit <- ifelse(CO_CDPHE_adjusted$Characteristic.Name=='Magnesium', "mg/L", CO_CDPHE_adjusted$Result.Unit)

CO_CDPHE_adjusted$Characteristic.Name <- ifelse(CO_CDPHE_adjusted$Characteristic.Name == "Magnesium", "Magnesium, dissolved", CO_CDPHE_adjusted$Characteristic.Name)


###Nitrate
CO_CDPHE_adjusted <- CO_CDPHE_adjusted %>% filter(Characteristic.Name != 'Nitrate')

# CO_CDPHE_adjusted$Result.Value <- ifelse(CO_CDPHE_adjusted$Characteristic.Name=='Nitrate', CO_CDPHE_adjusted$Result.Value*1000, CO_CDPHE_adjusted$Result.Value)
# 
# CO_CDPHE_adjusted <- dt_convert('Characteristic.Name','Nitrate',1000,'mg/l','ug/L')
# 
# CO_CDPHE_adjusted$Result.Unit <- ifelse(CO_CDPHE_adjusted$Characteristic.Name=='Nitrate', "ug/L", CO_CDPHE_adjusted$Result.Unit)
# 
# CO_CDPHE_adjusted$Characteristic.Name <- ifelse(CO_CDPHE_adjusted$Characteristic.Name == "Nitrate", "Nitrogen, nitrite (NO2) + nitrate (NO3)", CO_CDPHE_adjusted$Characteristic.Name)


##pH
CO_CDPHE_adjusted$Characteristic.Name <- ifelse(CO_CDPHE_adjusted$Characteristic.Name == "pH", "pH, field or closed", CO_CDPHE_adjusted$Characteristic.Name)


##Phosphorus, total
CO_CDPHE_adjusted$Result.Value <- ifelse(CO_CDPHE_adjusted$Characteristic.Name=='Phosphorus, total', CO_CDPHE_adjusted$Result.Value*1000, CO_CDPHE_adjusted$Result.Value)

CO_CDPHE_adjusted <- dt_convert('Characteristic.Name','Phosphorus, total',1000,'mg/l','ug/L')

CO_CDPHE_adjusted$Result.Unit <- ifelse(CO_CDPHE_adjusted$Characteristic.Name=='Phosphorus, total', "ug/L", CO_CDPHE_adjusted$Result.Unit)


##Selenium
#total
CO_CDPHE_adjusted$Result.Unit <- ifelse(CO_CDPHE_adjusted$Characteristic.Name=='Selenium, total', "ug/L", CO_CDPHE_adjusted$Result.Unit)

#dissolved
CO_CDPHE_adjusted$Result.Unit <- ifelse(CO_CDPHE_adjusted$Characteristic.Name=='Selenium, dissolved', "ug/L", CO_CDPHE_adjusted$Result.Unit)



####Sulfate
CO_CDPHE_adjusted$Result.Unit <- ifelse(CO_CDPHE_adjusted$Characteristic.Name=='Sulfate', "mg/L", CO_CDPHE_adjusted$Result.Unit)

CO_CDPHE_adjusted$Characteristic.Name <- ifelse(CO_CDPHE_adjusted$Characteristic.Name == "Sulfate", "Sulfate, dissolved", CO_CDPHE_adjusted$Characteristic.Name)


##Temperature, sample
CO_CDPHE_adjusted$Result.Unit <- ifelse(CO_CDPHE_adjusted$Characteristic.Name=='Temperature, sample', "degC", CO_CDPHE_adjusted$Result.Unit)

CO_CDPHE_adjusted$Characteristic.Name <- ifelse(CO_CDPHE_adjusted$Characteristic.Name == "Temperature, sample", "Temperature", CO_CDPHE_adjusted$Characteristic.Name)


##Suspended solids
CO_CDPHE_adjusted$Result.Unit <- ifelse(CO_CDPHE_adjusted$Characteristic.Name=='Total suspended solids', "mg/L", CO_CDPHE_adjusted$Result.Unit)

CO_CDPHE_adjusted$Characteristic.Name <- ifelse(CO_CDPHE_adjusted$Characteristic.Name == "Total suspended solids", "Solids, total suspended", CO_CDPHE_adjusted$Characteristic.Name)





##remove any na result na values 
CO_CDPHE_adjusted <- CO_CDPHE_adjusted[!is.na(CO_CDPHE_adjusted$Result.Value), ] 


##csv writing###

write.csv(CO_CDPHE_adjusted, "CO_CDPHE_v3a_data_cleaned.csv", row.names=FALSE)



###############################################CLEAN CO_CDPHE PROFILE DATA###############################
library(reshape2)
library(dplyr)
library(tidyr)



co_cdphe_profile <- read.csv("C:\\Users\\addelany\\Dropbox\\CL_HUB_LIMNO\\LIMNO_METADATA\\A_CompletedMetadataFiles\\CO\\CO_CDPHE_v3b.csv", stringsAsFactors = FALSE, row.names = NULL)

vert_profile_data <- gather(co_cdphe_profile, "CharacteristicName", "ResultMeasureValue", c(4,9,10,11,12))


CO_CDPHE_profile <- distinct(vert_profile_data, STATIONID4, DATE, CharacteristicName, ResultMeasureValue, .keep_all = TRUE)

CO_CDPHE_profile$source_parameter <- CO_CDPHE_profile$CharacteristicName

##SECCHI

#first remove any sampledepts associated with secchi records
CO_CDPHE_profile$DEPMETER <- ifelse(CO_CDPHE_profile$CharacteristicName == 'SECCHI' & is.na(CO_CDPHE_profile$DEPMETER) == FALSE, NA, CO_CDPHE_profile$DEPMETER)

CO_CDPHE_profile$CharacteristicName <- ifelse(CO_CDPHE_profile$CharacteristicName == "SECCHI", "Secchi", CO_CDPHE_profile$CharacteristicName)


##TEMP_C
CO_CDPHE_profile$CharacteristicName <- ifelse(CO_CDPHE_profile$CharacteristicName == "TEMP_C", "Temperature", CO_CDPHE_profile$CharacteristicName)


#Conductivity
CO_CDPHE_profile$CharacteristicName <- ifelse(CO_CDPHE_profile$CharacteristicName == "SPCON_US", "Conductivity", CO_CDPHE_profile$CharacteristicName)


##PH
CO_CDPHE_profile$CharacteristicName <- ifelse(CO_CDPHE_profile$CharacteristicName == "PH", "pH, field or closed", CO_CDPHE_profile$CharacteristicName)


##RDO_MGL
CO_CDPHE_profile$CharacteristicName <- ifelse(CO_CDPHE_profile$CharacteristicName == "RDO_MGL", "Oxygen, dissolved", CO_CDPHE_profile$CharacteristicName)




##remove any na result na values 
CO_CDPHE_profile <- CO_CDPHE_profile[!is.na(CO_CDPHE_profile$ResultMeasureValue), ] 


##csv writing###

write.csv(CO_CDPHE_profile, "CO_CDPHE_v3b_profile_data_cleaned.csv", row.names=FALSE)



###CLEAN EXTRA DATA (VERSION 3C)
co_cdphe_wqx <- read.csv("C:\\Users\\addelany\\Dropbox\\CL_HUB_LIMNO\\LIMNO_METADATA\\A_CompletedMetadataFiles\\CO\\CO_CDPHE_v3c.csv", stringsAsFactors = FALSE, row.names = NULL)

CO_CDPHE_wqx <- distinct(co_cdphe_wqx,StationID, Date, Depth..m., Characteristic.Name, Res.Val, Res.Unit, .keep_all = TRUE)

CO_CDPHE_wqx$source_parameter <- CO_CDPHE_wqx$Characteristic.Name

####Chlorophyll a
CO_CDPHE_wqx$Res.Unit <- ifelse(CO_CDPHE_wqx$Characteristic.Name=='Chlorophyll-a', "ug/L", CO_CDPHE_wqx$Res.Unit)

CO_CDPHE_wqx$Characteristic.Name <- ifelse(CO_CDPHE_wqx$Characteristic.Name == "Chlorophyll-a", "Chlorophyll a", CO_CDPHE_wqx$Characteristic.Name)


##Coductivity
##nothing to do (correct units and name)

##DOC
CO_CDPHE_wqx$Characteristic.Name <- ifelse(CO_CDPHE_wqx$Characteristic.Name == "DOC**", "Carbon, dissolved organic", CO_CDPHE_wqx$Characteristic.Name)


##E coli
###assuming that #/100mL is ~ CFU/100mL
CO_CDPHE_wqx$Res.Unit <- ifelse(CO_CDPHE_wqx$Characteristic.Name=='e coli', "cfu/100mL", CO_CDPHE_wqx$Res.Unit)

CO_CDPHE_wqx$Characteristic.Name <- ifelse(CO_CDPHE_wqx$Characteristic.Name == "e coli", "E. coli-CFU", CO_CDPHE_wqx$Characteristic.Name)


##Mercury
CO_CDPHE_wqx$Res.Unit <- ifelse(CO_CDPHE_wqx$Characteristic.Name=='Mercury', "ug/L", CO_CDPHE_wqx$Res.Unit)

CO_CDPHE_wqx$Characteristic.Name <- ifelse(CO_CDPHE_wqx$Characteristic.Name == "Mercury", "Mercury, total dissolved", CO_CDPHE_wqx$Characteristic.Name)


##Secchi
#nothing to do (unit and name are already correct)

##Specific conductance
CO_CDPHE_wqx$Characteristic.Name <- ifelse(CO_CDPHE_wqx$Characteristic.Name == "Specific conductance", "Conductivity", CO_CDPHE_wqx$Characteristic.Name)




##remove any na result na values 
CO_CDPHE_wqx <- CO_CDPHE_wqx[!is.na(CO_CDPHE_wqx$Res.Val), ] 


##csv writing###

write.csv(CO_CDPHE_wqx, "CO_CDPHE_v3c_data_cleaned.csv", row.names=FALSE)