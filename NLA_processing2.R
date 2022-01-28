library(dplyr)
library(data.table)

data<-read.csv("PreFinal_NLA_data.csv")
table<-read.csv("./NLA/lagos_variable.csv")
conversions<-read.csv("./NLA/conversions.csv")
unique((data$source_parameter))

data$lagos_variablename[which(data$source_parameter == "ANC")] <- "Alkalinity"
data$lagos_variableid[which(data$lagos_variablename == "Alkalinity")] <- 1

data$lagos_variablename[which(data$source_parameter == "COND_FIELD"|
                              data$source_parameter == "CONDUCTIVITY"|
                              data$source_parameter == "COND")] <- "Conductivity"
data$lagos_variableid[which(data$lagos_variablename == "Conductivity")] <- 13

data$lagos_variablename[which(data$source_parameter == "OXYGEN"|
                              data$source_parameter == "DO_FIELD")] <- "Oxygen, dissolved"
data$lagos_variableid[which(data$lagos_variablename == "Oxygen, dissolved")] <- 23

data$lagos_variablename[which(data$source_parameter == "POTASSIUM")] <- "Potassium"
data$lagos_variableid[which(data$lagos_variablename == "Potassium")] <- 29

data$lagos_variablename[which(data$source_parameter == "SODIUM")] <- "Sodium"
data$lagos_variableid[which(data$lagos_variablename == "Sodium")] <- 32

data$lagos_variablename[which(data$source_parameter == "TSS")] <- "Solids, total suspended"
data$lagos_variableid[which(data$lagos_variablename == "Solids, total suspended")] <- 33

data$lagos_variablename[which(data$source_parameter == "TEMP_FIELD"|
                              data$source_parameter == "TEMPERATURE")] <- "Temperature"
data$lagos_variableid[which(data$lagos_variablename == "Temperature")] <- 35

data$lagos_variablename[which(data$source_parameter == "TURB")] <- "Turbidity"
data$lagos_variableid[which(data$lagos_variablename == "Turbidity")] <- 36

data$lagos_variablename[which(data$source_parameter == "TOC")] <- "Carbon, total organic"
data$lagos_variableid[which(data$lagos_variablename == "Carbon, total organic")] <- 7

data$lagos_variablename[which(data$source_parameter == "CHLA"|
                                data$source_parameter == "CHLX")] <- "Chlorophyll a"
data$lagos_variableid[which(data$lagos_variablename == "Chlorophyll a")] <- 9

data$lagos_variablename[which(data$source_parameter == "COLOR")] <- "Color, true"
data$lagos_variableid[which(data$lagos_variablename == "Color, true")] <- 12

data$lagos_variablename[which(data$source_parameter == "TKN")] <- "Nitrogen, total Kjeldahl"
data$lagos_variableid[which(data$lagos_variablename == "Nitrogen, total Kjeldahl")] <- 16

data$lagos_variablename[which(data$source_parameter == "NITRITE_N")] <- "Nitrogen, nitrite (NO2)"
data$lagos_variableid[which(data$lagos_variablename == "Nitrogen, nitrite (NO2)")] <- 17

data$lagos_variablename[which(data$source_parameter == "NITRATE_NITRITE_N")] <- "Nitrogen, nitrite (NO2) + nitrate (NO3)"
data$lagos_variableid[which(data$lagos_variablename == "Nitrogen, nitrite (NO2) + nitrate (NO3)")] <- 18

data$lagos_variablename[which(data$source_parameter == "AMMONIA_N"|
                          data$source_parameter == "AMMONIA")] <- "Nitrogen, NH4"
data$lagos_variableid[which(data$lagos_variablename == "Nitrogen, NH4")] <- 19

data$lagos_variablename[which(data$source_parameter == "NTL")] <- "Nitrogen, total"
data$lagos_variableid[which(data$lagos_variablename == "Nitrogen, total")] <- 21

data$lagos_variablename[which(data$source_parameter == "PTL")] <- "Phosphorus, total"
data$lagos_variableid[which(data$lagos_variablename == "Phosphorus, total")] <- 27


data$lagos_variablename[which(data$source_parameter == "SECMEAN"|
                              data$source_parameter == "SECCHI")] <- "Secchi"
data$lagos_variableid[which(data$lagos_variablename == "Secchi")] <- 30

data$lagos_variablename[which(data$source_parameter == "DOC")] <- "Carbon, dissolved organic"
data$lagos_variableid[which(data$lagos_variablename == "Carbon, dissolved organic")] <- 6

data$lagos_variablename[which(data$source_parameter == "MICX"|
                                data$source_parameter == "MICZ")] <- "Microcystin"
data$lagos_variableid[which(data$lagos_variablename == "Microcystin")] <- 37

data$lagos_variablename[which(data$source_parameter == "ATRAZINE")] <- "Atrazine, dissolved"
data$lagos_variableid[which(data$lagos_variablename == "Atrazine, dissolved")] <- 39

data$lagos_variablename[which(data$source_parameter == "ALUMINUM")] <- "Aluminum, dissolved"
data$lagos_variableid[which(data$lagos_variablename == "Aluminum, dissolved")] <- 43

data$lagos_variablename[which(data$source_parameter == "PH"|
                                data$source_parameter == "PH_FIELD")] <- "pH, field or closed"
data$lagos_variableid[which(data$lagos_variablename == "pH, field or closed")] <- 25

data$lagos_variablename[which(data$source_parameter == "SULFATE")] <- "Sulfate, dissolved"
data$lagos_variableid[which(data$lagos_variablename == "Sulfate, dissolved")] <- 34

data$lagos_variablename[which(data$source_parameter == "CHLORIDE")] <- "Chloride, dissolved"
data$lagos_variableid[which(data$lagos_variablename == "Chloride, dissolved")] <- 8

data$lagos_variablename[which(data$source_parameter == "SILICA")] <- "Silica, dissolved"
data$lagos_variableid[which(data$lagos_variablename == "Silica, dissolved")] <- 31

data$lagos_variablename[which(data$source_parameter == "CALCIUM")] <- "Calcium, dissolved"
data$lagos_variableid[which(data$lagos_variablename == "Calcium, dissolved")] <- 3

data$lagos_variablename[which(data$source_parameter == "MAGNESIUM")] <- "Magnesium, dissolved"
data$lagos_variableid[which(data$lagos_variablename == "Magnesium, dissolved")] <- 14

data$lagos_variablename[which(data$source_parameter == "E_COLI")] <- "E.coli-MPN"
data$lagos_variableid[which(data$lagos_variablename == "E.coli-MPN")] <- 51


clean_data<-subset(data, source_parameter != 'BATCH_ID')
clean_data<-subset(clean_data, source_parameter != 'CYLSPER')
clean_data<-subset(clean_data, source_parameter != 'NITRATE_N')
clean_data<-subset(clean_data, source_parameter != '')

unique((clean_data$source_parameter))
unique((clean_data$lagos_variablename))
unique((clean_data$lagos_variableid))


merged_data<-merge(clean_data,table,by="lagos_variablename",all.x=T)
merged_data$datavalue_unit<-merged_data$unitsabbreviation
merged_data$lagos_variableid<-merged_data$variableid_lagos


convert<-merge(merged_data,conversions,by=c("lagos_variablename","datavalue_unit","source_unit"), all.x=T)
convert$datavalue_conversion<-as.numeric(convert$Conversion_factor)
convert$datavalue<-c(as.numeric(convert$source_value)*convert$datavalue_conversion)

convert$detectionlimitvalue_conversion<-as.numeric(convert$Conversion_factor)
convert$detectionlimit_value<-c(as.numeric(convert$source_detectionlimit_value)*convert$detectionlimitvalue_conversion)

convert$lagos_sampletype<-convert$source_sampletype
convert$lagos_sampledepth<-convert$source_sampledepth

convert$source_sampleposition<-ifelse(is.na(convert$source_sampledepth),"UNKNOWN","SPECIFIED")
convert$lagos_sampleposition<-convert$source_sampleposition

convert$source_samplesiteid<-convert$SITE_ID
convert$source_activityid<-convert$UID

convert<-convert[complete.cases(convert$source_sampledepth)>-0.00001,]
convert$source_methodqualifier<-ifelse(convert$lagos_variablename =="Chlorophyll a",
                                       "unknown", NA)




final_check<- convert[, c('SITE_ID','NAMES','LAT_DD','LON_DD','STATE','CNTYNAME',
                                    'valueid','obs_id','lagoslakeid', 'sampledate','lagos_variableid',
                                    'lagos_variablename', 'datavalue', 'datavalue_unit', 'detectionlimit_value',
                                    'datavalue_conversion', 'detectionlimitvalue_conversion', 'lagos_comments',
                                    'lagos_sampledepth', 'lagos_sampleposition','lagos_sampletype', 'organization_id',
                                    'organization_name','source_activityid', 'source_comments', 'source_detectionlimit_value',
                                    'source_labmethoddescription', 'source_labmethodid', 'source_labmethodname', 'source_parameter',
                                    'source_sampledepth', 'source_sampleposition', 'source_samplesiteid', 'source_sampletype',
                                    'source_unit','source_value',  'source_methodqualifier')]


write.csv(final_check,"Final_NLA_data.csv")


final_check <- data.frame(lapply(final_check, as.character), stringsAsFactors=FALSE)


final_check$obs_id<-toString(final_check$obs_id)
final_check$lagoslakeid<-as.integer(final_check$lagoslakeid)
final_check$sampledate<-as.Date(final_check$sampledate,format='%m/%d/%Y')
final_check$lagos_variableid<-as.integer(final_check$lagos_variableid)
final_check$lagos_variablename<-toString(final_check$lagos_variablename)
final_check$datavalue<-as.numeric(final_check$datavalue)
final_check$datavalue_unit<-toString(final_check$datavalue_unit)
final_check$detectionlimit_value<-as.numeric(final_check$detectionlimit_value)
final_check$datavalue_conversion<-as.numeric(final_check$datavalue_conversion)
final_check$detectionlimitvalue_conversion<-as.numeric(final_check$detectionlimitvalue_conversion)
final_check$lagos_comments<-toString(final_check$lagos_comments)
final_check$lagos_sampledepth<-as.numeric(final_check$lagos_sampledepth)
final_check$lagos_sampleposition<-toString(final_check$lagos_sampleposition)
final_check$lagos_sampletype<-toString(final_check$lagos_sampletype)
final_check$organization_id<-toString(final_check$organization_id)
final_check$organization_name<-toString(final_check$organization_name)
final_check$source_Activityid<-toString(final_check$source_activityid)
final_check$source_comments<-toString(final_check$source_comments)
final_check$source_detectionlimit_value<-toString(final_check$source_detectionlimit_value)
final_check$source_labmethoddescription<-toString(final_check$source_labmethoddescription)
final_check$source_labmethodid<-toString(final_check$source_labmethodid)
final_check$source_labmethodname<-toString(final_check$source_labmethodname)
final_check$source_parameter<-toString(final_check$source_parameter)
final_check$source_sampledepth<-as.numeric(final_check$source_sampledepth)
final_check$source_sampleposition<-toString(final_check$source_sampleposition)
final_check$source_samplesiteid<-toString(final_check$source_samplesiteid)
final_check$source_sampletype<-toString(final_check$source_sampletype)
final_check$source_unit<-toString(final_check$source_unit)
final_check$source_value<-toString(final_check$source_value)
final_check$source_methodqualifier<-toString(final_check$source_methodqualifier)




