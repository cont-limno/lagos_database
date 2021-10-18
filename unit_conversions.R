#convert to numeric once done
data_temp<-data_temp %>% mutate(ResultMeasureValue = as.numeric(ResultMeasureValue)) # non numeric values become NA
#check detection limits
unique(data_temp$DetectionQuantitationLimitMeasure.MeasureValue[is.na(as.numeric(data_temp$DetectionQuantitationLimitMeasure.MeasureValue))])
#no non NA values, convert to numeric
data_temp<-data_temp %>% mutate(DetectionQuantitationLimitMeasure.MeasureValue = as.numeric(ResultMeasureValue)) # non numeric values become NA
#remaining values don't contain enough info to proceed so convert to numeric
temp1 <- data_temp %>% filter(!is.na(ResultMeasureValue))
temp2 <- data_temp %>% filter(is.na(ResultMeasureValue)) %>% 
    filter(!is.na(DetectionQuantitationLimitMeasure.MeasureValue))
data_temp <- rbind(temp1,temp2)
rm(temp1,temp2)

methods_table<-data_temp %>% 
    select(CharacteristicName,
           ResultMeasure.MeasureUnitCode,
           Method_Id,
           ResultAnalyticalMethod.MethodName,
           DetectionQuantitationLimitMeasure.MeasureUnitCode,
           USGSPCode) %>% 
    distinct()

if(file.exists(paste0("conversion_files/",abbrev,"_Methods_Conversion.csv"))==FALSE) {
    write_csv(methods_table,paste0("conversion_files/",abbrev,"_Methods.csv"))
    stop("Conversion File needs to be created")
    
}

conversions <- read.csv(file = paste0("conversion_files/",abbrev,"_Methods_Conversion.csv")) # read in list of conversion factors
conversions[conversions == ""] <- NA #assign NA to blank values
check_methods<-merge(conversions,methods_table, by=c("CharacteristicName",
                                                     "ResultMeasure.MeasureUnitCode",
                                                     "DetectionQuantitationLimitMeasure.MeasureUnitCode",
                                                     "Method_Id",
                                                     "ResultAnalyticalMethod.MethodName", 
                                                     "USGSPCode"),all = T) #merge methods with conversion table to identify methods without a conversion factor
#write file if conversion factors need to be added
if(nrow(conversions)!=nrow(check_methods)) {
    write.csv(check_methods, file = paste0("conversion_files/",abbrev,"_Methods_Conversion.csv"), row.names=FALSE, na="")
}

if(is.na(min(check_methods$Conversion))) {stop("FIX Conversions and reload files")}

data.size <- nrow(data_temp) #check to ensure data frame stays same size after merge
data_temp<-merge(data_temp,conversions,by=c("CharacteristicName",
                                            "ResultMeasure.MeasureUnitCode",
                                            "DetectionQuantitationLimitMeasure.MeasureUnitCode",
                                            "Method_Id",
                                            "ResultAnalyticalMethod.MethodName", 
                                            "USGSPCode"),all.x = T) #Assign conversion factors using unique characteristic and method identifiers 
if((nrow(data_temp) - data.size)!=0) stop("error in merge") #should be zero!

data_temp <- data_temp %>% 
    mutate(Converted = ResultMeasureValue*Conversion) %>% 
    mutate(Converted_dl = DetectionQuantitationLimitMeasure.MeasureValue*Conversion_dl) 


data_temp_temp <- data_temp %>% 
    filter(!is.na(ResultMeasureValue)) %>% 
    filter(is.na(Converted)) %>% 
    select(CharacteristicName,
           ResultMeasureValue,
           ResultMeasure.MeasureUnitCode,
           Conversion,
           DetectionQuantitationLimitMeasure.MeasureUnitCode,
           DetectionQuantitationLimitMeasure.MeasureValue,
           Conversion_dl,
           Method_Id,
           ResultAnalyticalMethod.MethodName,
           USGSPCode)
if(nrow(data_temp_temp)>0) stop("not all results converted")
#should be zero rows if everything converted right

data_temp_dl_temp <- data_temp %>% 
    filter(!is.na(DetectionQuantitationLimitMeasure.MeasureValue)) %>% 
    filter(is.na(Converted_dl)) %>% 
    select(CharacteristicName,
           ResultMeasureValue,
           ResultMeasure.MeasureUnitCode,
           Conversion,
           DetectionQuantitationLimitMeasure.MeasureUnitCode,
           DetectionQuantitationLimitMeasure.MeasureValue,
           Conversion_dl,
           Method_Id,
           ResultAnalyticalMethod.MethodName,
           USGSPCode)
if(nrow(data_temp_dl_temp)>0) stop("not all detection limits converted")

#should be zero rows if everything converted right


#Check flagged methods

data_temp_temp_notes <- conversions %>% 
    filter(!is.na(Notes))

data_temp<- data_temp %>% filter(Conversion!=-1e6) %>% 
    filter(Conversion_dl != -1e6)

rm(data_temp_temp)
rm(conversions)
rm(check_methods)
rm(methods_table)
rm(temp)
rm(data.size)
rm(data_temp_temp_notes)
rm(data_temp_dl_temp)

data_temp$ResultMeasure.MeasureUnitCode[!is.na(data_temp$ResultMeasure.MeasureUnitCode)] <- lagos_unit
data_temp$DetectionQuantitationLimitMeasure.MeasureUnitCode[!is.na(data_temp$DetectionQuantitationLimitMeasure.MeasureUnitCode)] <- lagos_unit

unique(data_temp$ResultMeasure.MeasureUnitCode) 
unique(data_temp$DetectionQuantitationLimitMeasure.MeasureUnitCode) 

data_temp$CharacteristicName<-lagos_name  #set param name

assign(paste0("data_",dataset_name),data_temp)
# data <- data %>% filter(Obs_Id %notin% data_temp$Obs_Id)
rm(data_temp)
