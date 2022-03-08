
#https://github.com/NEONScience/NEON-water-chemistry-grab-samples/blob/main/linkingSummaryDataScrip/linkSummaryDataToLabData.R

waterChemList <-
  neonUtilities::loadByProduct(
    dpID = "DP1.20093.001",
    site = "CRAM",
    package = 'expanded',
    check.size = FALSE
  )

externalLabData <- waterChemList$swc_externalLabDataByAnalyte
externalSummaryData <- waterChemList$swc_externalLabSummaryData
externalSummaryData$labSpecificEndDate[is.na(externalSummaryData$labSpecificEndDate)] <-
  as.POSIXct(Sys.Date())

#Check that the analytes match
externalLabData$analyte[!externalLabData$analyte %in% externalSummaryData$analyte]

#Link the data by analyte
namesToAdd <-
  names(externalSummaryData)[!(names(externalSummaryData) %in% names(externalLabData))]
namesToAdd <- namesToAdd[!(namesToAdd %in% c("endDate"))]
externalLabData[, namesToAdd] <- NA
for (i in 1:nrow(externalSummaryData)) {
  currStartDate <- externalSummaryData$labSpecificStartDate[i]
  currEndDate <- externalSummaryData$labSpecificEndDate[i]
  currAnalyte <- externalSummaryData$analyte[i]
  currLab <- externalSummaryData$laboratoryName[i]
  externalLabData[(
    externalLabData$startDate >= currStartDate &
      externalLabData$startDate < currEndDate &
      externalLabData$analyte == currAnalyte &
      externalLabData$laboratoryName == currLab
  ), namesToAdd] <- externalSummaryData[i, namesToAdd]
}

CRAM<-externalLabData

waterChemList <-
  neonUtilities::loadByProduct(
    dpID = "DP1.20093.001",
    site = "BARC",
    package = 'expanded',
    check.size = FALSE
  )

externalLabData <- waterChemList$swc_externalLabDataByAnalyte
externalSummaryData <- waterChemList$swc_externalLabSummaryData
externalSummaryData$labSpecificEndDate[is.na(externalSummaryData$labSpecificEndDate)] <-
  as.POSIXct(Sys.Date())

#Check that the analytes match
externalLabData$analyte[!externalLabData$analyte %in% externalSummaryData$analyte]

#Link the data by analyte
namesToAdd <-
  names(externalSummaryData)[!(names(externalSummaryData) %in% names(externalLabData))]
namesToAdd <- namesToAdd[!(namesToAdd %in% c("endDate"))]
externalLabData[, namesToAdd] <- NA
for (i in 1:nrow(externalSummaryData)) {
  currStartDate <- externalSummaryData$labSpecificStartDate[i]
  currEndDate <- externalSummaryData$labSpecificEndDate[i]
  currAnalyte <- externalSummaryData$analyte[i]
  currLab <- externalSummaryData$laboratoryName[i]
  externalLabData[(
    externalLabData$startDate >= currStartDate &
      externalLabData$startDate < currEndDate &
      externalLabData$analyte == currAnalyte &
      externalLabData$laboratoryName == currLab
  ), namesToAdd] <- externalSummaryData[i, namesToAdd]
}

BARC<-externalLabData

waterChemList <-
  neonUtilities::loadByProduct(
    dpID = "DP1.20093.001",
    site = "LIRO",
    package = 'expanded',
    check.size = FALSE
  )

externalLabData <- waterChemList$swc_externalLabDataByAnalyte
externalSummaryData <- waterChemList$swc_externalLabSummaryData
externalSummaryData$labSpecificEndDate[is.na(externalSummaryData$labSpecificEndDate)] <-
  as.POSIXct(Sys.Date())

#Check that the analytes match
externalLabData$analyte[!externalLabData$analyte %in% externalSummaryData$analyte]

#Link the data by analyte
namesToAdd <-
  names(externalSummaryData)[!(names(externalSummaryData) %in% names(externalLabData))]
namesToAdd <- namesToAdd[!(namesToAdd %in% c("endDate"))]
externalLabData[, namesToAdd] <- NA
for (i in 1:nrow(externalSummaryData)) {
  currStartDate <- externalSummaryData$labSpecificStartDate[i]
  currEndDate <- externalSummaryData$labSpecificEndDate[i]
  currAnalyte <- externalSummaryData$analyte[i]
  currLab <- externalSummaryData$laboratoryName[i]
  externalLabData[(
    externalLabData$startDate >= currStartDate &
      externalLabData$startDate < currEndDate &
      externalLabData$analyte == currAnalyte &
      externalLabData$laboratoryName == currLab
  ), namesToAdd] <- externalSummaryData[i, namesToAdd]
}

LIRO<-externalLabData

waterChemList <-
  neonUtilities::loadByProduct(
    dpID = "DP1.20093.001",
    site = "PRLA",
    package = 'expanded',
    check.size = FALSE
  )

externalLabData <- waterChemList$swc_externalLabDataByAnalyte
externalSummaryData <- waterChemList$swc_externalLabSummaryData
externalSummaryData$labSpecificEndDate[is.na(externalSummaryData$labSpecificEndDate)] <-
  as.POSIXct(Sys.Date())

#Check that the analytes match
externalLabData$analyte[!externalLabData$analyte %in% externalSummaryData$analyte]

#Link the data by analyte
namesToAdd <-
  names(externalSummaryData)[!(names(externalSummaryData) %in% names(externalLabData))]
namesToAdd <- namesToAdd[!(namesToAdd %in% c("endDate"))]
externalLabData[, namesToAdd] <- NA
for (i in 1:nrow(externalSummaryData)) {
  currStartDate <- externalSummaryData$labSpecificStartDate[i]
  currEndDate <- externalSummaryData$labSpecificEndDate[i]
  currAnalyte <- externalSummaryData$analyte[i]
  currLab <- externalSummaryData$laboratoryName[i]
  externalLabData[(
    externalLabData$startDate >= currStartDate &
      externalLabData$startDate < currEndDate &
      externalLabData$analyte == currAnalyte &
      externalLabData$laboratoryName == currLab
  ), namesToAdd] <- externalSummaryData[i, namesToAdd]
}

PRLA<-externalLabData

waterChemList <-
  neonUtilities::loadByProduct(
    dpID = "DP1.20093.001",
    site = "SUGG",
    package = 'expanded',
    check.size = FALSE
  )

externalLabData <- waterChemList$swc_externalLabDataByAnalyte
externalSummaryData <- waterChemList$swc_externalLabSummaryData
externalSummaryData$labSpecificEndDate[is.na(externalSummaryData$labSpecificEndDate)] <-
  as.POSIXct(Sys.Date())

#Check that the analytes match
externalLabData$analyte[!externalLabData$analyte %in% externalSummaryData$analyte]

#Link the data by analyte
namesToAdd <-
  names(externalSummaryData)[!(names(externalSummaryData) %in% names(externalLabData))]
namesToAdd <- namesToAdd[!(namesToAdd %in% c("endDate"))]
externalLabData[, namesToAdd] <- NA
for (i in 1:nrow(externalSummaryData)) {
  currStartDate <- externalSummaryData$labSpecificStartDate[i]
  currEndDate <- externalSummaryData$labSpecificEndDate[i]
  currAnalyte <- externalSummaryData$analyte[i]
  currLab <- externalSummaryData$laboratoryName[i]
  externalLabData[(
    externalLabData$startDate >= currStartDate &
      externalLabData$startDate < currEndDate &
      externalLabData$analyte == currAnalyte &
      externalLabData$laboratoryName == currLab
  ), namesToAdd] <- externalSummaryData[i, namesToAdd]
}

SUGG<-externalLabData

waterChemList <-
  neonUtilities::loadByProduct(
    dpID = "DP1.20093.001",
    site = "TOOK",
    package = 'expanded',
    check.size = FALSE
  )

externalLabData <- waterChemList$swc_externalLabDataByAnalyte
externalSummaryData <- waterChemList$swc_externalLabSummaryData
externalSummaryData$labSpecificEndDate[is.na(externalSummaryData$labSpecificEndDate)] <-
  as.POSIXct(Sys.Date())

#Check that the analytes match
externalLabData$analyte[!externalLabData$analyte %in% externalSummaryData$analyte]

#Link the data by analyte
namesToAdd <-
  names(externalSummaryData)[!(names(externalSummaryData) %in% names(externalLabData))]
namesToAdd <- namesToAdd[!(namesToAdd %in% c("endDate"))]
externalLabData[, namesToAdd] <- NA
for (i in 1:nrow(externalSummaryData)) {
  currStartDate <- externalSummaryData$labSpecificStartDate[i]
  currEndDate <- externalSummaryData$labSpecificEndDate[i]
  currAnalyte <- externalSummaryData$analyte[i]
  currLab <- externalSummaryData$laboratoryName[i]
  externalLabData[(
    externalLabData$startDate >= currStartDate &
      externalLabData$startDate < currEndDate &
      externalLabData$analyte == currAnalyte &
      externalLabData$laboratoryName == currLab
  ), namesToAdd] <- externalSummaryData[i, namesToAdd]
}

TOOK<-externalLabData


all_data<-rbind(CRAM,BARC,LIRO,PRLA,TOOK,SUGG)
unique(all_data$laboratoryName)

write.csv(all_data,"NEON_chemandmethods_data.csv")
