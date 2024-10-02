####packages required####
library(robis)
library(dplyr)
library(car)
library(tidyverse)
####1. Data acquisition####
#####GBIF#####
#Data downloaded from GBIF portal and imported to R first#

#add "nameinlist" into each species data#
Aval$nameinlist <- "Avicennia alba"
Avbi$nameinlist <- "Avicennia bicolor"
Avge$nameinlist <- "Avicennia germinans"
Avin$nameinlist <- "Avicennia integra"
Avma$nameinlist <- "Avicennia marina"
Avof$nameinlist <- "Avicennia officinalis"
Avsc$nameinlist <- "Avicennia schaueriana"
Brcy$nameinlist <- "Bruguiera cylindrica"
Brex$nameinlist <- "Bruguiera exaristata"
#B. gymnorrhiza on GBIF as both B. gymnorrhiza and B. gymnorhiza, and occurrences do not overlap#
Brgy_gymnorhiza$nameinlist <- "Bruguiera gymnorrhiza"
Brgy_gymnorrhiza$nameinlist <- "Bruguiera gymnorrhiza"
Brha$nameinlist <- "Bruguiera hainesii"
Brpa$nameinlist <- "Bruguiera parviflora"
Brrh$nameinlist <- "Bruguiera ×rhynchopetala "
Brse$nameinlist <- "Bruguiera sexangula"
Ceau$nameinlist <- "Ceriops australis"
Cede$nameinlist <- "Ceriops decandra"
Kaca$nameinlist <- "Kandelia candel"
Kaob$nameinlist <- "Kandelia obovata"
Lara$nameinlist <- "Laguncularia racemosa"
Luli$nameinlist <- "Lumnitzera littorea"
Lura$nameinlist <- "Lumnitzera racemosa"
Nyfr$nameinlist <- "Nypa fruticans"
Rhap$nameinlist <- "Rhizophora apiculata"
Rhha$nameinlist <- "Rhizophora xharrisonii"
Rhla$nameinlist <- "Rhizophora ×lamarckii"
Rhma$nameinlist <- "Rhizophora mangle"
Rhmu$nameinlist <- "Rhizophora mucronata"
Rhra$nameinlist <- "Rhizophora racemosa"
Rhsa$nameinlist <- "Rhizophora samoensis"
Rhst$nameinlist <- "Rhizophora stylosa"
Soal$nameinlist <- "Sonneratia alba"
Soap$nameinlist <- "Sonneratia apetala"
Soca$nameinlist <- "Sonneratia caseolaris"
Sogr$nameinlist <- "Sonneratia griffithii"
Sogu$nameinlist <- "Sonneratia gulngai"
Sola$nameinlist <- "Sonneratia lanceolata"
Soov$nameinlist <- "Sonneratia ovata"

#Remove Ceau data from Ceta#
Ceta_corrected<-anti_join(Ceta,Ceau, by = 'gbifID')
Ceta_corrected$nameinlist <- "Ceriops tagal"

#merging all GBIF data#
gbiflist <- list(Aval, Avbi, Avge, Avin, Avma, 
                 Avof, Avsc,Brcy, Brex, Brgy_gymnorhiza, Brgy_gymnorrhiza, Brha, Brpa, Brrh, Brse, Ceau,
                 Cede,Ceta_corrected, Kaca, Kaob, Lara, Luli, Lura, Nyfr, Rhap, Rhha, Rhla, Rhma, Rhmu, Rhra, 
                 Rhsa, Rhst, Soal, Soap, Soca, Sogr, Sogu, Sola, Soov)

#re-define column properties after merging, if needed#
Colchangefunction<- function(data){
  clean_data <- data %>% 
    mutate(infraspecificEpithet = as.character(infraspecificEpithet)) %>%
    mutate(individualCount = as.numeric(individualCount )) %>%
    mutate(coordinatePrecision = as.numeric(coordinatePrecision)) %>%
    mutate(depth = as.numeric(depth)) %>%
    mutate(depthAccuracy = as.numeric(depthAccuracy)) %>%
    mutate(establishmentMeans = as.character(establishmentMeans)) %>%
    mutate(catalogNumber = as.character(catalogNumber)) %>%
    mutate(typeStatus = as.character(typeStatus)) 
  
  return(clean_data)
  
}

clean_list <- lapply(gbiflist, Colchangefunction) 

majorgbifnew <- do.call(rbind, clean_list)

####excluding data without coordinates####
majorgbifnew <- majorgbifnew[!(is.na(majorgbifnew$decimalLatitude)|majorgbifnew$decimalLatitude==""),]
dim(majorgbifnew)


#export GBIF data#
write.csv(majorgbifnew,"LOCATION/majorgbifnew.csv", row.names = FALSE)


#####OBIS#####
#acquire occurrence data from obis#
#Nypa#
nyfr <- occurrence("Nypa fruticans")
#Avicennia#
aval <- occurrence("Avicennia alba")
avge <- occurrence("Avicennia germinans")
avma <- occurrence("Avicennia marina")
avof <- occurrence("Avicennia officinalis")
avsc <- occurrence("Avicennia schaueriana")
#Lumnitzera#
luli <- occurrence("Lumnitzera littorea")
lura <- occurrence("Lumnitzera racemosa")
#Bruguiera#
brcy <- occurrence("Bruguiera cylindrica")
brex <- occurrence("Bruguiera exaristata")
brgy <- occurrence("Bruguiera gymnorhiza")
brha <- occurrence("Bruguiera hainesii")
brpa <- occurrence("Bruguiera parviflora")
brse <- occurrence("Bruguiera sexangula")
#Ceriops#
ceau <- occurrence("Ceriops australis")
ceta <- occurrence("Ceriops tagal")
#Kandelia#
kaca <- occurrence("Kandelia candel")
kaob <- occurrence("Kandelia obovata")
#Rhizophora#
rhap <- occurrence("Rhizophora apiculata")
rhma <- occurrence("Rhizophora mangle")
rhmu <- occurrence("Rhizophora mucronata")
rhra <- occurrence("Rhizophora racemosa")
rhst <- occurrence("Rhizophora stylosa")
rhha <- occurrence("Rhizophora harrisonii")
rhla <- occurrence("Rhizophora ×lamarckii")
#Sonneratia#
soal <- occurrence("Sonneratia alba")
soap <- occurrence("Sonneratia apetala")
soca <- occurrence("Sonneratia caseolaris")
sogu <- occurrence("Sonneratia ×gulngai")
#Laguncularia#
lara <- occurrence("Laguncularia racemosa")


#merge all species data (dplyr)#
majorobisnew <- do.call("bind_rows", list(aval,avge,avma,avof,avsc,brcy,brex,brgy,brha,brpa,brse,ceau,ceta,kaca,kaob,lara,luli,lura,nyfr,rhap,rhma,rhmu,rhra,rhst,rhha,rhla,soal,soap,soca,sogu))


#export combined data to csv file#
majorobisnew$nameinlist <- majorobisnew$scientificName
#A. marina data also include A. m. subsp. eucalyptifolia so nameinlist also need to further modified#
majorobisnew$nameinlist[majorobisnew$nameinlist == "Avicennia marina subsp. eucalyptifolia"] <- "Avicennia marina"  
write.csv(majorobisnew,"LOCATION/majorobisnew.csv", row.names = FALSE)



#####Combining data#####
#dropping unwanted columns#
#keeping columns which we think will be useful#
majorgbifnew$database <-"GBIF"
majorobisnew$database <-"OBIS"
gbifmajorcol <- select(majorgbifnew,nameinlist,gbifID,family,genus,species,infraspecificEpithet,
                       taxonRank,scientificName,verbatimScientificName,countryCode,
                       locality,stateProvince,occurrenceStatus,decimalLatitude,decimalLongitude,
                       coordinateUncertaintyInMeters,coordinatePrecision,eventDate,
                       day,month,year,basisOfRecord,institutionCode,collectionCode,
                       establishmentMeans,issue,database)

obismajorcol <- select(majorobisnew,nameinlist,scientificNameID,family,genus,species,infraspecificEpithet,
                       specificEpithet,taxonRank,subspecies,scientificName,
                       scientificNameAuthorship,id,absence,basisOfRecord,institutionCode,
                       country,locality,occurrenceStatus,occurrenceRemarks,decimalLatitude,decimalLongitude,
                       coordinateUncertaintyInMeters,coordinatePrecision,geodeticDatum,eventDate,
                       day,month,year,date_year,habitat,island,datasetName,flags,database)

#merging OBIS and GBIF data#
gbifmajorcol$eventDate<-as.character(gbifmajorcol$eventDate)
majornewest<-merge(x = gbifmajorcol, y = obismajorcol,  all=TRUE)

#rearrange columns#
majornewest_WGS84 <- majornewest[, c("nameinlist","scientificName","verbatimScientificName","family","genus","species","specificEpithet",
                                           "subspecies","infraspecificEpithet","scientificNameAuthorship","taxonRank","scientificNameID","database",
                                           "id","gbifID","decimalLatitude","decimalLongitude","coordinateUncertaintyInMeters","coordinatePrecision",
                                           "countryCode","geodeticDatum","country","stateProvince","locality","island","habitat","occurrenceStatus",
                                           "absence","occurrenceRemarks","establishmentMeans","eventDate","day","month","year","date_year",
                                           "basisOfRecord","institutionCode","collectionCode","datasetName","issue","flags")]



#combine "year"(GBIF) and "date_year"(OBIS) into single column#
majornewest_WGS84$year <- ifelse(is.na(majornewest_WGS84$date_year)  == TRUE,majornewest_WGS84$year, majornewest_WGS84$date_year)
majornewest_WGS84 <- subset(majornewest_WGS84, select = -date_year)

#transfer "absence"(OBIS) to "occurrenceStatus"(GBIF): "absence"=FALSE = "occurrenceStatus"=PRESENT#
majornewest_WGS84$absence <- ifelse(majornewest_WGS84$absence == "FALSE", "PRESENT", majornewest_WGS84$absence)
majornewest_WGS84$occurrenceStatus <- ifelse(is.na(majornewest_WGS84$occurrenceStatus) == TRUE, majornewest_WGS84$absence,majornewest_WGS84$occurrenceStatus)
majornewest_WGS84$occurrenceStatus <- ifelse(majornewest_WGS84$occurrenceStatus == "present", "Present", majornewest_WGS84$occurrenceStatus)
majornewest_WGS84 <- subset(majornewest_WGS84, select = -absence)

#combine "flags"(OBIS) and "issue"(GBIF) into "flags"(OBIS)), then rename column into issueFlag#
majornewest_WGS84$flags <- ifelse(is.na(majornewest_WGS84$flags) == TRUE, majornewest_WGS84$issue, majornewest_WGS84$flags)
majornewest_WGS84 <- subset(majornewest_WGS84, select = -issue)

majornewest_WGS84<-majornewest_WGS84 %>%
  rename(issueFlag = flags)

#combine "id"(OBIS) and "gbifId" (GBIF) into "id"#
majornewest_WGS84$id <- ifelse(is.na(majornewest_WGS84$id) == TRUE, majornewest_WGS84$gbifID, majornewest_WGS84$id)
majornewest_WGS84 <- subset(majornewest_WGS84, select = -gbifID)

#remove A.marina entries with flags ZERO_COORDINATE and COUNTRY_COORDINATE_MISMATCH#
#as these entries were not excluded on GBIF portal first by mistake before download#
majornewest_WGS84<-[!grepl("ZERO_COORDINATE|COUNTRY_COORDINATE_MISMATCH",majornewest_WGS84$issueFlag),]
dim(majornewest_WGS84)

#export the modified dataset#
#to create a new datafile with original WGS84 coordinates#
write.csv(majornewest_WGS84,"LOCATION/majornewest_WGS84.csv", row.names = FALSE)


####2. Data curation####
#import daata after checking in QGIS#
#majornewest_54034_84_TNCcom.csv is the combined data with additional data from QGIS analysis#
majornewest_TNCcom <- read_csv("majornewest_54034_84_TNCcom.csv")


#####Looking for duplicate within and across databases#####
#####complete duplicates#####
#complete replicates regardless of databases#
cdup<-as.data.frame(majornewest_Com %>% 
                      group_by(nameinlist,	scientificName,	scientificNameAuthorship,	scientificNameID,	verbatimScientificName,	family,	genus,	species,	
                               specificEpithet,	subspecies,infraspecificEpithet,taxonRank,	decimalLatitude,	decimalLongitude,	coordinateUncertaintyInMeters,
                               coordinatePrecision,	geodeticDatum,	country,	countryCode,	stateProvince, locality,	island,	habitat, occurrenceStatus,	occurrenceRemarks, 
                               establishmentMeans,	eventDate,	day, month,	year,	basisOfRecord,	institutionCode,	collectionCode, datasetName, issueFlag) %>% 
                      mutate(dupe = n()>1))
write.csv(cdup,"LOCATION/cdup.csv", row.names = FALSE)
dim(cdup)
sum(cdup$dupe == TRUE)
sum(cdup$database== 'OBIS' & cdup$dupe == TRUE)
sum(cdup$database== 'GBIF' & cdup$dupe == TRUE)

#complete replicates within database#
majornewestcompletedup<-as.data.frame(majornewest_Com %>% 
                                             group_by(nameinlist,	scientificName,	scientificNameAuthorship,	scientificNameID,	verbatimScientificName,	family,	genus,	species,	
                                                      specificEpithet,	subspecies,infraspecificEpithet,taxonRank,	decimalLatitude,	decimalLongitude,	coordinateUncertaintyInMeters,
                                                      coordinatePrecision,	geodeticDatum,	country,	countryCode,	stateProvince, locality,	island,	habitat, occurrenceStatus,	occurrenceRemarks, 
                                                      establishmentMeans,	eventDate,	day, month,	year,	basisOfRecord,	institutionCode,	collectionCode, datasetName, issueFlag, database) %>% 
                                             mutate(dupe = n()>1))
write.csv(majornewestcompletedup,"LOCATION/majornewestpseudoperfectdup.csv", row.names = FALSE)

#count number of complete duplicates 
sum(majornewestcompletedup$dupe == TRUE)#within database
sum(cdup$dupe == TRUE)#reguardless of database

#deduplicate, keep arbitrary one of duplicate and remove others#
majornewest_TNCcom_dedup<-distinct(majornewest_TNCcom, 
                                          nameinlist,	scientificName,	scientificNameAuthorship,	scientificNameID,	verbatimScientificName,	family,	genus,	species,	
                                          specificEpithet,	subspecies,infraspecificEpithet,taxonRank,	decimalLatitude,	decimalLongitude,	coordinateUncertaintyInMeters,
                                          coordinatePrecision,	geodeticDatum,	country,	countryCode,	stateProvince, locality,	island,	habitat, occurrenceStatus,	occurrenceRemarks, 
                                          establishmentMeans,	eventDate,	day, month,	year,	basisOfRecord,	institutionCode,	collectionCode, datasetName, issueFlag, database,
                                          .keep_all = TRUE)
write.csv(majornewest_TNCcom_dedup,"LOCATION/majornewest_TNCcom_dedup.csv", row.names = FALSE)
dim(majornewest_TNCcom_dedup)

#check again after deduplication#
majornewest_decompleteduplicate_check<-as.data.frame(majornewest_TNCcom_dedup %>% 
                                                       group_by(nameinlist,	scientificName,	scientificNameAuthorship,	scientificNameID,	verbatimScientificName,	family,	genus,	species,	
                                                                specificEpithet,	subspecies,infraspecificEpithet,taxonRank,	decimalLatitude,	decimalLongitude,	coordinateUncertaintyInMeters,
                                                                coordinatePrecision,	geodeticDatum,	country,	countryCode,	stateProvince, locality,	island,	habitat, occurrenceStatus,	occurrenceRemarks, 
                                                                establishmentMeans,	eventDate,	day, month,	year,	basisOfRecord,	institutionCode,	collectionCode, datasetName, issueFlag, database) %>% 
                                                       mutate(dupe3 = n()>1))

sum(majornewest_decompleteduplicate_check$dupe3 == TRUE)

#look for across-database complete duplicates#
#deduplication by database then recombine for checking#
majornewest_TNCcom_GBIF<-subset(majornewest_TNCcom, database == "GBIF")
dim(majornewest_TNCcom_GBIF)
majornewest_TNCcom_OBIS<-subset(majornewest_TNCcom, database == "OBIS")
dim(majornewest_TNCcom_OBIS)

majornewest_TNCcom_GBIF_dedup<-distinct(mmajornewest_TNCcom_GBIF, 
                                        nameinlist,	scientificName,	scientificNameAuthorship,	scientificNameID,	verbatimScientificName,	family,	genus,	species,	
                                        specificEpithet,	subspecies,infraspecificEpithet,taxonRank,	decimalLatitude,	decimalLongitude,	coordinateUncertaintyInMeters,
                                        coordinatePrecision,	geodeticDatum,	country,	countryCode,	stateProvince, locality,	island,	habitat, occurrenceStatus,	occurrenceRemarks, 
                                        establishmentMeans,	eventDate,	day, month,	year,	basisOfRecord,	institutionCode,	collectionCode, datasetName, issueFlag, database,
                                        .keep_all = TRUE)

majornewest_TNCcom_OBIS_dedup<-distinct(majornewest_TNCcom_OBIS, 
                                        nameinlist,	scientificName,	scientificNameAuthorship,	scientificNameID,	verbatimScientificName,	family,	genus,	species,	
                                        specificEpithet,	subspecies,infraspecificEpithet,taxonRank,	decimalLatitude,	decimalLongitude,	coordinateUncertaintyInMeters,
                                        coordinatePrecision,	geodeticDatum,	country,	countryCode,	stateProvince, locality,	island,	habitat, occurrenceStatus,	occurrenceRemarks, 
                                        establishmentMeans,	eventDate,	day, month,	year,	basisOfRecord,	institutionCode,	collectionCode, datasetName, issueFlag, database,
                                        .keep_all = TRUE)

dim(majornewest_TNCcom_GBIF_dedup)
dim(majornewest_TNCcom_OBIS_dedup)


majornewest_com_dedup<-rbind(majornewest_TNCcom_GBIF_dedup,majornewest_TNCcom_OBIS_dedup)
dim(majornewest_TNCcom_dedup)

majornewest_TNCcom_dedup2<-distinct(majornewest_TNCcom_dedup, 
                                    nameinlist,	scientificName,	scientificNameAuthorship,	scientificNameID,	verbatimScientificName,	family,	genus,	species,	
                                    specificEpithet,	subspecies,infraspecificEpithet,taxonRank,	decimalLatitude,	decimalLongitude,	coordinateUncertaintyInMeters,
                                    coordinatePrecision,	geodeticDatum,	country,	countryCode,	stateProvince, locality,	island,	habitat, occurrenceStatus,	occurrenceRemarks, 
                                    establishmentMeans,	eventDate,	day, month,	year,	basisOfRecord,	institutionCode,	collectionCode, datasetName, issueFlag, database,
                                    .keep_all = TRUE)

dim(majornewest_TNCcom_dedup2)

#####potential duplicates after moving complete duplicates#####
#find out the potential duplicates#
#overall
majornewest_potentialduplicate<-as.data.frame(majornewest_TNCcom_dedup %>% 
                                                group_by(nameinlist,decimalLatitude,decimalLongitude, eventDate, day, month, year) %>% 
                                                mutate(dupe2 = n()>1))
majornewest_potentialduplicate<-subset(majornewest_potentialduplicate, select = -c(dupe) )
sum(majornewest_potentialduplicate$dupe2 == TRUE)
write.csv(majornewest_potentialduplicate,"LOCATION/majornewest_potentialduplicate.csv", row.names = FALSE)

#within database
majornewest_potentialduplicate_dbdup<-as.data.frame(majornewest_TNCcom_dedup %>% 
                                                      group_by(nameinlist,decimalLatitude,decimalLongitude, eventDate, day, month, year, database) %>% 
                                                      mutate(dupe3 = n()>1))
majornewest_potentialduplicate_dbdup<-subset(majornewest_potentialduplicate_dbdup, select = -c(dupe) )
write.csv(majornewest_potentialduplicate_dbdup,"LOCATION/majornewest_potentialduplicate_dbdup.csv", row.names = FALSE)
sum(majornewest_potentialduplicate_dbdup$dupe3 == TRUE)

#count rows that are partial duplicates (overall) #
sum(majornewest_potentialduplicate$database== 'OBIS' & majornewest_potentialduplicate$dupe2 == TRUE)
sum(majornewest_potentialduplicate$database== 'GBIF' & majornewest_potentialduplicate$dupe2 == TRUE)
sum(majornewest_potentialduplicate$dupe2 == TRUE)

#count rows that are duplicates (within database)#
sum(majornewest_potentialduplicate_dbdup$database== 'OBIS' & majornewest_potentialduplicate_dbdup$dupe3 == TRUE)
sum(majornewest_potentialduplicate_dbdup$database== 'GBIF' & majornewest_potentialduplicate_dbdup$dupe3 == TRUE)
sum(majornewest_potentialduplicate_dbdup$dupe3 == TRUE)


#find out the across database potential duplicate#
#separate dedup within database then recombine#
nocdup_GBIF<-subset(majornewest_TNCcom_dedup, database == "GBIF")
dim(nocdup_GBIF)
nocdup_OBIS<-subset(majornewest_TNCcom_dedup, database == "OBIS")
dim(nocdup_OBIS)

depdup_GBIF<-distinct(nocdup_GBIF,nameinlist,decimalLatitude,decimalLongitude, eventDate, day, month, year,
                      .keep_all = TRUE)
dim(depdup_GBIF)


depdup_OBIS<-distinct(nocdup_OBIS,nameinlist,decimalLatitude,decimalLongitude, eventDate, day, month, year,
                      .keep_all = TRUE)
dim(depdup_OBIS)
dpdup<-rbind(depdup_GBIF,depdup_OBIS)
dim(dpdup)

dpdup_2<-distinct(dpdup,nameinlist,decimalLatitude,decimalLongitude, eventDate, day, month, year,
                  .keep_all = TRUE)
dim(dpdup_2)

dpdup_dup<-as.data.frame(dpdup %>% 
                           group_by(nameinlist,decimalLatitude,decimalLongitude, eventDate, day, month, year) %>% 
                           mutate(dupe4 = n()>1))

acrosspdup<-subset(dpdup_dup, dupe4 == "TRUE")
View(acrosspdup)


#deduplicate across-database potential duplicates; keeping GBIS as more info retained#
#"af0652d4-7aa3-4fc5-91bb-598765a34e68" and "f62c1bb2-62b4-4392-a742-404898d7184e" are the two potential duplicates found from manual inspection after the previous step#
dedup<-majornewest_TNCcom_dedup[!grepl("af0652d4-7aa3-4fc5-91bb-598765a34e68", 
                                       majornewest_TNCcom_dedup$id),]
dedup<-dedup[!grepl("f62c1bb2-62b4-4392-a742-404898d7184e", dedup$id),]
#Check after removal
sum(grepl("f62c1bb2-62b4-4392-a742-404898d7184e", dedup$id))
sum(grepl("af0652d4-7aa3-4fc5-91bb-598765a34e6", dedup$id))
dim(dedup)

####Excluding unsuitable occurrences####
# count entries with certain flag/issue#
dedup_geo_inv<-grepl("GEODETIC_DATUM_INVALID", dedup$issueFlag)
sum(grepl("GEODETIC_DATUM_INVALID", dedup$issueFlag))
sum(grepl("GEODETIC_DATUM_INVALID", dedup$issueFlag) & dedup$database == "GBIF")
sum(grepl("PRESUMED_SWAPPED_COORDINATE", dedup$issueFlag))
sum(grepl("PRESUMED_SWAPPED_COORDINATE", dedup$issueFlag) & dedup$database == "GBIF")
sum(grepl("ZERO_COORDINATE", dedup$issueFlag))
sum(grepl("ZERO_COORDINATE", dedup$issueFlag) & dedup$database == "GBIF")

sum(grepl("FOSSIL_SPECIMEN", dedup$basisOfRecord))
sum(grepl("FOSSIL_SPECIMEN", dedup$basisOfRecord) & dedup$database == "GBIF")
sum(grepl("LIVING_SPECIMEN", dedup$basisOfRecord))
sum(grepl("LIVING_SPECIMEN", dedup$basisOfRecord) & dedup$database == "GBIF")

#remove records with issues/flags#
dedup1<-dedup[!grepl("ZERO_COORDINATE", dedup$issueFlag),]
dedup1<-dedup1[!grepl("FOSSIL_SPECIMEN|LIVING_SPECIMEN", dedup1$basisOfRecord),]
dedup_flag<-dedup1[!grepl("PRESUMED_SWAPPED_COORDINATE|GEODETIC_DATUM_INVALID", dedup1$issueFlag),]

write.csv(dedup_flag,"LOCATION/dedup_flag.csv", row.names = FALSE)
View(dedup_flag)
dim(dedup_flag)

####Checking the reliability of the spatial location####
#####count how many within mangrove latitudinal range#####
sum(dedup_flag$decimalLatitude >32)
sum(dedup_flag$decimalLatitude <(-39) )
largelat<-subset(dedup_flag,decimalLatitude >32 | decimalLatitude <(-39))
View(largelat)
dim(largelat)
sum(largelat$database == "GBIF")
write.csv(largelat,"LOCATION/largelat.csv", row.names = FALSE)

#####Remove records outside latitudinal range of mangroves#####
inLat<-subset(dedup_flag, decimalLatitude <32 & decimalLatitude >(-39))
dim(inLat)
View(inLat)

#####Retain entries that are with year#####
inLat_withyear<-subset(inLat,is.na(inLat$eventDate) == FALSE | is.na(inLat$year) == FALSE)
dim(inLat_withyear)
dim(inLat)
#####Retain entries that are within known mangroves *Worthington et al. 2020#####
inman<-subset(inLat_withyear,distMan ==0)
dim(inman)


####3. Count number of entries####
#####Count entries by basis of record#####
inman_basisofrecord<-as.data.frame(inman %>% 
                               group_by(basisOfRecord) %>% 
                               summarise(n = n()))

#####Count entries by year#####
inman_yearcount<- as.data.frame(inman %>% count(year))


#####Count entries by coordinate decimal places#####
inman_decimal_count<-as.data.frame(inman%>% 
                                     group_by(database,coordecimal) %>% 
                                     summarise(n = n()))

View(inman_decimal_count)


inman_decimal_count<-(inman_decimal_count %>%
                        group_by(database) %>%
                        mutate(percent = n/sum(n)))

inman_decimal_count$percent<- as.numeric(format(round(inman_decimal_count$percent, 4), nsmall=4))

inman_decimal_count$coordecimal<-as.factor(inman_decimal_count$coordecimal)

#####Count entries by coodrinate precision#####
#find out the precision information from coordinatePrecision and coordinateUncertaintyInMeters#
inman$precision <- ifelse(inman$database == "GBIF", 
                          ifelse(is.na(inman$coordinateUncertaintyInMeters), inman$coordinatePrecision * 111139, inman$coordinateUncertaintyInMeters),
                          ifelse(is.na(inman$coordinateUncertaintyInMeters), inman$coordinatePrecision,inman$coordinateUncertaintyInMeters))

View(inman)
inman$precisionrange <- ifelse(is.na(inman$precision), 'Not Available', 
                               ifelse(inman$precision < 100, "<0.1 km",
                                      ifelse(inman$precision > 100 & inman$precision <= 1000, "0.1-1 km",
                                             ifelse(inman$precision > 1000 & inman$precision <= 5000, "1-5 km",
                                                    ifelse(inman$precision > 5000 & inman$precision <= 10000, "5-10 km",
                                                           ifelse(inman$precision > 10000 & inman$precision <= 100000, "10-100 km",
                                                                  ">100km"))))))

inman_precision<-as.data.frame(inman%>% 
                                 group_by(database,precisionrange) %>% 
                                 summarise(n = n()))

View(inman_precision)

inman_precision<-(inman_precision %>%
                    group_by(database) %>%
                    mutate(percent = n/sum(n)))

inman_precision$percent<- as.numeric(format(round(inman_precision$percent, 4), nsmall=4))


#####Count entries by species#####
inman_species<-as.data.frame(inman %>% 
                               group_by(nameinlist) %>% 
                               summarise(n = n()))

#####Count number ff species by number of record range#####
inman_species$obsrange2 <- ifelse(inman_species$n < 10, "<10", 
                                  ifelse(inman_species$n >= 10 & inman_species$n < 50, "10-49", 
                                         ifelse(inman_species$n >= 50 & inman_species$n < 100, "50-99",
                                                ifelse(inman_species$n >= 100 & inman_species$n < 1000, "100-999",
                                                       ifelse(inman_species$n >= 1000 & inman_species$n < 10000, "1000-9999",">10000")))))

inman_speciescount2<-as.data.frame(inman_species%>% 
                                     group_by(obsrange2) %>% 
                                     summarise(n = n()))

#####Count by AWP/IWP#####
##IWP
#species
sum(grepl("Avicennia alba|Avicennia integra|Avicennia marina|Avicennia officinalis|Bruguiera cylindrica|Bruguiera exaristata|Bruguiera gymnorrhiza|Bruguiera hainesii|Bruguiera parviflora|Bruguiera sexangula|Bruguiera ×rhynchopetala|Ceriops australis|Ceriops decandra|Ceriops tagal|Kandelia candel|Kandelia obovata|Lumnitzera littorea|Lumnitzera racemosa|Nypa fruticans|Rhizophora apiculata|Rhizophora mucronata|Rhizophora samoensis|Rhizophora stylosa|Rhizophora ×lamarckii|Sonneratia alba|Sonneratia apetala|Sonneratia caseolaris|Sonneratia griffithii|Sonneratia lanceolata|Sonneratia ovata|Sonneratia ×gulngai", inman_species$nameinlist))
inman_species$region <- ifelse(inman_species$nameinlist == "Avicennia alba|Avicennia integra|Avicennia marina|Avicennia officinalis|Bruguiera cylindrica|Bruguiera exaristata|Bruguiera gymnorrhiza|Bruguiera hainesii|Bruguiera parviflora|Bruguiera sexangula|Bruguiera ×rhynchopetala|Ceriops australis|Ceriops decandra|Ceriops tagal|Kandelia candel|Kandelia obovata|Lumnitzera littorea|Lumnitzera racemosa|Nypa fruticans|Rhizophora apiculata|Rhizophora mucronata|Rhizophora samoensis|Rhizophora stylosa|Rhizophora ×lamarckii|Sonneratia alba|Sonneratia apetala|Sonneratia caseolaris|Sonneratia griffithii|Sonneratia lanceolata|Sonneratia ovata|Sonneratia ×gulngai", "IWP", "AEP")

#obs
sum(grepl("Avicennia alba|Avicennia integra|Avicennia marina|Avicennia officinalis|Bruguiera cylindrica|Bruguiera exaristata|Bruguiera gymnorrhiza|Bruguiera hainesii|Bruguiera parviflora|Bruguiera sexangula|Bruguiera ×rhynchopetala|Ceriops australis|Ceriops decandra|Ceriops tagal|Kandelia candel|Kandelia obovata|Lumnitzera littorea|Lumnitzera racemosa|Nypa fruticans|Rhizophora apiculata|Rhizophora mucronata|Rhizophora samoensis|Rhizophora stylosa|Rhizophora ×lamarckii|Sonneratia alba|Sonneratia apetala|Sonneratia caseolaris|Sonneratia griffithii|Sonneratia lanceolata|Sonneratia ovata|Sonneratia ×gulngai", inman$nameinlist))

##AEP
#species
sum(grepl("Avicennia bicolor|Avicennia germinans|Avicennia schaueriana|Laguncularia racemosa|Rhizophora mangle|Rhizophora racemosa|Rhizophora ×harrisonii", inman_species$nameinlist))

#obs
sum(grepl("Avicennia bicolor|Avicennia germinans|Avicennia schaueriana|Laguncularia racemosa|Rhizophora mangle|Rhizophora racemosa|Rhizophora ×harrisonii", inman$nameinlist))

#####Count entries by country#####
##Need to do manually combine countryCode and country##
inman$countries<-ifelse(is.na(inman$countryCode),inman$country,inman$countryCode)
#change country name to country code
inman$countries[inman$countries == "Australia"] <- "AU"
inman$countries[inman$countries == "Brazil"] <-"BR"
inman$countries[inman$countries == "Malaysia"] <-"MY"
inman$countries[inman$countries == "New Zealand"] <-"NZ"
#count entries#
inman_countries<-as.data.frame(inman %>% 
                                 group_by(countries) %>% 
                                 summarise(n = n()))
