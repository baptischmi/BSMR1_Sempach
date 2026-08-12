#---------------------------File description----------------------------------
# This script contains the workflow for downloading of MTR from Birdscan Radar in
# Sempach. The output MTR tables are used within a workflow for real-time migration
# forecasts.

# Working documentation

# existing git scripts:

#- simple script to extract echo information
# https://github.com/baptischmi/BirdScan_RTools/blob/develop/developmentTools/scripts/bcrtScript_db_query_echo.R

#- birdScanR fctions:
# https://github.com/BirdScanCommunity/birdscanR/tree/develop/R

#- previous attempt to make realtime MTR compuation:
# https://github.com/baptischmi/MTRtables/blob/main/src/computeMTR_live.R
# and some settings : https://github.com/baptischmi/MTRtables/blob/main/settings/Feature_renaming_ClassAbrv_sbrs.R


# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ------------------------------ Settings ------------------------------
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


##---------------------------Working directory---------------------------------

# clean environment
rm( list=ls() ); gc() # remove all object from the R-working space; and empty the garbage
## set working directory, e.g. of the project folder
working_dir <- "C:/RadarData/CH_Sempach_2025/Rtime"
setwd(working_dir) # set the working directory
getwd() # call the path-name of the working directory

#link .Renviron file
readRenviron(".Renviron") 
  # toDo
  # list the key features saved in this Renvironment

# define output directory for MTR tables
mainOutputDir = file.path(working_dir, "mtr_output")

# define output directory for BZ exhibition and Biolovision (ornitho.ch) MTR tables an plots
plot_dir <- file.path(working_dir, "BZexhibition_plot")# "C:/RadarData/CH_Sempach/R-plot"



##----------------------------Libraries----------------------------------------

# the following lines ensure that required packages are installed

#define packages needed
list.of.packages <- c("tidyverse", "RODBC", "odbc", "birdscanR", "lubridate", "RCurl", "png", "scales") # , "suntools"

#check for uninstalled packages
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

#install new packages
if(length(new.packages)) install.packages(new.packages)

#Load libraries
lapply(list.of.packages, library, character.only = TRUE)

##--------------------------- rename colnames ---------------------------------
# toDo: save as a new file

# Add or remove features and tables as you wish
my_col_dbo_collection <- as.data.frame(
  rbind(
    # from table 'dbo.collection'
    c("echoID","echoID"),
    c("time_stamp","time_stamp"),
    c("time_string","time_string"), # use this to verfiy any timezone conversion issue of $time_stamp.
    c("stc_level","stc_level"),
    c("feature1","height"),
    c("feature2","direction"),
    c("feature3","speed"),
    #    c("feature14","pegel"), # out-comment or delete at wish
    c("feature24","alpha"),
    c("feature25","theta"),
    c("feature17","rcs"),
    c("feature37","speed_v1_7") # - This is the new speed variable, included in the Birdscan software as of v1.7
  ),
  stringsAsFactors = FALSE,
  # nm = c( "colname_org","colname_new")
) ; names(my_col_dbo_collection) <-  c( "colname_org","colname_new")


my_col_dbo_protocol <- as.data.frame(
  rbind(
    # from table 'dbo.protocol'
    c("protocolID","protocolID"),
    c("siteID","siteID"),
    c("startTime","startTime"),
    c("stopTime","stopTime"), 
    c("blockTime","blockTime"), 
    c("pulseType","pulseType"),
    c("rotate","rotate"),
    c("stc","stc"),
    c("threshold","threshold"),
    c("autoThreshold","autoThreshold")
  ),
  stringsAsFactors = FALSE,
  # nm = c( "colname_org","colname_new")
) ; names(my_col_dbo_protocol) <-  c( "colname_org","colname_new")


my_col_dbo_RFtables <- as.data.frame(
  rbind(
    # dbo_rf_class <- tbl(conn, "rf_classification") 
    c("class","class_id"),
    c("class_probability","class_probability"),
    c("mtr_factor","mtr_factor_rf"),
    c("mtr_factor_sphereDiaCm","mtr_factor_sphereDiaCm"),
    c("classifierVersion","classifierVersion"),
    
    # dbo_rf_classdef <- tbl(conn, "rfclasses") 
    c("name","class"),
    
    # ToDelete - not needed
    # # dbo_rf_allclasses_prob <- tbl(conn, "rf_class_probability") 
    # # this list the class probabilitiy  of each echo for all classes
    # c("class", "class_pot"),
    # c("value", "class_prob"),
    # c("mtr_factor", "mtr_factor_pot"),
    
    # dbo_rf_features <- tbl(conn, "echo_rffeature_map") 
    c("feature", "feature_key"),
    c("value", "feature_value")
  ),
  stringsAsFactors = FALSE,
  # nm = c( "colname_org","colname_new")
) ; names(my_col_dbo_RFtables) <-  c( "colname_org","colname_new")

my_col <- rbind(my_col_dbo_collection, 
                my_col_dbo_protocol,
                my_col_dbo_RFtables)


# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# --------------------------- Connect to DB ----------------------------
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

## ------------------------- SQL connection ----------------------------

# Define server and database
dbServer = "BIRDSCAN2200" # "server\\instance"
dbName = "CH_Sempach_2025"
dbDriverChar = "SQL Server Native Client 11.0"

# Create odbc connection (see link for help)
# (https://www.r-bloggers.com/setting-up-an-odbc-connection-with-ms-sql-server-on-windows/)
conn <- DBI::dbConnect(odbc::odbc(),
                       Driver = dbDriverChar,
                       Server = dbServer,
                       Database = dbName,
                       Uid='sa', # Sys.getenv("db_id"),
                       Pwd= 'radar', # Sys.getenv("db_key"), # can be hard coded, e.g. '123pwd'
                       Port=1433 #change the port information
)

## ---------------dbConnect for each required table ---------------------
  
# site table: important for lat, lon, altitude, time_shift, start and end of the project
dbo_site <- tbl(conn, "site")

# site table: not very useful, but radarID, or if one want to reconstruct the radar beam
dbo_radar <- tbl(conn, "radar")

# collection table: original table for echoID, time_stamp, height (feature1), direction (feature2), speed (feature3), RCS (feature14), 
# alpha (feature xx), theta (feature xx)
dbo_collection <- tbl(conn, "collection")

# classification for each echo
# toDelete: dbo_rf_classif <- tbl(conn, "rf_classification")
dbo_rf_class <- tbl(conn, "rf_classification")
  # following colnames: > dbo_rf_classif
  # echo : 'echoID' i.e. row number of collection_table
  # class : integer > winning class. get name from dbo.rf_classes
  # mtr_factor: mtr-factor for the class and WBF (see 'dbo.rfclass_wff_sizes') at the detection height 
  # class_probability : classificaiton probability foe the winning class. See table dbo.rf_class_probability for all class probabilities
  # mtr_factor_sphereDiaCm : RCS used to calcualte th MTR-factor
  # classifierVersion: version of the applied classifier (timestamp of its creation)

# classes >> mostly to get classification name from the classID.
# toDelete: dbo_rf_classes <- tbl(conn, "rfclasses")
dbo_rf_classdef <-  tbl(conn, "rfclasses")

# features not included in the collection table, incl. new RCS and WBF, shape features, etc.
dbo_rf_features <-  tbl(conn, "echo_rffeature_map")

# for each echo, the class probability of all classes defined in 
# dbo_rf_allclasses_prob <- tbl(conn, "rf_class_probability")

# protocol: important for pulyType, rotate, operation time (together with visibility)
dbo_protocol <- tbl(conn, "protocol")

# visibility: record automated interuption of detection 
dbo_visibility <- tbl(conn, "visibility")



# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ------------------- Set time zone and time range ---------------------
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


#- for realtime cMTR computation, incl. past 31 days
if(tz('') != 'UTC') Sys.setenv(TZ='UTC')# we work in UTC time zone, make this the default in handling dates
  # toDO
  # check if TZ is defined in '.Renviron'
message( paste0("Script running in ", tz('')), "\n..." )

#- Set timezones used by the radar (timestamp of echoes) and the target (timezone) for the purpose of the analyses - plotting)
# Example: targetTimeZone <- "Etc/GMT-1" # --> Etc/GMT-1 equals UTC+1
# use "Etc/GMT0" (UTC) as radarTimeZone for birdscan v1.6 and greater. Birdscan v1.6 and greater stores all times as UTC.
tz_shift <- as.numeric( dbo_site %>% select('timeShift') %>%  collect() ) # Get time zone saved in the database table 'dbo.site'
radarTimeZone <- paste0("Etc/GMT", ifelse(tz_shift >=0 , "-", "+"), abs(tz_shift)) # note that "UTC+1" is denoted as "Etc/GMT-1"
if( any( unlist(OlsonNames()) == radarTimeZone) ){
  message( paste0("The radar is operating in the following time zone (as entered in the database [dbo].[site].[timeShift]): ", radarTimeZone, "\n ...") )
} else {
  stop("The timeshift entered in the database [dbo].[site].[timeShift] is either missing or misleading. Per defulat, the Time Zone is set to 'UTC' (more exactly to 'Etc/GMT0'). \n ...")
  radarTimeZone <- "Etc/GMT0"
}

# ToDo: give  stop-message if no appropriate timeShift is included in the DB, i.e. radarTimeZone is not listed among the OlsenNames
targetTimeZone <- "Etc/GMT0" # keep all MTR in UTC. 
#- Retrieve & Set time range // requires lubridate package
NOW <- Sys.time() # in UTC!!!
t_max_radarTZ <- force_tz(NOW, radarTimeZone)
t_min_radarTZ <- as_date(t_max_radarTZ) - 31 # days...

timeRangeRadarTZ <- c(t_min_radarTZ, t_max_radarTZ)

#- Convert time range to time zone used in database
# add functionality about summer time!!!
t_min_targetTZ <- with_tz(t_min_radarTZ, tz = targetTimeZone)
t_max_targetTZ <- with_tz(t_max_radarTZ, tz = targetTimeZone)
timeRangeTargetTZ <- c(t_min_targetTZ, t_max_targetTZ)

# for plotting, use local timezone... 
IS_SUMMERTIME <- ifelse(NOW > as.POSIXct("2024-03-31 01:00", format="%Y-%m-%d %H:%M") & NOW < as.POSIXct("2024-10-27 02:00", format="%Y-%m-%d %H:%M"), TRUE, FALSE)
# for alternatives, check: https://stackoverflow.com/questions/16086962/how-to-get-a-time-zone-from-a-location-using-latitude-and-longitude-coordinates

date_tag <- format(NOW, "%Y%m%d")

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ------------------------ Extract and reformat data -------------------
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::


## --------------- Get site and define siteLocation -------------------

#- Extract all of dbo.site
site <-
  dbo_site %>%
  collect()

#- Save latitude and longitude in a new object, or set Sempach per default  
t_lat <- site$latitude
t_lon <- site$longitude
if(!any(is.na(c(t_lat, t_lon)))){
  siteLocation = c(t_lat, t_lon) # from DB
} else {
  siteLocation = c(47.127641, 8.192569) # SEMAPCH
}

t_altitude_asl <- site$altitude

## -------------- Compute twilight events fot the time periods -----------------
sunriseSunset <- twilight(timeRange = timeRangeTargetTZ,
                     latLon = siteLocation, 
                     timeZone = targetTimeZone
                      )

## -------------- Get echo, add day/night, reformat colnames -------------------

#extract collection table from database
echoData <-
  dbo_collection %>% 
  filter(time_stamp >= t_min_radarTZ & time_stamp <= t_max_radarTZ) %>% # doesn't work using timeRange_radarTZ[1] for t_min_radarTZ
  left_join(., dbo_rf_class, by = c("row" = "echo"), suffix = c("", ""))  %>%  # add class prob and MTR-factor
  left_join(., dbo_rf_classdef, by = c("class" = "id"), suffix = c("", ""))  %>% 
  left_join(., dbo_rf_features, by = c("row" = "echo"), suffix = c("", ""))  %>% # add two columns: 'feature' und 'value'
  filter(feature == "109" | feature == "167" | feature == "168") %>% # select a few features of interest from dbo_rf_feature$feature
  left_join(., dbo_protocol, by = "protocolID", suffix = c("", "_protocol"))  %>% 
  select(my_col$colname_org) %>%
  rename_all(~my_col$colname_new) %>%
  as_tibble %>%
  spread("feature_key", "feature_value") %>% # from long to wide format of the single class probabilities
  collect() # only then the sql-query will be performed.
# show_query()
echoData <- echoData %>% rename("rcs_14" = "109", "wbf" = "167", "wbf_cred" =  "168")

# table(echoData$class)
classSelection = c("passerine_type", "wader_type", "swift_type",
                   "large_bird", "unid_bird", "bird_flock"
)# omitted classes : "insect", "nonbio", "precipitation"
echoData <- echoData %>% 
  filter(class %in% classSelection)
if( nrow(echoData) > 0 ) {
  message( paste0(nrow(echoData), " echoes for the following classed retained for MTR computation: \n") )
  print( table(echoData$class, useNA = "ifany") )
}
if(nrow(echoData) == 0) stop( paste0("After classe selection, ZERO echo remaining for MTR computation. \n These were the selected classes: ", list(classSelection) ) )

#- Convert timeZone - refmorat time variable
echoData = convertTimeZone(data = echoData, colNames = c("time_stamp", "startTime", "stopTime"),
                           originTZ = radarTimeZone, targetTZ = targetTimeZone)


#- add day/night info to each echo
sunOrCivil = "civil"
echoData = addDayNightInfoPerEcho(echoData = echoData, # require colname "time_stamp_targetTZ"
                                  sunriseSunset = sunriseSunset, sunOrCivil = sunOrCivil)

#- rename colnames
echoData <- echoData %>% 
  mutate(feature1.altitude_AGL = height)


#- filter data - keep only short-pulse
t_pulsetype <- unique(echoData$pulseType)
# if( any(t_pulsetype == "M") ) message("Ignore echoes registered with medium pulse. \n...")
if( any(t_pulsetype == "L") ) message("Ignore echoes registered with medium pulse. \n...")

# pulseLengthSelection = c("S") # keep only Short-pulse data >> otherwise, need to build a for-loop.
echoData_s <- echoData %>% 
  filter(pulseType == "S")
if( nrow(echoData_s) > 0 ) {
  message( paste0(nrow(echoData_s), " echoes registered with *SHORT PULSE*, including all classes: \n") )
  print( table(echoData_s$class, useNA = "ifany") )
}
if(nrow(echoData_s) == 0) stop("No echo registreed with short-pulse. No echo remaining for MTR computation. \n If you want to get MTR using Medium/long pulse, change the script accordingly.")

echoData_m <- echoData %>% 
  filter(pulseType == "M")
if( nrow(echoData_m) > 0 ) {
  message( paste0(nrow(echoData_m), " echoes registered with *MEDIUM PULSE*, including all classes: \n") )
  print( table(echoData_m$class, useNA = "ifany") )
}
if(nrow(echoData_m) == 0) stop("No echo registreed with *MEDIUM PULSE*. No echo remaining for MTR computation. \n If you want to get MTR using Medium/long pulse, change the script accordingly.")


## --------------- Get protocol and visibility table ---------------------

#define protocol filter
prot_filter <- echoData$protocolID

#extract protocol
protocolData <-
  dbo_protocol %>%
  select(my_col_dbo_protocol$colname_org) %>% 
  rename_all(~my_col_dbo_protocol$colname_new) %>%
  collect()%>%
  filter(protocolID %in% prot_filter)

#- Convert timeZone - reformat time variable
protocolData = convertTimeZone(data = protocolData, colNames = c("startTime", "stopTime"),
                       originTZ = radarTimeZone, targetTZ = targetTimeZone)
protocolData_s <- protocolData %>% 
  filter(pulseType == "S")
protocolData_m <- protocolData %>% 
  filter(pulseType == "M")

#extract visibility table
visibilityData <-
  dbo_visibility %>%
  collect()%>%
  filter(protocolID %in% prot_filter)

#- Convert timeZone - reformat time variable
visibilityData = convertTimeZone(data = visibilityData, colNames = c("blind_from", "blind_to"),
                                originTZ = radarTimeZone, targetTZ = targetTimeZone)


# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# --------------------------- MTR computation --------------------------
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::



##------------------------Set parameters for MTR computation--------------------

# define the parameters for data extraction. This section is based on the vignette
# of the birdscanR package :
# https://cran.r-project.org/web/packages/birdscanR/vignettes/mtrCalculationWorkflow.html

radarTimeZone  = radarTimeZone # see above

# Set target timezone for the radar/dataset
targetTimeZone = targetTimeZone # see above

# Geographic location of the radar measurements - c(Latitude, Longitude)
siteLocation = siteLocation # see above

# Set type of twilight to use for day/night decision, i.e., "sun" or "civil"
sunOrCivil = sunOrCivil # see above

# Set desired pulse Length modes
pulseLengthSelection = "S" # per default short-pulse

# Set desired rotation modes (multiple simultaneous selections possible)
# options: 1 (rotation), 0 (nonrotation)
rotationSelection    = c(1, 0) # add non-rotation mode that was often used in the past.

# Set classes to use in the analysis
classSelection = classSelection # see above

# Set the classification probability cutoff (between 0 and 1), NULL for no cutoff
classProbCutoff = NULL

# Set the altitude range (in meters agl)

altitudeRange_s = c(50, 1500) # short-pulse
altitudeRange_m = c(150, 2000) # short-pulse
# if(pulseLengthSelection == "S"){
#   altitudeRange = c(50, 1500) # short-pulse
# } else{
#   if(pulseLengthSelection == "M"){
#     altitudeRange = c(100, 2000) # medium pulse > theoretical max: 3000
#   } else{
#     altitudeRange = c(200, 3000) # long-pulse > theoretical max: 6000
#   }
# }

# Set the time range for echodata (in the targetTimeZone)
# use format "yyyy-MM-dd hh:mm"
timeRangeTargetTZ = timeRangeTargetTZ # see above

# toDelete
# # Set whether to get the manual blind time from:
# # "mandb": the db file;
# # "csv"  : a separate csv file
# manBlindSource  = "csv"
# 
# # Set paths to manual blind times file, if manBlindSource == "csv"
# if (manBlindSource %in% "csv"){
#   manblindFile = file.path("data", "manualBlindTimes.csv")
# }
# 
# # set blindtime types which should not be treated as blindtime but MTR = 0
# blindTimeAsMtrZero = c("rain") # not relevent for realttime MTR compuation.

# Set whether to use the echoValidator - If set to TRUE, echoes labelled
# by the echo validator as "non-bio scatterer" will be excluded.
useEchoValidator = FALSE

# Set whether to save the blind times to file
saveBlindTimes = TRUE

# Set altitude Range and Bin Size for the MTR calculations
# altitudeRange.mtr = altitudeRange
# altitudeBinSize   = 50 # see bleow >> bin size is specified ahead of each computeMTR function

# time range for timeBins (targetTimeZone) - format: "yyyy-MM-dd hh:mm"
timeRangeTargetTZ  = timeRangeTargetTZ # see above

# # timeBin size in seconds
# timeBinduration_sec = 3600 # see bleow >> timeBin size is specified ahead of each computeMTR function

# Set whether to compute MTR per timebin or per day/night
# TRUE: MTR is computed per day and night;
# FALSE: MTR is computed for each time bin
# computePerDayNight = FALSE # see bleow >> timeBin size is specified ahead of each computeMTR function

# cutoff for proportional observation times: Ignore TimeBins where
# "observationTime/timeBinDuration < propObsTimeCutoff"
# in 'computeMTR' only used if parameter 'computePerDayNight' is set to
# 'TRUE'
propObsTimeCutoff = 0

# Set classes for which you want the MTR
classSelection.mtr = classSelection # see above

# toDelete
# Set whether to save the MTR to file
# saveMTR2File = TRUE



##------------------------------Compute hourly MTR------------------------------

#altitudeChunkId, timeChunkId,

altitudeRange = altitudeRange_s
# altitudeBin size in meter
altitudeBinSize   = 50 
# timeBin size in second
timeBinduration_sec = 3600
# whether to aggregate the data to day/night periods instead of using 'timeBinduration_sec'
computePerDayNight = FALSE

MTR_50m_1h <- computeMTR(dbName = dbName,
                         echoes = echoData[ , c("time_stamp_targetTZ", "feature1.altitude_AGL", "class","mtr_factor_rf")], # these are the fou colnames required for the function computeMTR.
                         classSelection = classSelection.mtr,
                         altitudeRange = altitudeRange,
                         altitudeBinSize = altitudeBinSize,
                         timeRange = timeRangeTargetTZ,
                         timeBinDuration_sec = timeBinduration_sec,
                         timeZone = targetTimeZone,
                         sunriseSunset = sunriseSunset, # as given by the twilight-function
                         sunOrCivil = "civil",
                         protocolData = protocolData, # necessary columns selected above
                         visibilityData = visibilityData, # necessary columns selected above
                         manualBlindTimes = NULL,
                         saveBlindTimes = FALSE,
                         blindTimesOutputDir = getwd(),
                         blindTimeAsMtrZero = NULL,
                         propObsTimeCutoff = 0,
                         computePerDayNight = computePerDayNight,
                         computeAltitudeDistribution = FALSE)

##---------------------------Compute Day-Night MTR-----------------------------
altitudeBinSize = 1450
computePerDayNight = TRUE
# timeBinduration_sec

MTR_dayNight <- computeMTR(dbName = dbName,
                           echoes = echoData[, c("time_stamp_targetTZ", "feature1.altitude_AGL", "class","mtr_factor_rf")], # these are the fou colnames required for the function computeMTR.,
                           classSelection = classSelection.mtr,
                           altitudeRange = altitudeRange,
                           altitudeBinSize = altitudeBinSize,
                           timeRange = timeRangeTargetTZ,
                           timeBinDuration_sec = timeBinduration_sec,
                           timeZone = targetTimeZone,
                           sunriseSunset = sunriseSunset,  # as given by the twilight-function
                           sunOrCivil = "civil",
                           protocolData = protocolData, # necessary columns selected above
                           visibilityData = visibilityData, # necessary columns selected above
                           manualBlindTimes = NULL,
                           saveBlindTimes = FALSE,
                           blindTimesOutputDir = getwd(),
                           blindTimeAsMtrZero = NULL,
                           propObsTimeCutoff = 0,
                           computePerDayNight = computePerDayNight,
                           computeAltitudeDistribution = FALSE)

##------------------------------Read MTR--------------------------------------

# MTR_50m_1h <- readRDS('./computed_mtr.rds')
#
# summary(MTR_50m_1h$timeChunkDuration_sec)
# sum(MTR_50m_1h$timeChunkDuration_sec != 3600)
#
# col_mtr[which(!col_mtr %in% colnames(MTR_50m_1h))]
# colnames(MTR_50m_1h)[which(!colnames(MTR_50m_1h) %in% col_mtr)]

##------------------------------Save FTP---------------------------------------

# write CSVs
filename_hourly <- paste0("RT_SEM_MTR_1hour_", date_tag, ".csv")
filename_dayNight <- paste0("RT_SEM_MTR_dayNight_", date_tag, ".csv")

write.csv(MTR_50m_1h, file= file.path(mainOutputDir, filename_hourly))
write.csv(MTR_dayNight, file= file.path(mainOutputDir, filename_dayNight))

# ftpUpload("Localfile.html", "ftp://User:Password@FTPServer/Destination.html")
url.ftp <- "ftp://transfer.vogelwarte.ch/ForBesucherZentrum/Forecast"
# userpwd <- paste0(Sys.getenv("ftp_user"), ":", Sys.getenv("ftp_key"))
userpwd <- "radar:radarTr@ns"

message(". \n .. \n .. \n upload hourly MTR")



ftpUpload(file.path(mainOutputDir, filename_hourly), file.path(url.ftp, filename_hourly), userpwd=userpwd)
message(". \n .. \n .. \n  hourly MTR uploaded")

message(". \n .. \n .. \n upload dayNight MTR")

ftpUpload(file.path(mainOutputDir, filename_dayNight), file.path(url.ftp, filename_dayNight), userpwd=userpwd)
message(". \n .. \n .. \n  dayNight MTR uploaded")

Sys.sleep(30)


#--------------------------------------
# ORNITHO.ch & BZ-exhibition


# reformat for Ornitho.ch
classSelection.mtr <- c("passerine_type", "wader_type", "swift_type", "large_bird", "unid_bird", "bird_flock")

# Flight direction
# ,"echoID","Class","is_night","dir_degree","dir_radian"
# 114488,"echo330_0001052370","Bird",0,173.803,173.803
# $class > ifelse(... %in% c("passerine_type", "wader_type", "swift_type","large_bird", "unid_bird", "bird_flock"), 
#                 'Bird' else 'NA'
# $is_night > ifelse(... =='night, 1, 0)
# $dir_radian not used? at least not convertred to radian...


# MTR twilight
# ,"Tint","Hint","Class","OperMod","Eweight","nEcho","is_night","twilightDate","tstepS","RecS","RecH","tstepH","mtr","meanAzim","sdAzim.Mardia72","sdAzim.Batschelet84","medAzim","meanSpeed","sdSpeed","medSpeed","nFlightBehav"
# remove aggregated direction

NOW <- floor_date(NOW, "hour") 
tzOffSet <- "+0000" # radar run since 2021 in UTC
tzOffSet.sec <- 0
NOW.ltz <- paste(format(NOW+tzOffSet.sec, format = "%Y-%m-%d %H:%M:%OS"), tzOffSet, sep=" ") # add the hour difference to UTC
NOW.ltz.ct <- NOW + tzOffSet.sec # local time in numeric format ('ct'), note that the arguement ('UTC') is wrong! see tzOffSet.sec for time difference to UTC   
# toDelete - see above
# IS_SUMMERTIME <- ifelse(NOW > as.POSIXct("2024-03-31 01:00", format="%Y-%m-%d %H:%M") & NOW < as.POSIXct("2024-10-27 02:00", format="%Y-%m-%d %H:%M"), TRUE, FALSE)

Ndays <- 30
timePeriod_24h <- 60*60*24 # last 24 hours in sec
timePeriod <- timePeriod_24h * Ndays # last Ndays (in sec)

tmin <- NOW - 24*60*60 # tmin <- twilight$tstart[twilight$Date=="2016-03-01" & twilight$is_night==0]
tmax <- NOW # tmax <- twilight$tstop[twilight$Date=="2016-05-31" & twilight$is_night==1]

# MTR_hour
##------------------------------Compute hourly MTR------------------------------

# timeBin size in second
timeBinduration_sec = 3600
# whether to aggregate the data to day/night periods instead of using 'timeBinduration_sec'
computePerDayNight = FALSE
# altitudeBin size in meter
Hstep = altitudeBinSize  = 100 

#- SHORT pulse
altitudeRange = c(50, 850) 
MTR_100m_1h_short <- computeMTR(dbName = dbName,
                                echoes = echoData[, c("time_stamp_targetTZ", "feature1.altitude_AGL", "class","mtr_factor_rf")], # these are the fou colnames required for the function computeMTR.,
                                classSelection = classSelection.mtr,
                                altitudeRange = altitudeRange,
                                altitudeBinSize = Hstep,
                                timeRange = timeRangeTargetTZ,
                                timeBinDuration_sec = timeBinduration_sec,
                                timeZone = targetTimeZone,
                                sunriseSunset = sunriseSunset,
                                sunOrCivil = "civil",
                                protocolData = protocolData,
                                visibilityData = visibilityData,
                                manualBlindTimes = NULL,
                                saveBlindTimes = FALSE,
                                blindTimesOutputDir = getwd(),
                                blindTimeAsMtrZero = NULL,
                                propObsTimeCutoff = 0,
                                computePerDayNight = computePerDayNight,
                                computeAltitudeDistribution = FALSE)
#- MEDIUM pulse
altitudeRange = altitudeRange = c(850, 2000) 
MTR_100m_1h_medium <- computeMTR(dbName = dbName,
                                echoes = echoData[, c("time_stamp_targetTZ", "feature1.altitude_AGL", "class","mtr_factor_rf")], # these are the fou colnames required for the function computeMTR.,
                                classSelection = classSelection.mtr,
                                altitudeRange = altitudeRange,
                                altitudeBinSize = Hstep,
                                timeRange = timeRangeTargetTZ,
                                timeBinDuration_sec = timeBinduration_sec,
                                timeZone = targetTimeZone,
                                sunriseSunset = sunriseSunset,
                                sunOrCivil = "civil",
                                protocolData = protocolData,
                                visibilityData = visibilityData,
                                manualBlindTimes = NULL,
                                saveBlindTimes = FALSE,
                                blindTimesOutputDir = getwd(),
                                blindTimeAsMtrZero = NULL,
                                propObsTimeCutoff = 0,
                                computePerDayNight = computePerDayNight,
                                computeAltitudeDistribution = FALSE)

#- rbind and reorder by Tint and Hint
MTR_100m_1h <- rbind(MTR_100m_1h_short, MTR_100m_1h_medium)
MTR_100m_1h <- MTR_100m_1h[order(MTR_100m_1h$timeChunkBegin, MTR_100m_1h$altitudeChunkBegin),]


#- rename some variables
MTR_100m_1h["Tint"] <- MTR_100m_1h$timeChunkBegin
MTR_100m_1h["Hint"] <- MTR_100m_1h$altitudeChunkBegin
MTR_100m_1h$Class <- "Bird"
MTR_100m_1h$OperMod <- pulseLengthSelection
MTR_100m_1h["mtr"] <- MTR_100m_1h$mtr.allClasses
MTR_100m_1h["Eweight"] <- MTR_100m_1h$sumOfMTRFactors.allClasses
MTR_100m_1h["nEcho"] <- MTR_100m_1h$nEchoes.allClasses
MTR_100m_1h["is_night"] <- ifelse(MTR_100m_1h$dayOrNight == 'night', 1, 0)
MTR_100m_1h["twilightDate"] <- MTR_100m_1h$timeChunkDateSunset
MTR_100m_1h["tstepS"] <- MTR_100m_1h$timeChunkDuration_sec
MTR_100m_1h["RecS"] <- MTR_100m_1h$observationTime_sec
MTR_100m_1h["RecH"] <- MTR_100m_1h$observationTime_h
MTR_100m_1h["tstepH"] <- MTR_100m_1h$timeChunkDuration_sec / (60*60)

t_vars <- c("Tint", "Hint", "Class", "OperMod", "Eweight", "nEcho", "is_night", "twilightDate", "tstepS", "RecS", "RecH", "tstepH", "mtr") 
MTR <- MTR_100m_1h[, t_vars]


# Hstep <- Hint$hstep
Hbuffer <- 0 # add buffer to avoid overlap with next Hint
mtr.scale <- 10 # scale mtr value to reduce the point density
pdata <- data.frame("timeUTC"=as.integer(),"height"=as.numeric())
for(i in 1:nrow(MTR)){
  # i <- 1
  i.hmin <- MTR$Hint[i]-(Hstep/2)
  i.hmax <- MTR$Hint[i]+(Hstep/2)-Hbuffer
  i.Tint <- MTR$Tint[i] # Beginning of Tint, in UTC
  i.mtr <- round(MTR$mtr[i]/mtr.scale, digit=0)
  if(!is.finite(i.mtr)) i.mtr <- 0
  if(i.mtr < 0) next
  i.data <- data.frame("timeUTC"=rep(i.Tint, i.mtr)+runif(n=i.mtr, min=0, max=60*60), "height"=runif(n=i.mtr, min=i.hmin, max=i.hmax))
  
  pdata <- rbind(pdata, i.data)
  
  #  rm(i.hmin, i.hmax, i.Tint, i.mtr, i.data)
}
pdata$hourToNow <- as.numeric(pdata$timeUTC - NOW, units="hours") # in UTC
#pdata$hour.ltz <- lubridate::hour(pdata$timeUTC) + 1 # see time offset >>>> tzOffSet

pdata <- pdata[which(pdata$timeUTC >= tmin),]

blindTime <- data.frame("timeUTC"=MTR$Tint[MTR$mtr%in%NA])
blindTime$hourToNow <- as.numeric(blindTime$time - NOW, units="hours")# in UTC
#blindTime$hour.ltz <- lubridate::hour(blindTime$time) + 1 # see time offset >>>> tzOffSet



# save data
#  save(NOW, tmin, tmax, pdata, blindTime, file=paste0(format(NOW, format="%Y%m%d%H"),"UTC_24StdZug_BZ_small",".rData"))


#---------------------- SAVE BZ-Plot ------------------------------------#
# save plot
filename <- paste0(format(NOW, format="%Y%m%d%H"),"UTC_24StdZug_BZ_small",".png")
png(filename = file.path(plot_dir, filename), width = 1088, height = 1000, pointsize = 12, bg = "white",  res = NA)
par(omi=c(1,1,0,0), mai=c(0.3,0.3,0.1,0.1))
# plot bird distributions
tnow <- NOW  # in UTC
xtime <- seq(tnow, tnow-(23*60*60), -(60*60)) 
Xlab <- data.frame("time"=xtime,"hourToNow"=as.numeric(xtime - tnow, units="hours"),"xlab"=lubridate::hour(xtime)+1,"xlab.summer"=lubridate::hour(xtime+(1*60*60))+1)
#  Xlab$xlab[Xlab$xlab==0] <- 24 # label midnight as "24" instead of "0"
Xrange <- c(-24,0)
Yrange <- c(0,1400)
lineAxis <- -1
lineText <- 3.5
cexAxis <- 1.5
cexText <- 2.3
# plot axes
plot(Xrange, Yrange, type="n", bty="n", main="", xlab="",ylab="",xaxt="n",yaxt="n", adj=0) #
axis(1, at=Xlab$hourToNow, tick=TRUE, tcl=-0.25, labels=NA, line=lineAxis, las=1)
if(IS_SUMMERTIME){
  axis(1, at=c(Xlab$hourToNow[seq(1,24,2)],-24), tick=TRUE, tcl=-0.5, labels=NA, line=lineAxis, las=1)
  axis(1, at=c(Xlab$hourToNow[seq(1,24,2)],-24), tick=FALSE, tcl=-0.5, labels=c(Xlab$xlab.summer[seq(1,24,2)],Xlab$xlab.summer[1]), line=lineAxis, las=1, cex.axis=cexAxis)
}else{
  axis(1, at=c(Xlab$hourToNow[seq(1,24,2)],-24), tick=TRUE, tcl=-0.5, labels=NA, line=lineAxis, las=1)
  axis(1, at=c(Xlab$hourToNow[seq(1,24,2)],-24), tick=FALSE, tcl=-0.5, labels=c(Xlab$xlab[seq(1,24,2)],Xlab$xlab[1]), line=lineAxis, las=1, cex.axis=cexAxis)
}
mtext("Die vergangenen 24-Stunden",side=1, line=lineText-1, cex=cexText)
axis(2, at=seq(Yrange[1],Yrange[2],200), tick=TRUE, tcl=-0.5, labels=NA, line=lineAxis, las=1)
axis(2, at=seq(Yrange[1],Yrange[2],200), tick=FALSE, tcl=-0.5, labels=seq(Yrange[1],Yrange[2],200), line=lineAxis, las=1, cex.axis=cexAxis)
mtext("H?he (Meter ?ber Boden)",side=2, line=lineText, cex=cexText)
# add blindTime
if(nrow(blindTime)>0){
  for(i in 1:nrow(blindTime)){
    i.x <- blindTime$hourToNow
    rect(xleft=i.x, ybottom=-20, xright=i.x+1, ytop=0, col="grey70", border=NA)
  }
}
# add twilight - in UTC!!! should be same as for 'tnow' defined above
date.range = c(tmin, tmax)
 # LonLat <- matrix(c(8.192493, 47.127779), nrow = 1)
lon_lat = data.frame(X = siteLocation[2], Y = siteLocation[1])
crds    = sp::CRS(paste0("+proj=longlat +datum=", "WGS84")) # crs_datum = "WGS84"
lon_lat = sp::SpatialPoints(lon_lat, proj4string = crds) 
dusk.sun = suntools::crepuscule(crds        = lon_lat, 
                                dateTime    = tmin, 
                                solarDep    = 0, 
                                direction   = "dusk", 
                                POSIXct.out = TRUE)$time  
dusk.civil = suntools::crepuscule(crds        = lon_lat, 
                                  dateTime    = tmin, 
                                  solarDep    = 6, 
                                  direction   = "dusk", 
                                  POSIXct.out = TRUE)$time  
dawn.civil = suntools::crepuscule(crds        = lon_lat, 
                                  dateTime    = tmax, 
                                  solarDep    = 6, 
                                  direction   = "dawn", 
                                  POSIXct.out = TRUE)$time  
dawn.sun = suntools::crepuscule(crds        = lon_lat, 
                                dateTime    = tmax, 
                                solarDep    = 0, 
                                direction   = "dawn", 
                                POSIXct.out = TRUE)$time  
# define overlay of twilight-rectangles...
TransparencyScale <- 1.2
Nstep <- 50 # 100
twilightMin <- round(abs(as.numeric(dawn.civil - dawn.sun, units="mins")), digits=0)
xstep <- twilightMin/(Nstep*60) #
# plot the twilight-rectangles
if(dawn.sun < dusk.sun & dawn.civil > dusk.civil){
  for(i in 0:(Nstep-1)){
    minFromDusk.s <- round(abs(as.numeric(dusk.sun - tnow, units="mins")))
    iLim <- round(100*minFromDusk.s/twilightMin, digits=0)# number of rectangle-iteration until tnow
    if(i <= iLim){
      # from night 'til dawn
      xleft = -24
      xright = as.numeric(dawn.sun - tnow, units="hours") + lubridate::minute(dawn.sun)/60 - i*xstep
      ybottom = Yrange[1]
      ytop = Yrange[2]
      rect(xleft=xleft, ybottom=ybottom, xright=xright, ytop=ytop, col=alpha("dodgerblue4", 0.2), border=NA)
      # from dusk towards night
      xleft = as.numeric(dusk.sun - tnow, units="hours") + lubridate::minute(dusk.sun)/60 + i*xstep
      xright = 0
      ybottom = Yrange[1]
      ytop = Yrange[2]
      rect(xleft=xleft, ybottom=ybottom, xright=xright, ytop=ytop, col=alpha("dodgerblue4", 0.2), border=NA)
    } else {
      xleft = as.numeric(dusk.sun - tnow, units="hours") + lubridate::minute(dusk.sun)/60 + i*xstep
      xright = as.numeric(dawn.sun - tnow, units="hours") + lubridate::minute(dawn.sun)/60 - i*xstep
      ybottom = Yrange[1]
      ytop = Yrange[2]
      rect(xleft=xleft, ybottom=ybottom, xright=xright, ytop=ytop, col=alpha("dodgerblue4", 0.2), border=NA)
    }
  }
}
if(dawn.sun > dusk.sun & dawn.civil < dusk.civil){
  for(i in 0:(Nstep-1)){
    minFromDawn.c <- round(abs(as.numeric(dusk.sun - tnow, units="mins")))
    iLim <- round(100*(1-(minFromDawn.c/twilightMin)), digits=0)# number of rectangle-iteration until tnow
    if(i < iLim){
      # from night 'til dawn
      xleft = -24
      xright = as.numeric(dawn.sun - tnow, units="hours") + lubridate::minute(dawn.sun)/60 - i*xstep
      ybottom = Yrange[1]
      ytop = Yrange[2]
      rect(xleft=xleft, ybottom=ybottom, xright=xright, ytop=ytop, col=alpha("dodgerblue4", 1/(Nstep*2)), border=NA)
      # from dusk towards night
      xleft = as.numeric(dusk.sun - tnow, units="hours") + lubridate::minute(dusk.sun)/60 + i*xstep
      xright = 0
      ybottom = Yrange[1]
      ytop = Yrange[2]
      rect(xleft=xleft, ybottom=ybottom, xright=xright, ytop=ytop, col=alpha("dodgerblue4", 0.2), border=NA)
    } else {
      xleft = as.numeric(dusk.sun - tnow, units="hours") + lubridate::minute(dusk.sun)/60 + i*xstep
      xright = as.numeric(dawn.sun - tnow, units="hours") + lubridate::minute(dawn.sun)/60 - i*xstep
      ybottom = Yrange[1]
      ytop = Yrange[2]
      rect(xleft=xleft, ybottom=ybottom, xright=xright, ytop=ytop, col=alpha("dodgerblue4", 0.2), border=NA)
    }
  }
}
if(dawn.sun > dusk.sun & dawn.civil > dusk.civil){
  for(i in 0:Nstep-1){
    xleft = as.numeric(dusk.sun - tnow, units="hours") + lubridate::minute(dusk.sun)/60 + i*xstep
    xright = as.numeric(dawn.sun - tnow, units="hours") + lubridate::minute(dawn.sun)/60 - i*xstep
    ybottom = Yrange[1]
    ytop = Yrange[2]
    rect(xleft=xleft, ybottom=ybottom, xright=xright, ytop=ytop, col=alpha("dodgerblue4", 0.2), border=NA)
  }
}
if(dawn.sun < dusk.sun & dawn.civil < dusk.civil){
  for(i in 0:Nstep-1){
    # from night 'til dawn
    xleft = -24
    xright = as.numeric(dawn.sun - tnow, units="hours") + lubridate::minute(dawn.sun)/60 - i*xstep
    ybottom = Yrange[1]
    ytop = Yrange[2]
    rect(xleft=xleft, ybottom=ybottom, xright=xright, ytop=ytop, col=alpha("dodgerblue4", 0.2), border=NA)
    # from dusk towards night
    xleft = as.numeric(dusk.sun - tnow, units="hours") + lubridate::minute(dusk.sun)/60 + i*xstep
    xright = 0
    ybottom = Yrange[1]
    ytop = Yrange[2]
    rect(xleft=xleft, ybottom=ybottom, xright=xright, ytop=ytop, col=alpha("dodgerblue4", 0.2), border=NA)
  }
}
# add data
# points(jitter(pdata$hourToNow+0.5, factor=2), pdata$height, pch="_", cex=2.5)

#  check is current graphic device  
#  dev.capabilities(what=c("semiTransparency","transparentBackground","rasterImage","capture"))

#img <-  readPNG(file.path("C:/RadarData/CH_Sempach/Rtime", "BirdSketch_RadarPlot_thick.png"), native = FALSE)  #readPNG(paste(fct_folder, "BirdSketch_RadarPlot_thick.png", sep="/"), native = FALSE)
img.n <- readPNG(file.path("C:/RadarData/CH_Sempach/Rtime", "BirdSketch_RadarPlot_thick.png"), native = TRUE) #readPNG(paste(fct_folder, "BirdSketch_RadarPlot_thick.png", sep="/"), native = TRUE)
for(i in 1:nrow(pdata)){
  # i <- nrow(pdata)-2
  i_time <- pdata$hourToNow[i]
  xadj <- 0.5/2
  i_height <- pdata$height[i]
  yadj <- 20/2
  #    points(i_time, i_height, pch=19, cex=3, col="grey50")
  # rdmly working             
  graphics::rasterImage(img.n, i_time-xadj, i_height-yadj, i_time+xadj, i_height+yadj, interpolate=TRUE)
  ## not yet working    grid::grid.raster(img.n, x= X-xadj, y= Y-yadj, width=2*xadj, height=2*yadj, interpolate=FALSE)
}

dev.off()

message(". \n .. \n .. \n .. \n .. \n .. \n plot saved, upload to ftp pending")
Sys.sleep(15)

# ftpUpload("Localfile.html", "ftp://User:Password@FTPServer/Destination.html")
url.ftp <- "ftp://transfer.vogelwarte.ch/ForBesucherZentrum/Plots_24StdZugUeberBZ"
userpwd <- "radar:radarTr@ns"
ftpUpload(file.path(plot_dir, filename), file.path(url.ftp, filename), userpwd=userpwd)

# sessionInfo()
# RStudio.Version()

message(". \n .. \n .. \n .. \n .. \n .. \n plot saved, upload to ftp done")
Sys.sleep(15)




#----------------------
#- BIOLOVISION - Ornitho table

#------------------------------------------------------------------------------#
# MTR per hour and 100 m step

# #- define time interval
# tmin <- NOW - timePeriod # tmin <- twilight$tstart[twilight$Date=="2016-03-01" & twilight$is_night==0]
# tmax <- NOW # tmax <- twilight$tstop[twilight$Date=="2016-05-31" & twilight$is_night==1]
# tstep <-  1*60*60  # how to do when time step is variable (e.g. day/night length)?
# Tint <- data.frame("tmin" = tmin, "tmax" = tmax, "tstep" = tstep) # give twighlight info in similar format
# #- need to calcualte the twilight because at the beginning of the year, the DB is new and do not contain any data for the entire month, so taht the time period of the object "twilight" (as computed above) do not include the entire time range.
# Date_min <- as.POSIXct(strptime(substr(Tint$tmin,1,10), tz="UTC", format="%Y-%m-%d"))
# Date_max <- as.POSIXct(strptime(substr(Tint$tmax,1,10), tz="UTC", format="%Y-%m-%d"))
# twilight_1month <- twilight(latLon = c(46.168993, 5.983040), 
#                             timeRange = seq(Date_min, Date_max, "day"), 
#                             crs_datum = "WGS84", timeZone = 'UTC')
# 



# write CSVs
MTR_hour<- MTR # see above, already computed for plotting only the BZ-png
date_tag <- format(NOW, "%Y%m%d")
filename <- paste0("SEM_MTR_hour_", date_tag, ".csv")
filepath <- file.path(plot_dir, filename)
write.csv(MTR_hour, file=filepath)

message(". \n .. \n .. \n .. \n .. \n .. \n hourly MTR saved, upload to ftp pending")
Sys.sleep(15)


# ftpUpload("Localfile.html", "ftp://User:Password@FTPServer/Destination.html")
url.ftp <- "ftp://transfer.vogelwarte.ch/ForBesucherZentrum/Tables_Biolovision_ZugUeberBZ"
userpwd <- "radar:radarTr@ns"
ftpUpload(filepath, file.path(url.ftp, filename), userpwd=userpwd)

message(". \n .. \n .. \n .. \n .. \n .. \n hourly MTR saved, upload to ftp done")
Sys.sleep(15)



#------------------------------------------------------------------------------#
# MTR per sunet/sunrise and all heights

altitudeRange <- c(50, 1500)
Hstep <- 1450
MTR_vi_1h <- computeMTR(dbName = dbName,
                          echoes = echoData[, c("time_stamp_targetTZ", "feature1.altitude_AGL", "class","mtr_factor_rf")], # these are the fou colnames required for the function computeMTR.,,
                          classSelection = classSelection.mtr,
                          altitudeRange = altitudeRange,
                          altitudeBinSize = Hstep,
                          timeRange = timeRangeTargetTZ,
                          timeBinDuration_sec = -1,
                          timeZone = targetTimeZone,
                          sunriseSunset = sunriseSunset,
                          sunOrCivil = "civil",
                          protocolData = protocolData,
                          visibilityData = visibilityData,
                          manualBlindTimes = NULL,
                          saveBlindTimes = FALSE,
                          blindTimesOutputDir = getwd(),
                          blindTimeAsMtrZero = NULL,
                          propObsTimeCutoff = 0,
                          computePerDayNight = FALSE,
                          computeAltitudeDistribution = FALSE)
# rename some variables

MTR_vi_1h[,"Tint"] <- MTR_vi_1h$timeChunkBegin
MTR_vi_1h[,"Hint"] <- MTR_vi_1h$altitudeChunkBegin
MTR_vi_1h$Class <- "bird"
MTR_vi_1h$OperMod <- pulseLengthSelection
MTR_vi_1h[,"mtr"] <- MTR_vi_1h$mtr.allClasses
MTR_vi_1h[,"Eweight"] <- MTR_vi_1h$sumOfMTRFactors.allClasses
MTR_vi_1h[,"nEcho"] <- MTR_vi_1h$nEchoes.allClasses
MTR_vi_1h[,"is_night"] <- ifelse(MTR_vi_1h$dayOrNight == 'night', 1, 0)
MTR_vi_1h[,"twilightDate"] <- MTR_vi_1h$timeChunkDateSunset
MTR_vi_1h[,"tstepS"] <- MTR_vi_1h$timeChunkDuration_sec
MTR_vi_1h[,"RecS"] <- MTR_vi_1h$observationTime_sec
MTR_vi_1h[,"RecH"] <- MTR_vi_1h$observationTime_h
MTR_vi_1h["tstepH"] <- MTR_vi_1h$timeChunkDuration_sec / (60*60)

MTR_vi_1h[,c("meanAzim","sdAzim.Mardia72","sdAzim.Batschelet84","medAzim","meanSpeed","sdSpeed","medSpeed","nFlightBehav")] <- NA

t_vars <- c("Tint", "Hint", "Class", "OperMod", "Eweight", "nEcho", "is_night", "twilightDate", "tstepS", "RecS", "RecH", "tstepH", "mtr") 
flightvals <- c("meanAzim","sdAzim.Mardia72","sdAzim.Batschelet84","medAzim","meanSpeed","sdSpeed","medSpeed","nFlightBehav")
MTR_twilight <- MTR_vi_1h[, c(t_vars, flightvals)]


# write CSVs
filename <- paste0("SEM_MTR_twilight_", date_tag, ".csv")
filepath <- file.path(plot_dir, filename)
write.csv(MTR_twilight, file=filepath)

# ftpUpload("Localfile.html", "ftp://User:Password@FTPServer/Destination.html")
url.ftp <- "ftp://transfer.vogelwarte.ch/ForBesucherZentrum/Tables_Biolovision_ZugUeberBZ"
userpwd <- "radar:radarTr@ns"
ftpUpload(filepath, file.path(url.ftp, filename), userpwd=userpwd)



#------------------------------------------------------------------------------#
# Bird echoes with flight directions

echoData_bird <- echoData[which(echoData$class %in% classSelection.mtr), ]
echoData_bird$Class <- "Bird"
echoData_bird[,"is_night"] <- ifelse(echoData_bird$dayOrNight == 'night', 1, 0)
echoData_bird[,"dir_degree"] <- echoData_bird[,"direction"]
echoData_bird[,"dir_radian"] <- echoData_bird[,"dir_degree"] # echoData_bird[,"dir_degree"] * (pi/180)
echoData_bird$month_sunset <- lubridate::month(echoData_bird$dateSunset)

# filter by pulse length!
echoData_bird <- echoData_bird[which(echoData_bird$pulseType %in% "S"), ]

NOW_month <- lubridate::month(NOW)
Echo_flight <- echoData_bird[which(echoData_bird$month_sunset == NOW_month), c("echoID","Class","is_night","dir_degree", "dir_radian")]


# write CSVs
NOW_month <- tolower(as.character(lubridate::month(NOW, label=TRUE, abbr=FALSE)))
filename <- paste0("FlightDirection_", NOW_month, ".csv")
filepath <- file.path(plot_dir, filename)
write.csv(Echo_flight, file=filepath)

# ftpUpload("Localfile.html", "ftp://User:Password@FTPServer/Destination.html")
url.ftp <- "ftp://transfer.vogelwarte.ch/ForBesucherZentrum/Tables_Biolovision_ZugUeberBZ"
userpwd <- "radar:radarTr@ns"
ftpUpload(filepath, file.path(url.ftp, filename), userpwd=userpwd)


message(". \n .. \n .. \n .. \n .. \n .. \n MTR tables computed and uploaded")
Sys.sleep(15)
