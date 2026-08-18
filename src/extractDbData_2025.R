#---------------------------File description----------------------------------
# Extracts and saves the yearly archive snapshot for a Birdscan season, using
# birdscanR::extractDbData() as documented in the package vignette:
#   https://cran.r-project.org/web/packages/birdscanR/vignettes/mtrCalculationWorkflow.html
#
# Connects directly to the radar's live SQL database (unlike archive_MTR.R /
# archive_MTR_batch.R, which only ever READ the *_DataExtract.rds files this
# script produces) and writes:
#   <dbDataDir>/CH_Sempach_<year>_DataExtract.rds
# in the same format/location already used by the existing 2016-2025 archive
# files under U:/RadarData/CH_Sempach/<year>/Database/.
#
# Why this is needed: the current U:/RadarData/CH_Sempach/2025/Database/
# CH_Sempach_2025_DataExtract.rds only contains echoes up to 2025-08-13, even
# though the CH_Sempach_2025 database's own project window runs to ~2026-01-01
# - the RDS snapshot was simply never refreshed after that date. Re-running
# this script re-extracts the FULL season from the (by now closed, static) DB
# and overwrites the stale snapshot with a complete one.
#
# NOT executed/tested here: this machine has no network path to the radar
# server (BIRDSCAN2200) - only syntax-checked. Run this on a machine that can
# actually reach the SQL server (e.g. the same one the realtime script runs
# on), and sanity-check the printed summary at the end before trusting it.

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ------------------------------ Settings ------------------------------
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

##---------------------------Working directory---------------------------------

rm( list=ls() ); gc()


#link .Renviron file, for DB credentials (see 'DB connection' below)
readRenviron(".Renviron")


##----------------------------Season / DB settings------------------------------

# Season to (re-)extract. The corresponding SQL database is expected to be
# named "CH_Sempach_<year>", matching the existing archive files' naming.
year <- 2025
dbName <- paste0("CH_Sempach_", year)

# Output location: same convention as the existing yearly archive files, so
# archive_MTR.R / archive_MTR_batch.R pick this up automatically afterwards.
dbDataDir <- file.path("U:/RadarData/CH_Sempach", year, "Database")


##----------------------------- DB connection -----------------------------------

# Set either "SQL Server" or "PostgreSQL" - these are the only two values
# birdscanR::dbConnectBirdscanSQL() checks for; it is NOT the literal Windows
# ODBC driver name (unlike the realtime script's own hand-rolled DBI/odbc
# connection, which uses the more specific "SQL Server Native Client 11.0").
# If RODBC can't find a driver registered simply as "SQL Server" on this
# machine, that's the first thing to check - see RODBC::odbcDataSources().
dbDriverChar = "SQL Server"
dbServer     = "BIRDSCAN2200"

# Credentials: prefer .Renviron (db_id / db_key) if set there, else fall back
# to the same credentials already hard-coded in RealtTimeMTR_tidy_inclMpulse.R.
# If both dbUser/dbPwd end up NULL, dbConnectBirdscanSQL() will instead prompt
# interactively via rstudioapi::askForPassword() - which requires an active
# RStudio session, so don't leave both unset if this is meant to run unattended
# (e.g. from a scheduled task).
dbUser = Sys.getenv("db_id", unset = "sa")
dbPwd  = Sys.getenv("db_key", unset = "radar")
dbPort = 5432 # only used for PostgreSQL; ignored for "SQL Server"


##----------------------------Extraction settings--------------------------------

# Radar time zone: leave NULL to auto-detect from [dbo].[site].[timeShift] in
# the database (this is what extractDbData() does internally when NULL - do
# NOT hard-code "Etc/GMT0" here the way the vignette's generic example does;
# Sempach's own archives show the radar actually runs at "Etc/GMT-1").
radarTimeZone  = NULL
targetTimeZone = "Etc/GMT-1" # keep all MTR/echo timestamps in UTC, same as the other archive scripts

# NULL = don't extract any of the rf_feature columns (rcs/wbf/etc.) beyond
# the standard feature1-feature37 set already in the collection table - this
# matches what's actually present in the existing 2016-2024 archive files, so
# the new 2025 file stays structurally consistent with them.
listOfRfFeaturesToExtract = 'all'

# NULL = extract the FULL season (no time filter), matching the other yearly
# archive files (archive_MTR_batch.R assumes a year's file covers that whole
# year - a partial/incremental extract would silently break that assumption).
timeInterval = NULL

# Sempach site coordinates (c(Latitude, Longitude)) - same values used as the
# fallback default in the realtime and archive scripts.
siteLocation = c(47.127641, 8.192569)

sunOrCivil = "civil"
crepuscule = "nauticalSolar" # default; only affects the (unused downstream) crepuscule/day-night boundary refinement


##----------------------------Libraries----------------------------------------

list.of.packages <- c("birdscanR")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, library, character.only = TRUE)


# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# --------------------------- Extract & save -----------------------------
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

if( !dir.exists(dbDataDir) ) dir.create(dbDataDir, recursive = TRUE)

message( paste0("Extracting data from ", dbName, " (", dbServer, ") - this can take a while for a full season..." ) )

dbData = extractDbData(
  dbDriverChar              = dbDriverChar,
  dbServer                  = dbServer,
  dbName                    = dbName,
  dbUser                    = dbUser,
  dbPwd                     = dbPwd,
  dbPort                    = dbPort,
  saveDbToFile              = TRUE,
  dbDataDir                 = dbDataDir,
  radarTimeZone             = radarTimeZone,
  targetTimeZone            = targetTimeZone,
  timeInterval              = timeInterval,
  listOfRfFeaturesToExtract = listOfRfFeaturesToExtract,
  siteLocation              = siteLocation,
  sunOrCivil                = sunOrCivil,
  crepuscule                = crepuscule
)

message( paste0("Finished extracting data from ", dbName, ". Saved to: ",
                 file.path(dbDataDir, paste0(dbName, "_DataExtract.rds"))) )


# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# --------------------------- Sanity check -------------------------------
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

message( paste0(nrow(dbData$echoData), " echoes extracted.") )
message( paste0("echoData time range: ",
                 format(min(dbData$echoData$time_stamp_targetTZ, na.rm = TRUE)), " to ",
                 format(max(dbData$echoData$time_stamp_targetTZ, na.rm = TRUE))) )
message( paste0("site table project window: ",
                 format(min(dbData$siteData$projectStart_targetTZ, na.rm = TRUE)), " to ",
                 format(max(dbData$siteData$projectEnd_targetTZ, na.rm = TRUE))) )
message( paste0("radar time zone (auto-detected): ", dbData$TimeZone$radarTimeZone[1]) )
