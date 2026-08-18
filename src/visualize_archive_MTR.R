#---------------------------File description----------------------------------
# Visualises the MTR tables produced by archive_MTR.R / archive_MTR_batch.R
# (bird, insect, bat - short-pulse only). Does not touch the database or the
# archive *.rds files - it only reads the CSVs those scripts already wrote
# into archiveOutputDir, so it can be re-run any time after a batch (or single)
# archive run, independently of them.
#
# Produces two plots, each with one panel per class (bird / insect / bat):
#
# 1. Vertically-integrated MTR at day/night resolution, across whatever
#    year(s)/period(s) are present in archiveOutputDir - one point per
#    day/night chunk, connected within each contiguous year/period run (gaps
#    between different months are left as gaps, not bridged).
#    -> reads the "*_dayNight_*.csv" files (single 50-1500m altitude bin
#       already = vertically integrated).
#
# 2. Hourly diel (hour-of-day) pattern, averaged over the days within each
#    year x period, one line per year x period (colour = year, linetype =
#    period), so timing of activity can be compared across years and between
#    March vs September within each class.
#    -> reads the "*_1hour_*.csv" files (50m altitude bins) and sums mtr
#       across altitude bins per hour to vertically integrate them, since
#       there is no separate "hourly, vertically-integrated" product.

# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# ------------------------------ Settings ------------------------------
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

rm( list=ls() ); gc()
working_dir <- "C:/RadarData/CH_Sempach_2025/Rtime"
setwd(working_dir)

# where archive_MTR.R / archive_MTR_batch.R wrote their CSVs
archiveOutputDir <- file.path(working_dir, "archive_output")

# where to write the plots
plotOutputDir <- file.path(archiveOutputDir, "plots")
if( !dir.exists(plotOutputDir) ) dir.create(plotOutputDir, recursive = TRUE)

list.of.packages <- c("dplyr", "ggplot2", "lubridate")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, library, character.only = TRUE)


# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# --------------------------- Helpers -----------------------------------
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

# Filename prefixes for each class's dayNight/hourly CSVs, as written by
# archive_MTR.R / archive_MTR_batch.R. Anchored at the start so e.g.
# "ARCH_SEM_MTR_dayNight_" (bird) does not also match
# "ARCH_SEM_MTR_insect_dayNight_".
classFilePrefixes <- list(
  bird   = list(dayNight = "^ARCH_SEM_MTR_dayNight_",        hourly = "^ARCH_SEM_MTR_1hour_"),
  insect = list(dayNight = "^ARCH_SEM_MTR_insect_dayNight_", hourly = "^ARCH_SEM_MTR_insect_1hour_"),
  bat    = list(dayNight = "^ARCH_SEM_MTR_bat_dayNight_",    hourly = "^ARCH_SEM_MTR_bat_1hour_")
)

# Parses the "<year>_<period>" date tag out of a filename like
# "ARCH_SEM_MTR_insect_dayNight_2016_March.csv", given the matching prefix.
parseDateTag <- function(filename, prefix){
  tag <- sub("\\.csv$", "", sub(prefix, "", filename))
  parts <- strsplit(tag, "_")[[1]]
  list(year = as.integer(parts[1]), period = parts[2])
}

# computeMTR() writes exact-midnight timestamps without a time-of-day portion
# (e.g. "2024-03-01") but every other row with one (e.g. "2024-03-01 01:00:00"),
# so the same character column mixes both formats. base as.POSIXct() picks ONE
# format for the *whole* vector, silently truncating every value to midnight
# when the formats disagree - collapsing a day's 24 hourly rows into 1.
# lubridate::parse_date_time() parses each element separately, so it handles
# the mix correctly.
parseDateTimeMixed <- function(x){
  lubridate::parse_date_time(x, orders = c("Ymd HMS", "Ymd"), tz = "UTC")
}

# Reads and row-binds every dayNight CSV for one class, tagging each row with
# class/year/period. Returns columns: class, year, period, timeChunkBegin,
# dayOrNight, mtr (renamed from mtr.allClasses - for a single-bin
# vertically-integrated product this already sums over the classSelection
# used for that product, i.e. all 6 bird classes, or just "insect"/"bat").
readDayNightFiles <- function(className, prefix, archiveOutputDir){
  files <- list.files(archiveOutputDir, pattern = paste0(prefix, ".*\\.csv$"), full.names = FALSE)
  if(length(files) == 0){
    warning( paste0("No day/night files found for class '", className, "' (pattern: ", prefix, ")") )
    return(data.frame())
  }
  out <- lapply(files, function(f){
    tag <- parseDateTag(f, prefix)
    df <- read.csv(file.path(archiveOutputDir, f), stringsAsFactors = FALSE)
    if(nrow(df) == 0) return(NULL)
    data.frame(
      class = className,
      year = tag$year,
      period = tag$period,
      timeChunkBegin = parseDateTimeMixed(df$timeChunkBegin),
      dayOrNight = df$dayOrNight,
      mtr = df$mtr.allClasses
    )
  })
  dplyr::bind_rows(out)
}

# Reads and row-binds every hourly (50m-bin) CSV for one class, vertically
# integrating each hour by summing mtr.allClasses across altitude bins, then
# tags each row with class/year/period/hour-of-day.
readHourlyFiles <- function(className, prefix, archiveOutputDir){
  files <- list.files(archiveOutputDir, pattern = paste0(prefix, ".*\\.csv$"), full.names = FALSE)
  if(length(files) == 0){
    warning( paste0("No hourly files found for class '", className, "' (pattern: ", prefix, ")") )
    return(data.frame())
  }
  out <- lapply(files, function(f){
    tag <- parseDateTag(f, prefix)
    df <- read.csv(file.path(archiveOutputDir, f), stringsAsFactors = FALSE)
    if(nrow(df) == 0) return(NULL)
    df$timeChunkBegin <- parseDateTimeMixed(df$timeChunkBegin)
    df %>%
      dplyr::group_by(timeChunkBegin) %>%
      dplyr::summarise(mtr = sum(mtr.allClasses, na.rm = TRUE), .groups = "drop") %>%
      dplyr::mutate(class = className, year = tag$year, period = tag$period,
                    hour = lubridate::hour(timeChunkBegin))
  })
  dplyr::bind_rows(out)
}


# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# --------------------- Plot 1: day/night, vertically integrated --------
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

dayNightData <- dplyr::bind_rows(lapply(names(classFilePrefixes), function(cls){
  readDayNightFiles(cls, classFilePrefixes[[cls]]$dayNight, archiveOutputDir)
}))

if(nrow(dayNightData) == 0){
  warning("No day/night data found in archiveOutputDir - skipping plot 1. Run archive_MTR.R / archive_MTR_batch.R first.")
} else {
  dayNightData$class <- factor(dayNightData$class, levels = c("bird", "insect", "bat"))

  p1 <- ggplot(dayNightData, aes(x = timeChunkBegin, y = mtr)) +
    geom_line(aes(group = interaction(year, period)), colour = "grey60", linewidth = 0.4) +
    geom_point(aes(colour = dayOrNight), size = 1.4) +
    facet_wrap(~class, ncol = 1, scales = "free_y") +
    scale_colour_manual(values = c(day = "#f0a500", night = "#2c3e6b"), name = "") +
    labs(
      title = "Vertically-integrated MTR by day/night period",
      subtitle = "50-1500 m, short-pulse only. Gaps between segments are months not included in the archive run (only March/September were processed).",
      x = "Date", y = expression("MTR ("*birds/insects/bats~km^-1~h^-1*")")
    ) +
    theme_bw(base_size = 11) +
    theme(legend.position = "top", strip.text = element_text(face = "bold"))

  ggsave(file.path(plotOutputDir, "MTR_dayNight_verticallyIntegrated_byClass.png"),
         plot = p1, width = 9, height = 9, dpi = 150)
  message( paste0("Plot 1 (day/night, vertically integrated) saved to ",
                   file.path(plotOutputDir, "MTR_dayNight_verticallyIntegrated_byClass.png")) )
}


# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
# --------------------- Plot 2: hourly diel pattern ----------------------
# ::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::

hourlyData <- dplyr::bind_rows(lapply(names(classFilePrefixes), function(cls){
  readHourlyFiles(cls, classFilePrefixes[[cls]]$hourly, archiveOutputDir)
}))

if(nrow(hourlyData) == 0){
  warning("No hourly data found in archiveOutputDir - skipping plot 2. Run archive_MTR.R / archive_MTR_batch.R first.")
} else {
  # average the (vertically-integrated) hourly MTR across all days within
  # each class x year x period, by hour-of-day -> the "typical" diel curve
  dielData <- hourlyData %>%
    dplyr::group_by(class, year, period, hour) %>%
    dplyr::summarise(meanMtr = mean(mtr, na.rm = TRUE), .groups = "drop")

  dielData$class <- factor(dielData$class, levels = c("bird", "insect", "bat"))
  dielData$period <- factor(dielData$period, levels = c("March", "September"))

  p2 <- ggplot(dielData, aes(x = hour, y = meanMtr,
                             colour = year, linetype = period,
                             group = interaction(year, period))) +
    geom_line(linewidth = 0.7) +
    geom_point(size = 1) +
    facet_wrap(~class, ncol = 1, scales = "free_y") +
    scale_colour_viridis_c(name = "Year", option = "D") +
    scale_linetype_manual(values = c(March = "solid", September = "dashed"), name = "Period") +
    scale_x_continuous(breaks = seq(0, 23, 3)) +
    labs(
      title = "Hourly (diel) MTR pattern by month x year",
      subtitle = "Vertically integrated (50-1500 m, short-pulse only), averaged by hour-of-day across days within each month",
      x = "Hour of day (UTC)", y = expression("Mean MTR ("*birds/insects/bats~km^-1~h^-1*")")
    ) +
    theme_bw(base_size = 11) +
    theme(strip.text = element_text(face = "bold"))

  ggsave(file.path(plotOutputDir, "MTR_hourly_diel_byClass_yearPeriod.png"),
         plot = p2, width = 9, height = 11, dpi = 150)
  message( paste0("Plot 2 (hourly diel pattern) saved to ",
                   file.path(plotOutputDir, "MTR_hourly_diel_byClass_yearPeriod.png")) )
}

message(". \n .. \n .. \n visualisation complete.")
