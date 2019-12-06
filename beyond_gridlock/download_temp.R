#
# Surface temperature records
#
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(RcppRoll)

data_dir <- 'data'

berkeley_file <- c(land.sea = file.path('global_temp', 'berkeley', 'Land_and_Ocean_complete.txt'),
                   land = file.path('global_temp', 'berkeley', 'Complete_TAVG_complete.txt'))

giss_file <- c(land.sea = file.path('global_temp', 'giss', "GLB.Ts+dSST.txt"),
               land = file.path('global_temp', 'giss', 'GLB.Ts.txt'))

noaa_file <- c(land.sea = file.path('global_temp', 'noaa', "land_sea_1880-2015.csv"),
               land = file.path('global_temp', 'noaa', 'land_1880-2015.csv'))

hadley_file <- c(land.sea = file.path('global_temp', 'hadley', "CRUTEM.4.4.0.0.global_n+s_monthly.txt"),
                 land = file.path('global_temp', 'hadley', 'HadCRUT.4.4.0.0.monthly_ns_avg.txt'))

setup_dirs <- function() {
  for (dir in c('noaa','giss','berkeley','hadley')) {
    fp <- file.path(data_dir, 'global_temp', dir)
    if (! dir.exists(fp)) dir.create(fp, recursive = TRUE)
  }
}

download_berkeley <- function() {
  berkeley_url <- c(land.sea = 'http://berkeleyearth.lbl.gov/auto/Global/Land_and_Ocean_complete.txt',
                    land = 'http://berkeleyearth.lbl.gov/auto/Global/Complete_TAVG_complete.txt')

  download.file(berkeley_url, file.path(data_dir, berkeley_file))
}

download_giss <- function() {
  giss_url <- c(land.sea = "http://data.giss.nasa.gov/gistemp/tabledata/GLB.Ts+dSST.txt",
                land = "http://data.giss.nasa.gov/gistemp/tabledata/GLB.Ts.txt")

  for (set in names(giss_url)) {
    download.file(giss_url[set], file.path(data_dir, giss_file[set]))
  }
}

download_noaa <- function() {
  noaa_url <- c( land.sea = 'https://www.ncdc.noaa.gov/cag/time-series/global/globe/land_ocean/p12/12/1880-2015.csv',
                 land = 'https://www.ncdc.noaa.gov/cag/time-series/global/globe/land/p12/12/1880-2015.csv')

  for (set in names(noaa_url)) {
    download.file(noaa_url[set], file.path(data_dir, noaa_file[set]))
  }
}

download_hadley <- function() {
  hadley_url <- c(land = 'http://www.metoffice.gov.uk/hadobs/crutem4/data/diagnostics/global/nh+sh/CRUTEM.4.4.0.0.global_n+s_monthly',
                  land.sea = 'http://www.metoffice.gov.uk/hadobs/hadcrut4/data/current/time_series/HadCRUT.4.4.0.0.monthly_ns_avg.txt')
  for (set in names(hadley_url)) {
    download.file(hadley_url[set], file.path(data_dir, hadley_file[set]))
  }
}

rebaseline <- function(monthly_data, new_baseline, old_baseline = NULL) {
  if (is.null(old_baseline)) {
    ob <- monthly_data %>% group_by(month) %>% summarize(old.baseline = 0) %>% ungroup()
  } else {
    ob <- monthly_data %>% filter(between(year, old_baseline[1], old_baseline[2])) %>%
      group_by(month) %>% summarize(old.baseline = mean(t.anom)) %>% ungroup()
  }
  nb <- monthly_data %>% filter(between(year, new_baseline[1], new_baseline[2])) %>%
    group_by(month) %>% summarize(new.baseline = mean(t.anom)) %>% ungroup()
  monthly_data <- monthly_data %>%
    left_join(ob, by = 'month') %>% left_join(nb, by = 'month') %>%
    mutate(t.anom = t.anom + old.baseline - new.baseline ) %>%
    select(-old.baseline, -new.baseline)
}

add_averages <- function(monthly_data) {
  monthly_data <- monthly_data %>% mutate(
    t.anom.annual = roll_mean(t.anom, 12, fill = NA, align="center",
                              na.rm=T),
    t.anom.decadal = roll_mean(t.anom, 120, fill = NA, align = "center",
                               na.rm=T)
  )
  invisible(monthly_data)
}


load_noaa <- function(which.set = 'land.sea', baseline_years = NULL) {
  file <- file.path(data_dir, noaa_file[which.set])
  t_data <- read_csv(file, col_names = c('year', 't.anom'), skip = 3) %>%
    mutate(month = year %% 100, year = year %/% 100, time = year + (month - 0.5) / 12)
  if (! is.null(baseline_years))
    t_data <- t_data %>% rebaseline(baseline_years)
  t_data <- t_data %>% arrange(year, month) %>% add_averages()
  invisible(t_data)
}


load_giss <- function(which.set = 'land.sea',
                           baseline_years = NULL) {
  file <- file.path(data_dir, giss_file[which.set])
  giss_file_skip <- 7
  giss_names <- c('year',
                  'jan', 'feb', 'mar', 'apr', 'may', 'jun',
                  'jul', 'aug', 'sep', 'oct', 'nov', 'dec',
                  'jan.dec', 'dec.nov',
                  'djf','mam','jja','son',
                  'year.2')
  col_widths <- fwf_widths(c(rep_len(5, 13), 7, 6, rep_len(5, 4), 6),
                           col_names = giss_names)

  text <- read_lines(file, skip = giss_file_skip) %>% str_subset('^ *[0-9]') %>%
    str_c(collapse = '\n')
  giss_data <- read_fwf(text, col_positions = col_widths,
                        col_types = cols(.default=col_character()))

  t_data <- giss_data %>% select(year, jan:dec)
  t_data <- t_data %>% gather(month, t.anom, -year) %>%
    mutate(year = as.numeric(year),
           t.anom = ifelse(str_detect(t.anom,fixed('***')), NA,
                           suppressWarnings(as.numeric(t.anom)/100)),
           month = ordered(month, levels = giss_names[2:13]) %>% as.numeric(),
           time = year + (month - 0.5) / 12) %>%
    arrange(time) %>%
    filter(! is.na(t.anom))
  if (! is.null(baseline_years)) {
    t_data <- t_data %>% rebaseline(baseline_years)
  }
  t_data <- t_data %>% arrange(year, month) %>% add_averages()
  invisible(t_data)
}

load_berkeley <- function(which.set = 'land.sea',
                          baseline_years = NULL,
                          sea.ice = 'air') {
  text <- read_lines(file.path(data_dir, berkeley_file[which.set]))
  air_line <- which(str_detect(text,
                               fixed("Global Average Temperature Anomaly with Sea Ice Temperature Inferred from Air Temperatures")))
  water_line <- which(str_detect(text,
                                 fixed("Global Average Temperature Anomaly with Sea Ice Temperature Inferred from Water Temperatures")))
  berkeley_names <- c('year','month','t.anom','t.unc','t.anom.annual','unc.annual',
                      't.anom.5', 'unc.5','t.anom.decadal','unc.decadal',
                      't.anom.20', 'unc.20')
  widths <- c(7,6, rep(c(10,7),4),10,6)
  col_widths = fwf_widths(setNames(widths, berkeley_names))

  if (sea.ice == 'air') {
    text <- text[air_line:water_line]
  } else {
    text <- text[water_line:length(text)]
  }
  text <- text %>% str_subset('^ *[0-9]') %>% str_c(collapse = '\n')
  t_data <- read_fwf(text, col_positions = col_widths, na = 'NaN') %>%
    setNames(berkeley_names) %>%
    select(year, month, t.anom) %>% mutate(time = year + (month - 0.5) / 12) %>%
    filter(! is.na(t.anom))
  if (! is.null(baseline_years)) {
    t_data <- t_data %>% rebaseline(baseline_years)
  }
  t_data <- t_data %>% arrange(year, month) %>% add_averages()
  invisible(t_data)
}
