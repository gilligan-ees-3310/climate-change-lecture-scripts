#
#
#
library(tidyverse)

download_nino34 <- function(dir = NULL) {
  if (is.null(dir)) dir <- data.dir
  download.file('http://www.esrl.noaa.gov/psd/data/correlation/nina34.data',
                file.path(dir,'nino34.dat'))
  download.file('http://www.cpc.ncep.noaa.gov/data/indices/wksst8110.for',
                file.path(dir,'nino_weekly.dat'))
}

load_nino34 <- function(file = "nino34.dat", dir = NULL) {
  if (is.null(dir)) dir <- data.dir
  lines <- readLines(file.path(dir, file))[-1]
  lines <- lines[grepl('^[[:space:]]*[[:digit:]]{4}[[:space:]]', lines)]
  nino <- read.table(text=lines, header=F, na.strings = c('NA','9999','-99.99'))
  names(nino) <- c('year',month.abb)
  nino <- nino %>% gather(-year, key="month", value="nino_3.4")
  nino$month <- ordered(nino$month, levels = month.abb)
  nino <- nino %>% mutate(time = year + (as.numeric(month) - 0.5)/12)
  nino <- nino %>% group_by(month) %>%
    mutate(anomaly = nino_3.4 - mean(nino_3.4, na.rm=T)) %>% ungroup()
  nino <- nino %>% mutate(warm = pmax(anomaly, 0), cool = pmin(anomaly,0),
                          phase = ifelse(anomaly > 0, "warm", "cool"))
  nino
}

load_weekly <- function(file = "nino_weekly.dat", dir = NULL) {
  if (is.null(dir)) dir <- data.dir
  nino <- read.fwf(file.path(data.dir,file),
                     header=F, skip=4, na.strings = c('NA','9999','-99.99'))
  names(nino) <- c('week','nino_1.2_sst', 'nino_1.2_anomaly',
                   'nino_3_sst', 'nino_3_anomaly',
                   'nino_3.4_sst', 'nino_3.4_anomaly',
                   'nino_4_sst', 'nino_4_anomaly',
                   widths = c(11,9,4,9,4,9,4)  )

  nino
}


plot_nino34 <- function(nino, min_year = NA, max_year = NA) {
  p <- ggplot(nino, aes(x = time, ymax = warm, ymin=0)) +
    geom_ribbon(fill="red") +
    geom_ribbon(aes(ymax=0,ymin=cool), fill="blue") +
    labs(x = "Year", y = "NINO 3.4 anomaly") +
    theme_classic()
  if (any(! is.na(c(min_year,max_year)))) p <- p + xlim(min_year, max_year)
  p
}
