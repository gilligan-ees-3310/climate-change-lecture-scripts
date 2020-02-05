#
#
#

source('semester_config.R')

mlo.url <- 'http://scrippsco2.ucsd.edu/assets/data/atmospheric/stations/in_situ_co2/monthly/monthly_in_situ_co2_mlo.csv'
keeling_filename = 'co2_mm_mlo.txt'

update.co2 <- function(data_dir = 'data') {

  link <-url(mlo.url)
  co2_data <- read.csv(link, header = F, skip = 57,
                       colClasses = rep_len("numeric", 10),
                       na.strings = '-99.99')
  names(co2_data) <- c("year", "month", "date.excel", "frac.year",
                       "co2", "co2.adj", "co2.fit", "co2.fit.adj",
                       "co2.interpolated", "co2.interpolated.adj")
  write.table(co2_data,file=file.path(data_dir, 'keeling', keeling_filename),row.names=F)
}

