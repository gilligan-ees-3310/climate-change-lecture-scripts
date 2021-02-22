#
#
#

tsi_url <- str_c('http://lasp.colorado.edu/lisird/latis/dap/historical_tsi.csv',
                 '?&time>=1610-01-01T00:00:00.000Z')

tsi_file <- 'historical_tsi.csv'

download_tsi_data <- function(data_dir = NULL) {
  filename = tsi_file
  if (! is.null(data_dir)) filename <- file.path(data_dir, "solar", filename)
  download.file(tsi_url, filename)
}

#
# Historical reconstruction, including the Maunder minimum
#
tsi_reconstruction_url <- "https://spot.colorado.edu/~koppg/TSI/Historical_TSI_Reconstruction.txt"
tsi_reconstruction_file <- "historical_reconstruction.txt"

download_tsi_reconstruction <- function(data_dir = NULL) {
  filename <- tsi_reconstruction_file
  if (! is.null(data_dir)) filename <- file.path(data_dir, "solar", filename)
  download.file(tsi_reconstruction_url, filename)
}

sunspot_url <- "https://www.ngdc.noaa.gov/stp/space-weather/solar-data/solar-indices/sunspot-numbers/group/daily-values-and-means/group-sunspot-numbers_yearly-means(yearrg).txt"
sunspot_file <- "sunspots.txt"

download_sunspots <- function(data_dir = NULL) {
  filename <- sunspot_file
  if (! is.null(data_dir)) filename <- file.path(data_dir, "solar", filename)
  download.file(sunspot_url, filename)
}

alt_sunspot_url <- "https://www.ngdc.noaa.gov/stp/space-weather/solar-data/solar-indices/sunspot-numbers/depricated/international/listings/listing_international-sunspot-numbers_yearly.txt"
alt_sunspot_file <- "alt_sunspots.txt"

download_alt_sunspots <- function(data_dir = NULL) {
  filename <- alt_sunspot_file
  if (! is.null(data_dir)) filename <- file.path(data_dir, "solar", filename)
  download.file(alt_sunspot_url, filename)
}
