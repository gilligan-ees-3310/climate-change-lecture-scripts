#
#
#

tsi.url <- 'http://lasp.colorado.edu/lisird/latis/historical_tsi.csv'

tsi.file <- 'data/tsi/historical_tsi.csv'

download.tsi.data <- function(data_dir = NULL) {
  filename = tsi.file
  if (! is.null(data_dir)) filename <- file.path(data_dir, tsi.file)
  download.file(tsi.url, tsi.file)
}

