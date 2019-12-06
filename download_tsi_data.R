#
#
#

tsi.url <- 'http://lasp.colorado.edu/lisird/latis/historical_tsi.csv'

tsi.file <- 'data/tsi/historical_tsi.csv'

download.tsi.data <- function(data.dir = NULL) {
  filename = tsi.file
  if (! is.null(data.dir)) filename <- file.path(data.dir, tsi.file)
  download.file(tsi.url, tsi.file)
}

