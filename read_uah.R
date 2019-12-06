
read.uah <- function(download.data = FALSE, data.dir = NULL) {
  url <- 'http://www1.ncdc.noaa.gov/pub/data/cmb/temp-and-precip/upper-air/uahncdc.ls'
  file <- file.path('upper', 'uahncdc.ls')
  if (! is.null(data.dir)) file = file.path(data.dir, file)
  if (download.data)
    download.file(url,file)

  lines <- readLines(file)
  rows <- length(lines) - 11
  ## Read file as  char vector, one line per row, Exclude first 8 rows
  lines <- lines[2:rows]

  df <- read.fwf(textConnection(lines),widths=c(5,3,rep(6,27)),header=F,sep='$',
                 stringsAsFactors=F,na.strings=c('-999','-999.0','NA'))
  df <- df[,1:3]
  names(df) <- c('year','month','anomaly')
  df$year <- df$year + (df$month - 0.5) / 12
  df <- df[,c('year','anomaly')]
  return(df)
}
