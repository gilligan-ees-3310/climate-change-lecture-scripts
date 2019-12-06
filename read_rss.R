
read.rss <- function(download.data = FALSE, data.dir = NULL) {
  url <- 'http://www1.ncdc.noaa.gov/pub/data/cmb/temp-and-precip/upper-air/rss_monthly_msu_amsu_channel_tls_anomalies_land_and_ocean.txt'
  file <- file.path('upper', 'rss_monthly_msu_amsu_channel_tls_anomalies_land_and_ocean.txt')
  if (! is.null(data.dir)) file = file.path(data.dir, file)
  if (download.data)
    download.file(url,file)
  
  df <- read.table(file,header=F,skip=3,stringsAsFactors=F,na.strings=c('-999','-999.0','NA'))
  df <- df[,c(1,2,10,11)]
  names(df) <- c('year','month','NH','SH')
  df$anomaly <- rowMeans(df[,c('NH','SH')])
  df$year <- df$year + (df$month - 0.5) / 12
  df <- df[,c('year','anomaly')]
  return(df)
}
