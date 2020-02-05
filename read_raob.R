library(zoo)

read.raob <- function(download.data = FALSE, data_dir = NULL) {
  
  url <- 'http://hadobs.metoffice.com/hadat/hadat2/hadat2_monthly_global_mean.txt'
  file <- file.path('upper', 'hadat2_monthly_global_mean.txt')
  if (! is.null(data_dir)) file <- file.path(data_dir, file)
  
  if (download.data)
    download.file(url, file)
  
  df <- read.table(file,header=T,skip=16,stringsAsFactors=F,na.strings=c('-999','-999.0'))
  names(df)[1:2] <- c('month', 'year')
  df$yearmon <- as.yearmon(paste(df$year,df$month),'%Y %b')
  df$year <- as.numeric(format(df$year)) + (as.numeric(format(df$yearmon,'%m')) - 0.5)/12
  df <- df[,c('year',names(df)[3:11])]
  return(df)
}
