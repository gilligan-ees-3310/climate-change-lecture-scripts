library(zoo)

read.hadley.strat <- function(download.data = FALSE) {

  url <- 'http://hadobs.metoffice.com/hadat/hadat2/hadat2_monthly_global_mean.txt'
  file <- 'data/hadat2_monthly_global_mean.txt'

  if (download.data)
    download.file(url, file)

  df <- read.table(file,header=T,skip=16,stringsAsFactors=F,na.strings=c('-999','-999.0'))
  df$Year.Mon <- as.yearmon(paste(df$Yr,df$Mon),'%Y %b')
  df$Year.frac <- as.numeric(format(df$Yr)) + as.numeric(format(df$Year.Mon,'%m'))/12
  df <- df[,c('Year.frac',names(df)[3:11])]
}
