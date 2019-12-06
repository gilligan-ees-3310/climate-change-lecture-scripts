library(tidyverse)
library(reshape)

## File Download and File
## GISS monthly data import script develodped by http://LearnR.wordpress.com
read.giss.monthly <- function(download.data = FALSE) {
  url <- c("http://data.giss.nasa.gov/gistemp/tabledata/GLB.Ts+dSST.txt")
  file <- c("data/GLB.Ts+dSST.txt")
  if (download.data)
    download.file(url, file)

  ## 1st 8 rows and the last 12 rows contain instructions
  ## Find out the number of rows in a file, and exclude the last 12
  lines <- readLines(file)
  rows <- length(lines) - 12
  ## Read file as  char vector, one line per row, Exclude first 8 rows
  lines <- lines[8:rows]
  ## Data Manipulation, R vector
  ## Use regexp to replace all the occurences of **** with NA
  lines2 <- gsub("\\*{3,5}", " NA", lines, perl=TRUE)
  ## Convert the character vector to a dataframe
  df <- read.table(
    textConnection(lines2), header=TRUE, colClasses = "character")
#  closeAllConnections()
  ## Select monthly data in first 13 columns
  df.annual <- df[,c(1,14)] # annual mean J-D
  df <- df[,1:13] # monthly values
  ## Convert all variables (columns) to numeric format
  df <- colwise(as.numeric) (df)
  ## Remove rows where Year=NA from the dataframe
  df <- df [!is.na(df$Year),]
  ## Convert from wide format to long format
  df.monthly <- melt(df, id.var="Year", variable_name="Month")
  GISS_mo_num <- unclass(df.monthly$Month)
  GISS_mo_frac <- as.numeric((  unclass(df.monthly$Month)-0.5)/12)
  GISS_yr_frac <- df.monthly$Year + GISS_mo_frac
  GISS_anom <- df.monthly$value/100
  df.monthly <- data.frame(df.monthly, GISS_mo_num, GISS_yr_frac, GISS_anom)
  df.monthly <- df.monthly[order(df.monthly$GISS_yr_frac), ]
  df.monthly <- df.monthly[!is.na(df.monthly$GISS_anom),]
  ## Find last report month and last value
  GISS_last <- nrow(df.monthly)
  GISS_last_yr <- df.monthly$Year[GISS_last]
  GISS_last_mo <- df.monthly$Month[GISS_last]
  GISS_last_mo_num <- df.monthly$GISS_mo_num[GISS_last]
  GISS_last_yr_frac <- GISS_last_yr + (GISS_last_mo_num-0.5)/12
  GISS_last_temp <- df.monthly$GISS_anom[GISS_last]

  ## Calc decade averages
  dec_mean<- as.numeric(14)
  dec_st <- as.numeric(14)
  dec_end <- as.numeric(14)
  yr_n <- as.integer(df.monthly$GISS_yr_frac)
  base_yr <- 1870
  dec_n <-  (as.numeric((yr_n - base_yr) %/% 10) * 10) + base_yr
  df.monthly <- data.frame(df.monthly, dec_n)
  for (i in 1:13) {dec_st[i] = base_yr+ i*10
                   dec_sub <- subset(df.monthly, dec_n == dec_st[i], na.rm=T)
                   dec_mean[i] <- mean(dec_sub$GISS_anom)
  }
  dec_st[14] <- 2010              # Need to have for last step line across decade
  dec_mean[14] <- dec_mean[13]
  dec<- data.frame(dec_st, dec_mean)

  return(list(monthly = df.monthly, annual = df.annual, decadal = dec))
}
