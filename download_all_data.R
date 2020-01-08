library(tidyverse)
library(rprojroot)

data_dir <- rprojroot::find_rstudio_root_file("data")

setup_dirs <- function() {
  for (dir in c('noaa','giss','berkeley','hadley')) {
    fp <- file.path(data_dir, 'global_temp', dir)
    if (! dir.exists(fp)) dir.create(fp, recursive = TRUE)
  }
}



#
# DOWNLOAD GISS DATA
#
giss_url <- c(land.sea = "https://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts+dSST.txt",
              land = "https://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts.txt")

giss_file <- c(land.sea = file.path(data_dir, 'global_temp', 'giss', "GLB.Ts+dSST.txt"),
               land = file.path(data_dir, 'global_temp', 'giss', 'GLB.Ts.txt'))

giss_csv_url <- c(land.sea = "https://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts+dSST.csv",
              land = "https://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts.csv")

giss_csv_file <- c(land.sea = file.path(data_dir, 'global_temp', 'giss', "GLB.Ts+dSST.csv"),
               land = file.path(data_dir, 'global_temp', 'giss', 'GLB.Ts.csv'))

for (loc in c('land.sea','land')) {
  download.file(giss_url[loc], giss_file[loc])
  download.file(giss_csv_url[loc], giss_csv_file[loc])
}

#
# DOWNLOAD KEELING DATA
#
mlo_url <- 'https://scrippsco2.ucsd.edu/assets/data/atmospheric/stations/in_situ_co2/monthly/monthly_in_situ_co2_mlo.csv'
keeling_filename = 'co2_mm_mlo.txt'

noaa_mlo_url <- 'ftp://aftp.cmdl.noaa.gov/products/trends/co2/co2_mm_mlo.txt'
noaa_keeling_filename = 'co2_mm_mlo_noaa.txt'


link <-url(mlo_url)
co2_data <- read.csv(link, header = F, skip = 57,
                     colClasses = rep_len("numeric", 10),
                     na.strings = '-99.99')
names(co2_data) <- c("year", "month", "date.excel", "frac.year",
                     "co2", "co2.adj", "co2.fit", "co2.fit.adj",
                     "co2.interpolated", "co2.interpolated.adj")
write.table(co2_data,file=file.path(data_dir, 'keeling', keeling_filename),row.names=F)

CO2_data <- read.table(noaa_mlo_url,
                       sep = "", row.names = NULL, header = F, colClasses = rep("numeric", 7),
                       comment.char = "#", na.strings = -99.99)
names(CO2_data) <- c("Year", "Month", "Frac.Year", "CO2", "CO2.interpolated", "CO2.trend", "missing.days")
write.table(CO2_data,file=file.path(data_dir, "keeling", noaa_keeling_filename),row.names=F,)

#
# DOWNLOAD TSI DATA
#
tsi.url <- str_c('http://lasp.colorado.edu/lisird/latis/dap/historical_tsi.csv',
                 '?&time>=1610-01-01T00:00:00.000Z&time<=',
                 lubridate::now(tz = "UTC") %>%
                   strftime("%Y-%m-%dT%H:%M:%S%z", tz = "UTC"))

tsi.file <- 'historical_tsi.csv'

download.file(tsi.url, file.path(data_dir, "solar", tsi.file))


#
# DOWNLOAD BERKELEY EARTH DATA
#
global_land_ocean_url = "http://berkeleyearth.lbl.gov/auto/Global/Land_and_Ocean_complete.txt"
global_land_url = "http://berkeleyearth.lbl.gov/auto/Global/Complete_TAVG_complete.txt"
northern_hemisphere_url = "http://berkeleyearth.lbl.gov/auto/Regional/TAVG/Text/northern-hemisphere-TAVG-Trend.txt"
north_america_url = "http://berkeleyearth.lbl.gov/auto/Regional/TAVG/Text/north-america-TAVG-Trend.txt"
conus_url = "http://berkeleyearth.lbl.gov/auto/Regional/TAVG/Text/contiguous-united-states-TAVG-Trend.txt"
tennessee_url = "http://berkeleyearth.lbl.gov/auto/Regional/TAVG/Text/tennessee-TAVG-Trend.txt"
nashville_url = "http://berkeleyearth.lbl.gov/auto/Local/TAVG/Text/36.17N-87.51W-TAVG-Trend.txt"
nashville_stn_url = "http://berkeleyearth.lbl.gov/auto/Stations/TAVG/Text/161711-TAVG-Data.txt"

for (url in c(global_land_url, global_land_ocean_url, northern_hemisphere_url,
              north_america_url, conus_url, tennessee_url, nashville_url,
              nashville_stn_url)) {
  download.file(url, file.path(data_dir, 'global_temp', 'berkeley', basename(url)))
}


#
# DOWNLOAD NOAA GLOBAL TEMPERATURES
#

noaa_url <- c( land.sea = 'https://www.ncdc.noaa.gov/cag/time-series/global/globe/land_ocean/p12/12/1880-2019.csv',
               land = 'https://www.ncdc.noaa.gov/cag/time-series/global/globe/land/p12/12/1880-2019.csv')

noaa_file <- c(land.sea = file.path('global_temp', 'noaa', "land_sea_1880-2018.csv"),
               land = file.path('global_temp', 'noaa', 'land_1880-2018.csv'))

for (set in names(noaa_url)) {
  download.file(noaa_url[set], file.path(data_dir, noaa_file[set]))
}

#
# DOWNLOAD HADLEY GLOBAL TEMPERATURES
#
hadley_url <- c(land = 'http://www.metoffice.gov.uk/hadobs/crutem4/data/diagnostics/global/nh+sh/CRUTEM.4.6.0.0.global_n+s_monthly',
                land.sea = 'http://www.metoffice.gov.uk/hadobs/hadcrut4/data/current/time_series/HadCRUT.4.6.0.0.monthly_ns_avg.txt')

hadley_file <- c(land.sea = file.path('global_temp', 'hadley', "CRUTEM.4.6.0.0.global_n+s_monthly.txt"),
                 land = file.path('global_temp', 'hadley', 'HadCRUT.4.6.0.0.monthly_ns_avg.txt'))

for (set in names(hadley_url)) {
  download.file(hadley_url[set], file.path(data_dir, hadley_file[set]))
}

#
# DOWNLOAD PALEO CO2 DATA FROM LAW DOME AND EPICA AND CONTEMPORARY MEASUREMENTS FROM CAPE GRIM
#

law_co2_url <- 'ftp://ftp.ncdc.noaa.gov/pub/data/paleo/icecore/antarctica/law/law2006.txt'
law_co2_file <- basename(law_co2_url)

download.file(law_co2_url, destfile = file.path(data_dir, "paleo", law_co2_file), mode = 'wb')

grim_url <- 'http://capegrim.csiro.au/GreenhouseGas/data/CapeGrim_CO2_data_download.csv'
grim_file <- basename(grim_url)

download.file(grim_url, destfile = file.path(data_dir, 'keeling', grim_file), mode = 'wb')

epica_co2_url <- 'ftp://ftp.ncdc.noaa.gov/pub/data/paleo/icecore/antarctica/epica_domec/edc-co2-2008.txt'
epica_temp_url <- 'ftp://ftp.ncdc.noaa.gov/pub/data/paleo/icecore/antarctica/epica_domec/edc3deuttemp2007.txt'

epica_co2_file <- basename(epica_co2_url)
epica_temp_file <- basename(epica_temp_url)

download.file(epica_co2_url, file.path(data_dir, "paleo", epica_co2_file))
download.file(epica_temp_url, file.path(data_dir, "paleo", epica_temp_file))

#
# DOWNLOAD ENSO DATA
#
download.file('http://www.esrl.noaa.gov/psd/data/correlation/nina34.data',
              file.path(data_dir, "enso", 'nino34.dat'))
download.file('http://www.cpc.ncep.noaa.gov/data/indices/wksst8110.for',
              file.path(data_dir, 'enso', 'nino_weekly.dat'))

#
# DOWNLOAD HADLEY MONTHLY STRATOSPHERE TEMPERATURES
#
url <- 'http://hadobs.metoffice.co.uk/hadat/hadat2/hadat2_monthly_global_mean.txt'
file <- 'hadat2_monthly_global_mean.txt'

download.file(url, file.path(data_dir, "upper", file))


# rss_url <- 'http://www1.ncdc.noaa.gov/pub/data/cmb/temp-and-precip/upper-air/rss_monthly_msu_amsu_channel_tls_anomalies_land_and_ocean.txt'
rss_url <- 'https://www.ncdc.noaa.gov/monitoring-content/sotc/upper-air/msu/rss_monthly_msu_amsu_channel_tls_anomalies_land_and_ocean.txt'
rss_file <- 'rss_monthly_msu_amsu_channel_tls_anomalies_land_and_ocean.txt'
download.file(rss_url,file.path(data_dir, "upper", rss_file))


# alt_uah_url <- 'https://www.nsstc.uah.edu/data/msu/v6.0/tls/tlsglhmam_6.0.txt'
# uah_url <- 'http://www1.ncdc.noaa.gov/pub/data/cmb/temp-and-precip/upper-air/uahncdc.ls'
uah_url <- 'https://www.ncdc.noaa.gov/monitoring-content/sotc/upper-air/msu/uahncdc.ls'
uah_file <- 'uahncdc.ls'

download.file(uah_url,file.path(data_dir, "upper", uah_file))

download.file("http://www.eia.gov/totalenergy/data/browser/csv.cfm?tbl=T12.01",
              file.path(data_dir, 'ghg_emissions', 'MET_T12_01.csv'))

# nimbus_url <- "ftp://acdisc.gsfc.nasa.gov/data/s4pa/Nimbus4_IRIS_Level1B/IRISN4RAD.001/"
# nimbus_file <- "sahara.csv"
#
# download.file(nimbus_url, file.path(data_dir, "nimbus", nimbus_file))

