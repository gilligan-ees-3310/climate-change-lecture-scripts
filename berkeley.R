#
#  Read Berkeley Earth temperature anomaly
#
library(scales)
library(ggplot2)
library(grid)

berkeley_dir <- rprojroot::find_rstudio_root_file("data", "global_temp", "berkeley")

download_berkeley <- function() {
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
    download.file(url, file.path(berkeley_dir, basename(url)))
  }
}

load_station_file <- function(filename, data_dir = berkeley_dir) {
  if (! is.null(data_dir))
    filename <- file.path(data_dir, basename(filename))
  text <- readLines(filename)
  index <- min(grep("% Year, Month, Temperature, Anomaly,", text, fixed=TRUE))
  text <- tail(text, -index)
  df <- read.table(text = text, header=F, stringsAsFactors=F, na.strings='NaN',
                   col.names = c('year','month','r.raw','a.raw',
                                 'qc.fail','cont.break',
                                 't.adj', 'a.adj', 't.r.exp', 'a.r.exp'))
  df <- df %>% select(year, month, m.a = a.adj)
  df <- df %>% mutate(m.u = NA,
                      y.a = NA, y.u = NA,
                      a5.a = NA, a5.u = NA,
                      a10.a = NA, a10.u = NA,
                      a20.a = NA, a20.u = NA)
  runmean <- function(x, window) {
    h = ceiling((window * 12 - 1)/2)
    l = floor((window * 12 - 1)/2)
    offset = seq(-l, h)
    indices = seq(l + 1, length(x) - h)
    y <- unlist(lapply(indices,
                  function(i) mean(x[i + offset])))
    z = rep_len(NA, length(x))
    z[indices] <- y
    invisible(z)
  }
  df$y.a <- runmean(df$m.a, 1)
  df$a5.a <- runmean(df$m.a, 5)
  df$a10.a <- runmean(df$m.a, 10)
  df$a20.a <- runmean(df$m.a, 20)
  invisible(df)
}

load_one_berkeley_file <- function(filename, data_dir = berkeley_dir, text = NULL) {
  if (is.null(text)) {
    if (! is.null(data_dir)) filename <- file.path(data_dir, basename(filename))
    text <- readLines(filename)
    index <- min(grep("% Year, Month,  Anomaly, Unc.,", text, fixed=TRUE))
    text <- tail(text, - min(index))
  }
  df <- read.table(text = text, header=F,
                   stringsAsFactors=F, na.strings='NaN',
                   col.names=c('year','month','m.a','m.u','y.a','y.u',
                               'a5.a','a5.u','a10.a','a10.u','a20.a','a20.u'))
  df$t <- df$year + (df$month - 0.5)/12
  invisible(df)
}

load_global_berkeley_file <- function(filename, data_dir = berkeley_dir,
                                      above.sea.ice = TRUE) {
  if (! is.null(data_dir)) filename <-
      file.path(data_dir, basename(filename))
  lines <- tail(readLines(filename), -77)
  index <- min(grep("% Global Average Temp", lines, fixed=TRUE))
  if (! is.na(index)) {
    if (above.sea.ice)
      text = head(lines,index - 1)
    else
      text = tail(lines, -(index + 3))
  }
  invisible(load_one_berkeley_file(text = text))
}

load_berkeley_temp <- function(data_dir = berkeley_dir) {
  berkeley_files <- c(
    global = 'Land_and_Ocean_complete.txt',
    north_america = "north-america-TAVG-Trend.txt",
    conus = 'contiguous-united-states-TAVG-Trend.txt',
    tn = 'tennessee-TAVG-Trend.txt',
    nashville =  '36.17N-87.51W-TAVG-Trend.txt',
    nashville_stn =  '161711-TAVG-Data.txt'
  )
  file_labels <- c(
    global = 'Global Land-Ocean',
    north_america = "North America",
    conus = 'Contiguous US',
    tn = 'Tennessee',
    nashville =  'City of Nashville',
    nashville_stn =  'Nashville Climate Station'
  )

  data <- data.frame()

  for (i in seq_along(berkeley_files)) {
    id = names(berkeley_files)[i]
    filename <- berkeley_files[id]
    label <- file_labels[id]
    message(id, label, filename)
    if (id == 'global') {
      df <- load_global_berkeley_file(filename, data_dir)
    } else if (id == 'nashville_stn') {
      load_station_file(filename, data_dir)
    } else {
      df <- load_one_berkeley_file(filename, data_dir)
    }
    df$loc <- id
    df$label <- label
    data <- rbind(df, data)
  }
  data$loc <- ordered(data$loc, levels = names(file_labels))
  data$label <- ordered(data$label, levels = file_labels)
  invisible(data)
}

knit_berkeley_temp <- function(berkeley_data, key) {
  color_scale <- c('annual anomaly' = 'black',
                   '10-year avg' = 'red')

  fill_scale <- c('annual anomaly' = alpha('white',0.0),
                  '10-year avg' = alpha('red',0.2))

  data = berkeley_data %>% filter(loc %in% c(key))
  label = paste(unique(data$label), collapse=", ")

  p <- ggplot(data %>% filter(loc == key), aes(x=t,y=y.a)) +
    geom_smooth(aes(y=a10.a,ymin = a10.a - a10.u, ymax = a10.a + a10.u,
                    color='10-year avg', fill='10-year avg'),
                stat="identity", size=I(1.5)) +
    geom_line(aes(y=y.a, color='annual anomaly', fill='annual anomaly'),
              size=I(1)) +
    labs(y=expression(paste("Temp. anomaly ", (degree * C))), x="Year", title = label) +
    scale_color_manual(values = color_scale, name="",
                       breaks=c('annual anomaly', '10-year avg')) +
    scale_fill_manual(values = fill_scale, name="",
                      breaks=c('annual anomaly', '10-year avg')) +
    theme_classic(base_size=30) +
    theme(legend.position=c(1,0.1), legend.justification=c(1,0),
          legend.key.width=unit(0.03,'npc'))

  p
}
