############## RClimate Script: GISS Monthly Temperature Anomaly ###########################
## Script stored on http://chasrtsgraphs.woedpress.com account for Users to source()      ##                                                                                       ##
## Download and process GISS Monthly Anomaly Data File                                    ##
## Developed by D Kelly O'Day to demonstrate use of source() function for climate data    ##
##                   http:chartsgraphs.wordpress.com    2/5/10                            ##
############################################################################################
library(tidyverse)
require(grid)

source('load_giss.R')
source('load_tsi.R')

knit_tsi <- function(tsi_as_anomaly = TRUE) {
  runmean <- function(x, window) {
    h = ceiling((window  - 1)/2)
    l = floor((window  - 1)/2)
    offset = seq(-l, h)
    indices = seq(l + 1, length(x) - h)
    y <- unlist(lapply(indices,
                       function(i) mean(x[i + offset])))
    z = rep_len(NA, length(x))
    z[indices] <- y
    invisible(z)
  }

  giss_url <- c(land.sea = "https://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts+dSST.txt",
                land = "https://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts.txt")

  giss_file <- c(land.sea = file.path(data_dir, 'global_temp', 'giss', "GLB.Ts+dSST.txt"),
                 land = file.path(data_dir, 'global_temp', 'giss', 'GLB.Ts.txt'))

  giss_csv_url <- c(land.sea = "https://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts+dSST.csv",
                    land = "https://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts.csv")

  giss_csv_file <- c(land.sea = file.path(data_dir, 'global_temp', 'giss', "GLB.Ts+dSST.csv"),
                     land = file.path(data_dir, 'global_temp', 'giss', 'GLB.Ts.csv'))

  tsi_url <- 'http://lasp.colorado.edu/lisird/latis/dap/historical_tsi'
  alt_tsi_url <- 'https://spot.colorado.edu/~koppg/TSI/Historical_TSI_Reconstruction.txt'

  gt <- load_giss_data()
  giss_temp <- gt$data
  giss_url <- giss_csv_url[gt$label]

  giss_temp <- giss_temp %>% select(year, t.anom) %>% group_by(year) %>%
    summarize(t.anom.annual = mean(t.anom)) %>% ungroup()
  giss_temp$t.anom.decadal <- runmean(giss_temp$t.anom.annual,10)
  giss_temp$var <- "Global Temperature Anomaly"

  tsi <- load_tsi(tsi_as_anomaly)

  gisstem_source_text <- paste0("Temperature data: NASA GISS, ", giss_url['land.sea'])
  tsi.source.text <- paste0("TSI  data: LASP, ", tsi_url)

  data <- rbind(giss_temp, tsi)
  data <- data %>% gather(key = avg, value = value, -year, -var)
  data$avg <- ordered(data$avg, levels = c('t.anom.annual', 't.anom.decadal'),
                      labels = c('Annual', '10-year average'))

  ann.text <- data.frame(label.a = c(gisstem_source_text, tsi.source.text),
                         var = c(giss_temp$var[1], tsi$var[1]),
                         year = c(max(giss_temp$year, na.rm=T), max(tsi$year, na.rm=T)),
                         value = c(min(giss_temp$t.anom.annual, na.rm=T) * 1.5, min(tsi$t.anom.annual, na.rm=T)))

  p_xmin <- 1880
  GISS_last_yr <- max(data$year)

  p <- ggplot(data, aes(x=year, y=value, color=avg, size=avg)) +
    geom_point(aes(shape=avg), size=I(4)) +
    geom_line() +
    scale_shape_manual(name="", values=c("Annual" = 16, "10-year average" = NA)) +
    scale_size_manual(name = "", values=c("Annual" = 1, "10-year average" = 1.5)) +
    scale_color_manual(name = "", values=c("Annual"="slategray3","10-year average"="blue")) +
    labs(title = paste("Temperature Anomalies and Solar Irradiance (", p_xmin, " to ",
                       GISS_last_yr,")",sep=""),
         x  = "Year") +
    geom_text(aes(label=label.a), hjust=1, vjust=0, color="#A0A0A0", size = 5, data = ann.text) +
    xlim(p_xmin, GISS_last_yr + 1) +
    facet_wrap( ~var, scales="free_y",ncol=1) +
    theme_classic(base_size=30) +
    theme(legend.position=c(.2,.9), panel.grid.major=element_line(color="gray90"),
                   plot.title = element_text(size=30,hjust=0.5),
          #        axis.text = element_text(size=16),
          #        axis.title = element_text(size=18),
          #        axis.title.y = element_blank(),
          #        legend.text = element_text(size=16),
          #        legend.title=element_text(size=16,face="bold"),
          legend.key.width=unit(2,"lines"),
          legend.spacing=unit(0,'lines'),
          legend.background=element_blank(),
          strip.text=element_text(size=25),
          axis.title.y = element_blank())

  print(p)

}


