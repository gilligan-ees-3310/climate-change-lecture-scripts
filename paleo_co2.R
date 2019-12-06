library(tidyverse)
library(ggthemes)
library(RColorBrewer)

epica_co2_url <- 'ftp://ftp.ncdc.noaa.gov/pub/data/paleo/icecore/antarctica/epica_domec/edc-co2-2008.txt'
epica_temp_url <- 'ftp://ftp.ncdc.noaa.gov/pub/data/paleo/icecore/antarctica/epica_domec/edc3deuttemp2007.txt'
law_co2_url <- 'ftp://ftp.ncdc.noaa.gov/pub/data/paleo/icecore/antarctica/law/law2006.txt'
grim_co2_url <- 'http://www.csiro.au/greenhouse-gases/GreenhouseGas/data/CapeGrim_CO2_data_download.csv'


epica_co2_file <- basename(epica_co2_url)
epica_temp_file <- basename(epica_temp_url)
law_co2_file <- basename(law_co2_url)
grim_co2_file <- basename(grim_co2_url)

get_epica_co2_data <- function(data.dir = NULL, data.file = epica_co2_file, 
                           epica_url = epica_co2_url, quiet = TRUE) {
  if (is.null(data.dir)) {
    if (exists('data.dir', envir = parent.frame())) {
      data.dir <- get('data.dir', envir = parent.frame())
    }
  }
  if (! is.null(data.dir)) {
    data.file <- file.path(data.dir, 'paleo', data.file)
  }
  download.file(epica_url, destfile = data.file, quiet = quiet, mode = 'wb')
}

get_epica_temp_data <- function(data.dir = NULL, data.file = epica_temp_file, 
                               epica_url = epica_temp_url, quiet = TRUE) {
  if (is.null(data.dir)) {
    if (exists('data.dir', envir = parent.frame())) {
      data.dir <- get('data.dir', envir = parent.frame())
    }
  }
  if (! is.null(data.dir)) {
    data.file <- file.path(data.dir, 'paleo', data.file)
  }
  download.file(epica_url, destfile = data.file, quiet = quiet, mode = 'wb')
}

get_epica_data <- function(data.dir = NULL, quiet = TRUE) {
  get_epica_co2_data(data.dir, quiet = quiet)
  get_epica_temp_data(data.dir, quiet = quiet)
}

get_law_dome_data <- function(data.dir = NULL, data.file = law_co2_file, 
                              law_url = law_co2_url, quiet = TRUE) {
  if (is.null(data.dir)) {
    if (exists('data.dir', envir = parent.frame())) {
      data.dir <- get('data.dir', envir = parent.frame())
    }
  }
  if (! is.null(data.dir)) {
    data.file <- file.path(data.dir, 'paleo', data.file)
  }
  download.file(law_url, destfile = data.file, quiet = quiet, mode = 'wb')
}

get_grim_data <- function(data.file = grim_co2_file, data.dir = NULL, quiet = TRUE) {
  if (is.null(data.dir)) {
    if (exists('data.dir', envir = parent.frame())) {
      data.dir <- get('data.dir', envir = parent.frame())
    }
  }
  if (! is.null(data.dir)) {
    data.file <- file.path(data.dir, 'paleo', data.file)
  }

  download.file(grim_co2_url, destfile = data.file, 
                quiet = quiet, mode = 'wb')
}


load_epica_co2 <- function(data.file = epica_co2_file, data.dir = NULL) {
  if (is.null(data.dir)) {
    if (exists('data.dir', envir = parent.frame())) {
      data.dir <- get('data.dir', envir = parent.frame())
    }
  }
  if (! is.null(data.dir)) {
    data.file <- file.path(data.dir, 'paleo', data.file)
  }
  #
  # EPICA-C file has three sets of records. 
  # Skip to the composite record (EPICA + Vostok)
  #
  co2 <- read.table(data.file,header=F, skip=774, blank.lines.skip=T, 
                    col.names=c('age','co2'), stringsAsFactors = FALSE)
  co2$source <- 'Ice Core'
  last_epica <- head(co2,1)
  co2 <- rbind(data.frame(source = 'Cape Grim', age=c(last_epica$age, 0),
                          co2=c(last_epica$co2, 401.3)), co2)
  co2$source <- factor(co2$source)
  invisible(co2)
} 

load_epica_temp <- function(data.file = epica_temp_file, data.dir = NULL) {
  if (is.null(data.dir)) {
    if (exists('data.dir', envir = parent.frame())) {
      data.dir <- get('data.dir', envir = parent.frame())
    }
  }
  if (! is.null(data.dir)) {
    data.file <- file.path(data.dir, 'paleo', data.file)
  }
  temp <- read.fwf(data.file, widths=c(5,13,17,13,13),
                   header=F, skip=104, 
                   blank.lines.skip=T, col.names=c('bag','ztop','age','d2','value'))
  temp <- temp[,c('age','value')]
  temp <- rbind(temp,c(age=0,value=0))
  names(temp) <- c('age', 'temp')
  invisible(temp)
}

load_law_dome_co2 <- function(data.file = law_co2_file, data.dir = NULL) {
  if (is.null(data.dir)) {
    if (exists('data.dir', envir = parent.frame())) {
      data.dir <- get('data.dir', envir = parent.frame())
    }
  }
  if (! is.null(data.dir)) {
    data.file <- file.path(data.dir, 'paleo', data.file)
  }
  
  co2 <- read.fwf(data.file,widths = c(13,11,10), 
                  skip=2464, nrow=(2715-2464), 
                  header=F, stringsAsFactors = F, 
                  blank.lines.skip=T)
  colnames(co2) <- c('source', 'year','co2')
  co2$source <- trimws(co2$source, 'both')
  co2 <- rbind(data.frame(source = 'CAPE GRIM', year = 2017, co2 = 401.3), co2)
  co2 <- na.omit(co2)
  co2$source <- ifelse(co2$source == 'CAPE GRIM', 'Cape Grim','Ice Core')
  co2$source <- factor(co2$source)
  invisible(co2)
}

load_grim_co2 <- function(data.file = grim_co2_file, data.dir = NULL) {
  if (is.null(data.dir)) {
    if (exists('data.dir', envir = parent.frame())) {
      data.dir <- get('data.dir', envir = parent.frame())
    }
  }
  if (! is.null(data.dir)) {
    data.file <- file.path(data.dir, 'paleo', data.file)
  }

  grim <- read.csv(data.file, header=T, skip=24, stringsAsFactors = F)
  grim <- na.omit(grim)
  grim <- grim %>% group_by(YYYY) %>% summarize(co2 = mean(CO2.ppm.)) %>% ungroup()
  names(grim)[1] <- 'year'
  grim$year <- as.numeric(grim$year)
  grim$source <- 'Cape Grim'
  grim <- grim %>% select(source, year, co2)
  invisible(grim)
}

paleo_palette <- brewer.pal(3, 'Set1')[1:2]
names(paleo_palette) <- c('Cape Grim', 'Ice Core')

plot_epica <- function(co2, palette = paleo_palette, since = NA, text_size = 25) {
  p <- ggplot(data=co2, aes(x=age/1000, y=co2, color = source)) + 
    geom_line(lwd=1) +
    scale_color_manual(values = palette, name = "Data source")
  if (! is.na(since)) p <- p + xlim(0, since)
  p <- p + scale_x_reverse() + 
    labs(x="Thousands of years before present", y=expression(paste(CO[2]," (ppm)")), 
         title=expression(paste(CO[2]," (Antarctica: Vostok & EPICA Dome C)"))) + 
    theme_minimal(base_size = text_size) +
    theme(legend.position = c(0.1, 0.75), legend.justification = c(0,1),
          legend.background = element_rect(fill = 'white'))
  p
}

plot_law_dome <- function(co2, palette = paleo_palette, since = NA, text_size = 25) {
  annot.pos <- list(x = max(co2$year), y = 250)
  data.source.text <- paste0("Data sources: ", law_co2_url, "\n", grim_co2_url)
  grim_start <- co2 %>% filter(source == 'Cape Grim') %>% summarize(start = min(year)) %>% 
    select(start) %>% unlist()
  p <- ggplot(co2, aes(x=year, y=co2, color = source)) + 
    geom_line(size = 1) + 
    scale_color_manual(values = palette, name = "Data source",
                       labels = c("Cape Grim" = paste0("Cape Grim, Tasmania: Real-time monitoring since ", grim_start),
                                  "Ice Core" = "Law Dome, Antarctica: Ice core"))
  if (! is.na(since)) p <- p + xlim(0, since)
  p <- p + labs(x="Year (AD)", y=expression(paste(CO[2]," (ppm)")), 
                title=expression(paste(CO[2]," (Cape Grim + Law Dome)"))) + 
    annotate("text",x=annot.pos$x,y=annot.pos$y,
             label=data.source.text, hjust=1, vjust = 0, size=4.2,
             color = 'gray45') +
  theme_minimal(base_size=text_size) +
    theme(legend.position = c(0.1, 0.85), legend.justification = c(0,1),
          legend.background = element_rect(fill = 'white')) 
  
  p
}

core_labeller <-  structure(function(labels, multi_line = TRUE){
  # message('labels = ', labels)
  labels <- label_value(labels, multi_line = multi_line)
  # message('labels = ', labels)
  label_mapping = c(CO2 = 'plain(CO)[2] ~~ plain((ppm))',
                       Temperature = 'Delta * T')
  labels <- lapply(labels, function(values) {
    values <- label_mapping[as.character(values)]
  })
  # message('labels = ', paste(as.character(labels), collapse = ', '))
  label_parsed(labels, multi_line = multi_line)
}, class = 'labeller')

plot_epica_co2_temp <- function(co2, temp) {
  co2 <- co2 %>% select(-source)
  names(co2) <- c('age','value')
  names(temp) <- c('age','value')
  co2$measurement <- 'CO2'
  temp$measurement <- 'Temperature'
  
  data <- rbind(co2,temp)
  data <- na.omit(data)
  
  #label_mapping = list(CO2 = 'plain(CO)[2] ~~ plain((ppm))',
  #                     Temperature = 'Delta * T')
  #label_mapping = list(CO2 = 'CO2',
  #                     Temperature = 'Temperature')
  data$measurement <- factor(data$measurement, levels=c('CO2', 'Temperature'), ordered=T)
  
  palette <- paleo_palette
  names(palette) <- c('Cape Grim' = 'Temperature', 'Ice Core' = 'CO2')
  p <- ggplot(data=data, aes(x=age/1000, y=value, color=measurement)) + 
    geom_line(size = 1) +
    geom_point(size=I(4), shape=I(16), data=data[data$age == 0.0,]) +
    scale_x_reverse() + 
    labs(x="Thousands of years before present", 
         y="",
         title=expression(paste(CO[2]," and Temperature (Antarctica: Vostok & EPICA Dome C)"))) + 
    scale_color_manual(values = palette) +
    facet_wrap(~measurement, ncol = 1, scales='free_y', strip.position="left", labeller = core_labeller) +
    theme_minimal(base_size=20) + theme(legend.position="none", strip.placement = "outside")
  p
}


blend_law_grim <- function(law, grim) {
  law <- law %>% filter(source == 'Ice Core')
  data <- rbind(law, grim) %>% arrange(year)
  invisible(data)
}
