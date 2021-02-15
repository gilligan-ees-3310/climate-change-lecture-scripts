library(tidyverse)
library(reshape)
library(ggthemes)
library(grid)

source('load_giss.R')
source('read_uah.R')
source('read_rss.R')
source('read_raob.R')

read_strat_data <- function(data_dir = NULL) {
  if (is.null(data_dir)) {
    if (exists('data_dir', envir = parent.frame())) {
      data_dir <- get('data_dir', envir = parent.frame())
    }
  }
  surface <- load_giss_data()$data %>% select(year = time, anomaly = t.anom)

  uah <- read.uah(data_dir = data_dir)
  rss <- read.rss(data_dir = data_dir)
  raob <- read.raob(data_dir = data_dir)
  raob$anomaly <- raob$X100hPa

  f <- c(rep(1,12))/12
  surface$annual <- as.numeric(stats::filter(surface$anomaly,f))
  raob$annual <- as.numeric(stats::filter(raob$anomaly,f))
  rss$annual <- as.numeric(stats::filter(rss$anomaly,f))
  uah$annual <- as.numeric(stats::filter(uah$anomaly,f))

  surface$height <- 'Surface'
  uah$height <- 'Lower stratosphere'
  raob$height <- 'Lower stratosphere'
  rss$height <- 'Lower stratosphere'

  surface$src <- 'GISS'
  uah$src <- 'UAH satellite'
  raob$src <- 'Radiosonde'
  rss$src <- 'RSS satellite'

  columns <- c('year','annual','height','src')
  data <- rbind(surface[,columns],
                raob[,columns],
                rss[,columns],
                uah[,columns],
                deparse.level=0
  )

  data$src <- factor(data$src, levels=c('UAH satellite', 'RSS satellite',
                                        'Radiosonde', 'GISS'), ordered=TRUE)

  names(data)[names(data) == 'annual'] <- 'anomaly'
  invisible(data)
}

plot_strat <- function(data_dir = NULL) {
  data <- read_strat_data(data_dir)

  # Trend chart with trend line
  # specify plot yr min & max
  p_xmin <- floor(min(data$year[data$height != 'Surface']))
  p_xmax <- ceiling(max(data$year[data$height != 'Surface']))

  p <- ggplot(data, aes(year,anomaly)) +
    geom_line(aes(color=src),size=I(1)) +
    scale_x_continuous(limits=c(p_xmin,p_xmax),
                       breaks = 10 * seq(floor(p_xmin/10), ceiling(p_xmax/10)),
                       minor_breaks = 5 * seq(floor(p_xmin/5),
                                                    ceiling(p_xmax / 5))) +
    facet_wrap( ~height, scales="free_y",ncol=1) +
    labs(y='Tempeature anomaly (C)') +
    scale_color_brewer(type='qual',palette='Dark2', name='') +
    theme_classic(base_size=30) +
    theme(legend.position=c(1,0.05), legend.justification=c(1,0),
          legend.key.width=unit(0.03,'npc'), legend.key.height=unit(0.03,'npc'))
  p
}

plot_strat_2 <- function(data_dir = NULL) {
  data <- read_strat_data(data_dir)

    # Trend chart with trend line
  # specify plot yr min & max
  p_xmin <- floor(min(data$year[data$height != 'Surface']))
  p_xmax <- ceiling(max(data$year[data$height != 'Surface']))

  sst <-  c(1957, 1968, 1979, 1989, 2000, 2013, 1964, 1974, 1985, 1995, 2005)
  send <- c(1960, 1972, 1982, 1993, 2002, 2015, 1966, 1976, 1988, 1998, 2010)
  if (sst[1] < p_xmin && send[1] > p_xmin) sst[1] <- p_xmin
  var <- c(rep_len('max',6), rep_len('min',5))
  arect <- data.frame(start=sst, stop=send, Sunspots=var)
  brect <- arect
  arect$ymin <- min(unlist(data$anomaly[data$height != 'Surface']), na.rm=T)
  arect$ymax <- max(unlist(data$anomaly[data$height != 'Surface']), na.rm=T)
  brect$ymin <- min(unlist(data$anomaly[data$height == 'Surface']), na.rm=T)
  brect$ymax <- max(unlist(data$anomaly[data$height == 'Surface']), na.rm=T)
  arect$height='Lower stratosphere'
  brect$height = 'Surface'

  r <- rbind(arect,brect)

  p <- ggplot(data, aes(year,anomaly)) +
    geom_line(aes(color=src),size=I(1)) +
    scale_x_continuous(limits=c(p_xmin,p_xmax),
                       breaks = 10 * seq(floor(p_xmin/10), ceiling(p_xmax/10))) +
    geom_rect(aes(NULL,NULL,xmin=start,xmax=stop,ymin=ymin,ymax=ymax, fill=Sunspots),
              data=r, alpha=0.15) +
    scale_fill_manual(values=c('red','blue')) +
    facet_wrap( ~height, scales="free_y",ncol=1) +
    labs(y=expression(paste('Temperature anomaly ', (degree * C)))) +
    scale_color_brewer(type='qual',palette='Dark2', name="Data source") +

    theme_classic(base_size=30) +
    theme(legend.position=c(1,0.01), legend.justification=c(0.9,0),
          legend.title.align=0.5,
          #legend.background = element_rect(fill = scales::alpha("white", 0), color=NULL),
          legend.background = element_rect(fill = "white", color="black"),
          legend.key.width=unit(0.03,'npc'), legend.key.height=unit(0.015,'npc'),
          legend.text = element_text(size=20))
  p
}
