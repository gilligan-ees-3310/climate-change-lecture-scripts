library(plyr)
library(reshape)
library(ggplot2)
library(ggthemes)
library(grid)

source('giss_monthly_anomalies.R')
source('read_uah.R')
source('read_rss.R')
source('read_raob.R')

surface <- read.giss.monthly()$monthly[,c('GISS_yr_frac','GISS_anom')]
names(surface) <- c('Year','anomaly')
uah <- read.uah()
rss <- read.rss()
raob <- read.raob()
raob$anomaly <- raob$X100hPa

f <- c(rep(1,12))/12
surface$annual <- as.numeric(filter(surface$anomaly,f))
raob$annual <- as.numeric(filter(raob$anomaly,f))
rss$annual <- as.numeric(filter(rss$anomaly,f))
uah$annual <- as.numeric(filter(uah$anomaly,f))

surface$height <- 'Surface'
uah$height <- 'Lower stratosphere'
raob$height <- 'Lower stratosphere'
rss$height <- 'Lower stratosphere'

surface$src <- 'GISS'
uah$src <- 'UAH satellite'
raob$src <- 'Radiosonde'
rss$src <- 'RSS satellite'

columns <- c('Year','annual','height','src')
data <- rbind(surface[,columns],
              raob[,columns],
              rss[,columns],
              uah[,columns],
              deparse.level=0
              )

data$src <- factor(data$src, levels=c('UAH satellite', 'RSS satellite',
                                      'Radiosonde', 'GISS'), ordered=TRUE)

names(data)[names(data) == 'annual'] <- 'anomaly'

# Trend chart with trend line
# specify plot yr min & max
p_xmin <- floor(min(data$Year[data$height != 'Surface']))
p_xmax <- ceiling(max(data$Year[data$height != 'Surface']))

sst <- c(1957, 1968, 1979, 1989, 2000, 1964, 1974, 1985, 1995, 2005)
send <- c(1960, 1972,1982,1993,2002, 1966, 1976, 1988, 1998, 2010)
if (sst[1] < p_xmin && send[1] > p_xmin) sst[1] <- p_xmin
var <- c(rep_len('max',5), rep_len('min',5))
arect <- data.frame(start=sst, stop=send, Sunspots=var)
brect <- arect
arect$ymin <- min(unlist(data$anomaly[data$height != 'Surface']), na.rm=T)
arect$ymax <- max(unlist(data$anomaly[data$height != 'Surface']), na.rm=T)
brect$ymin <- min(unlist(data$anomaly[data$height == 'Surface']), na.rm=T)
brect$ymax <- max(unlist(data$anomaly[data$height == 'Surface']), na.rm=T)
arect$height='Lower stratosphere'
brect$height = 'Surface'

r <- rbind(arect,brect)

p <- ggplot(data, aes(Year,anomaly)) +
  geom_line(aes(color=src),size=I(1)) +
  scale_x_continuous(limits=c(p_xmin,p_xmax),
                     breaks = 10 * seq(floor(p_xmin/10), ceiling(p_xmax/10))) +
  geom_rect(aes(NULL,NULL,xmin=start,xmax=stop,ymin=ymin,ymax=ymax, fill=Sunspots), 
            data=r, alpha=0.15) + 
  scale_fill_manual(values=c('red','blue')) +
  facet_wrap( ~height, scales="free_y",ncol=1) +
  labs(y='Tempeature anomaly (C)') +
  scale_color_brewer(type='qual',palette='Dark2', name='') +
  
  theme_classic(base_size=30) +
  theme(legend.position=c(1,0.05), legend.justification=c(1,0),
        legend.key.width=unit(0.03,'npc'), legend.key.height=unit(0.03,'npc'))
print(p)
