############## RClimate Script: Mauna Loa Monthly CO2    ###################################
## Script stored on http://chartsgraphs.wordpress.com account for Users to source()       ##                                                                                       ## 
## Download and process Monthly CO2 Data File                                             ##
## Developed by D Kelly O'Day to demonstrate use of source() function for climate data    ##
##                   http:chartsgraphs.wordpress.com    1/16/10                           ##
############################################################################################
library(tidyverse)
library(ggthemes)
library(grid)
library(RColorBrewer)

if (! exists('get.co2')) source('load_keeling.R')

month.names <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')

plot.co2 <- function(co2data, inset = TRUE, base_size = 40) {
  trend <- co2data$keeling
  seasonal <- co2data$seasonal
  up.to.date <- co2data$up.to.date
  
  up.to.date.text <- paste("Data updated through ",
                           month.names[up.to.date$month],", ",up.to.date$year,
                           sep="")
  data.source.text <- paste("Data source: NOAA, ", mlo.url,
                            sep="")
  last.measurement <- tail(na.omit(trend),1)
  last.measurement.text <- paste(month.names[up.to.date$month]," ", up.to.date$year, 
                                 ": ", last.measurement$monthly, " ppm.", sep="")
  
  up.to.date.center = list(x=1970,y=395)
  up.to.date.halfwidth = 10
  up.to.date.halfheight = 5
  
  inset.vp <- list(width=0.4,height=0.38,x=0.13,y=0.86, just = c('left','top'))
  legend.pos <- c(1.0,0.1)
  legend.just = c(1,0)
  annot.pos <- list(x = up.to.date$year + 0.5, y.now = 314, y.src = 311)
  
  palette <- brewer.pal(3, 'Set1')[c(2,1,3)]
  names(palette) <- c('annual', 'monthly', 'last')
  
  kp  <- ggplot(data=trend, aes(x = year, y = monthly))
  kp <- kp + geom_line(aes(y=annual, size="annual", color="annual", shape="annual"))
  kp <- kp + geom_line(aes(size="monthly", color="monthly", shape="monthly"))
  kp <- kp + geom_point(aes(size="last", color="last", pch="last"), 
                        data=last.measurement)
  kp <- kp + scale_shape_manual(values=c("annual"=NA, "monthly"=NA, "last"=16),
                                name="Averaging", guide=FALSE)
  kp <- kp + scale_color_manual(values=palette, 
                                labels=c("annual","monthly",
                                         last.measurement.text),
                                name="Averaging")  
  kp <- kp + scale_size_manual(values=c("annual"=1.5, "monthly"=1, "last"=5), 
                               name="Averaging", guide=FALSE)
  kp <- kp + guides(color=guide_legend(override.aes=list(
    shape=c(NA,NA,16),
    linetype=c(1,1,0),
#    colour=c('blue','red','darkgreen'),
    colour = palette,
    size=c(1.5,1,5),
    lwd=c(1.5,1,0)),
    keywidth=4))
  
  max_co2 <- max(trend$monthly, na.rm = T) + 5
  kp <- kp + scale_x_continuous(limits=c(1957,up.to.date$year + 2), expand=c(0,0.1),
                                breaks = seq(1960,2020,10))
  kp <- kp + scale_y_continuous(limits=c(310,max_co2), expand = c(0,0.1),
                                breaks = seq(310,450,30))
  kp <- kp + labs(y=expression(paste(CO[2], " (parts per million)")), x="Year",
                  title = expression(paste(CO[2]," Trend Since 1958, Mauna Loa, Hawaii.")))
  kp <- kp + theme_gdocs(base_size = base_size)
  kp <- kp + theme(#text = element_text(size=base_size), 
                   axis.title.y = element_text(angle=90, vjust=1.2),
                   plot.title=element_text(hjust=0.5),
                   legend.position = legend.pos, 
                   legend.justification = legend.just,
                   legend.key.width = unit(2,"line"),
                   legend.key.height = unit(2,"line"),
                   legend.text = element_text(size=base_size * 0.75),
                   legend.background = element_rect(color = 'gray95', 
                                                    fill = 'white')
  )
  
  #       annotate('rect',xmin=up.to.date.center$x - up.to.date.halfwidth,xmax=up.to.date.center$x + up.to.date.halfwidth,
  #                ymin=up.to.date.center$y - up.to.date.halfheight, ymax=up.to.date.center$y + up.to.date.halfheight,
  #                fill="beige")
  #       annotate("text",x=up.to.date.center$x,y=up.to.date.center$y,label=up.to.date.text,size=7)
  kp <- kp + annotate("text",x=annot.pos$x,y=annot.pos$y.now,
                      label=up.to.date.text, hjust=1, vjust = 0, size=base_size * 0.15)
  kp <- kp + annotate("text",x=annot.pos$x,y=annot.pos$y.src,
                      label=data.source.text, hjust=1, vjust = 0, size= base_size * 0.105,
                      color = 'gray45')
  #        annotate("point",x=trend$Year[nrow(trend)],y=trend$monthly[nrow(trend)],colour="darkgreen",size=5)
  #        annotate("point",x=last.measurement$Year,y=last.measurement$monthly,colour="darkgreen",size=5)
  #        annotate("point",x=2000.5,y=last.measurement$monthly,size=5,colour="darkgreen")
  #        annotate("text", x=2001,y=last.measurement$monthly,label=last.measurement.text, vjust=0.5,hjust=0,size=6)
  
  seasonal.plot <- ggplot(seasonal, aes(x = month, y = co2)) +
    geom_point(size=4, color = palette['monthly']) +
    geom_line(size=1, color = palette['monthly']) +
    scale_x_discrete(labels=c("Jan","","","Apr","","","Jul","","","Oct","","","Jan")) +
    scale_y_continuous(breaks=NULL, name=bquote(CO[2])) +
    ggtitle("Annual cycle") +
    theme_bw(base_size = base_size * 0.75) + 
    theme(panel.grid.major = element_blank(),
          plot.margin = unit(c(0,2,0,0),"mm"),
          plot.background = element_rect(color="gray95"))
  print(kp)
  if (inset) {
    vp.subplot <- viewport(width = inset.vp$width, height = inset.vp$height,
                           x = inset.vp$x, y = inset.vp$y,
                           just=inset.vp$just)
    print(seasonal.plot, vp=vp.subplot)
  }
  
#  return(kp)
}
