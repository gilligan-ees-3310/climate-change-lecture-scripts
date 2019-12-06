############## RClimate Script: Mauna Loa Monthly CO2    ###################################
## Script stored on http://chartsgraphs.wordpress.com account for Users to source()       ##                                                                                       ##
## Download and process Monthly CO2 Data File                                             ##
## Developed by D Kelly O'Day to demonstrate use of source() function for climate data    ##
##                   http:chartsgraphs.wordpress.com    1/16/10                           ##
############################################################################################
library(ggplot2)
library(ggthemes)
library(grid)
library(extrafont)

prepare.fonts <- function() {
  loadfonts()
  loadfonts(device='win')
}


#mlo.url = "ftp://ftp.cmdl.noaa.gov/ccg/co2/trends/co2_mm_mlo.txt"
mlo.url = 'ftp://aftp.cmdl.noaa.gov/products/trends/co2/co2_mm_mlo.txt'
month.names <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')

get.CO2 <- function(live = F) {

  filename = 'co2_mm_mlo.txt'
  if (live || ! file.exists(filename)) {
    link <-url(mlo.url)
    CO2_data <- read.table(link,
                           sep = "", row.names = NULL,header = F,colClasses = rep("numeric", 7),
                           comment.char = "#", na.strings = -99.99)
    names(CO2_data) <- c("Year", "Month", "Frac.Year", "CO2", "CO2.interpolated", "CO2.trend", "missing.days")
    write.table(CO2_data,file=filename,row.names=F,)
  }
  else {
    CO2_data <- read.table(filename,row.names=NULL, header=T)
  }

  df <- data.frame(cbind(CO2_data$Frac.Year, CO2_data$CO2.interpolated, CO2_data$CO2.trend))
  names(df) <- c("Year","monthly","annual")

  seasonal.anomalies <- CO2_data$CO2.interpolated - CO2_data$CO2.trend
  monthly.cycle <- as.numeric(14)
  for (i in 1:12) {
    monthly.cycle[i] <- mean(seasonal.anomalies[CO2_data$Month == i], rm.na=T)
  }
  monthly.cycle[13] <- monthly.cycle[1]

  monthly <- data.frame(cbind(seq(1:13), monthly.cycle))
  names(monthly) <- c("Month", "CO2")

  invisible(list(keeling = df, seasonal = monthly,
              up.to.date = list('Year'=CO2_data$Year[nrow(CO2_data)], 'Month'=CO2_data$Month[nrow(CO2_data)])))
}

plot.CO2 <- function(co2data, vp.x=0.98,vp.y=0.07,vp.w=0.4,vp.h=0.38,y.adj = NA) {
  trend <- co2data$keeling
  seasonal <- co2data$seasonal
  up.to.date <- co2data$up.to.date

  up.to.date.text <- paste("Data updated through ",
                           month.names[up.to.date$Month],", ",up.to.date$Year,
                           sep="")
  data.source.text <- paste("Data source: NOAA, ", mlo.url,
                            sep="")
  last.measurement <- trend[nrow(trend),]
  last.measurement.text <- paste(month.names[up.to.date$Month]," ", up.to.date$Year, ": ", last.measurement$monthly, " ppm.", sep="")

  up.to.date.center = list(x=1970,y=395)
  up.to.date.halfwidth = 10
  up.to.date.halfheight = 5

  keeling.plot  <- ggplot(data=trend, aes(Year))
  keeling.plot <- keeling.plot + geom_line(aes(y=annual, size="annual", color="annual", shape="annual"))
  keeling.plot <- keeling.plot + geom_line(aes(y=monthly, size="monthly", color="monthly", shape="monthly"))
  keeling.plot <- keeling.plot + geom_point(aes(y=monthly,x=Year, size="last", color="last", pch="last"),
                                            data=last.measurement)
  keeling.plot <- keeling.plot + scale_shape_manual(values=c("annual"=NA, "monthly"=NA, "last"=16),
                                                    name="Averaging", guide=FALSE)
  keeling.plot <- keeling.plot + scale_color_manual(values=c("annual"="blue", "monthly"="red",
                                                             "last"="darkgreen"),
                                                    labels=c("annual","monthly",
                                                             last.measurement.text),
                                                    name="Averaging")
  keeling.plot <- keeling.plot + scale_size_manual(values=c("annual"=1.5, "monthly"=1, "last"=5),
                                                   name="Averaging", guide=FALSE)
  keeling.plot <- keeling.plot + guides(color=guide_legend(override.aes=list(
    shape=c(NA,NA,16),
    linetype=c(1,1,0),
    colour=c('blue','red','darkgreen'),
    size=c(1.5,1,5),
    lwd=c(1.5,1,0)),
    keywidth=2))


  keeling.plot <- keeling.plot + scale_y_continuous(limits=c(310,402))
  keeling.plot <- keeling.plot + labs(y=bquote(CO[2] ~ .("(parts per million)")), x="Year",
                                      title = expression(paste(CO[2]," Trends Since 1958, Mauna Loa, Hawaii.")))
  keeling.plot <- keeling.plot + theme_gdocs(base_size=20)
  keeling.plot <- keeling.plot + theme(axis.title.y = element_text(angle=90),
                                       plot.title=element_text(hjust=0.5),
                                       legend.position = c(0.25,0.82), legend.key.size = unit(1,"cm"))

  #       annotate('rect',xmin=up.to.date.center$x - up.to.date.halfwidth,xmax=up.to.date.center$x + up.to.date.halfwidth,
  #                ymin=up.to.date.center$y - up.to.date.halfheight, ymax=up.to.date.center$y + up.to.date.halfheight,
  #                fill="beige")
  #       annotate("text",x=up.to.date.center$x,y=up.to.date.center$y,label=up.to.date.text,size=7)
  keeling.plot <- keeling.plot + annotate("text",x=1990,y=312,label=up.to.date.text,
                                          hjust=1, vjust=0, size=6)
  keeling.plot <- keeling.plot + annotate("text",x=1990,y=310,label=data.source.text,
                                          hjust=1, vjust=1, size=3.5)
  #        annotate("point",x=trend$Year[nrow(trend)],y=trend$monthly[nrow(trend)],colour="darkgreen",size=5)
  #        annotate("point",x=last.measurement$Year,y=last.measurement$monthly,colour="darkgreen",size=5)
  #        annotate("point",x=2000.5,y=last.measurement$monthly,size=5,colour="darkgreen")
  #        annotate("text", x=2001,y=last.measurement$monthly,label=last.measurement.text, vjust=0.5,hjust=0,size=6)

  seasonal.plot <- ggplot(seasonal, aes(Month,CO2)) +
    geom_point(size=I(4), color=I('darkblue')) +
    geom_line(size=I(1), color=I('darkblue')) +
    scale_x_discrete(labels=c("Jan","","","Apr","","","Jul","","","Oct","","","Jan")) +
    scale_y_continuous(breaks=NULL, name=bquote(CO[2])) +
    ggtitle("Annual cycle") +
    theme_bw(base_size=15) +
    theme(panel.grid.major = element_blank(),
          plot.margin = unit(c(0,0,0,0),"npc"))
  if (! is.na(y.adj))
    seasonal.plot <- seasonal.plot + theme(axis.title.y = element_text(vjust=y.adj))
  print(keeling.plot)
  vp.subplot <- viewport(width=vp.w, height=vp.h, x=vp.x, y=vp.y,
                         just=c(1,0))
  print(seasonal.plot, vp=vp.subplot)

  invisible(list(keeling.plot, seasonal.plot))
}

