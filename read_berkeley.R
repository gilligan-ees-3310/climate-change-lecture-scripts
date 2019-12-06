library(zoo)
library(ggplot2)

extract_year <- function(data) {
  invisible(data[data$Year >= 1850 & data$Month == 6,])
}

land_sea <- read.fwf('berkeleyearth/land_and_ocean_complete.txt',
                     widths=c(6,6,rep.int(c(10,7),5)),
                     comment.char='%', strip.white=T,
                     col.names=c('Year','Month','anom.mon', 'unc.mon',
                                 'anom.ann', 'unc.ann', 'anom.5', 'unc.5',
                                 'anom.10', 'unc.10', 'anom.20', 'unc.20'),
                     header=F, stringsAsFactors=F, na.strings=c('NA','NaN'),
                     blank.lines.skip=TRUE, nrow=12 * (2014 - 1850) + 2)

land_sea <- land_sea[! is.na(land_sea$Year),]
ls <- extract_year(land_sea)


us.48 <- read.fwf('berkeleyearth/contiguous-united-states-TAVG-Trend.txt',
                     widths=c(6,6,rep.int(c(10,7),5)),
                     comment.char='%', strip.white=T,
                     col.names=c('Year','Month','anom.mon', 'unc.mon',
                                 'anom.ann', 'unc.ann', 'anom.5', 'unc.5',
                                 'anom.10', 'unc.10', 'anom.20', 'unc.20'),
                     header=F, stringsAsFactors=F, na.strings=c('NA','NaN'),
                     blank.lines.skip=TRUE)

us.48 <- us.48[! is.na(us.48$Year),]
us.48s <- extract_year(us.48)

tennessee <- read.fwf('berkeleyearth/tennessee-TAVG-Trend.txt',
                  widths=c(6,6,rep.int(c(10,7),5)),
                  comment.char='%', strip.white=T,
                  col.names=c('Year','Month','anom.mon', 'unc.mon',
                              'anom.ann', 'unc.ann', 'anom.5', 'unc.5',
                              'anom.10', 'unc.10', 'anom.20', 'unc.20'),
                  header=F, stringsAsFactors=F, na.strings=c('NA','NaN'),
                  blank.lines.skip=TRUE)

tennessee <- tennessee[! is.na(tennessee$Year),]
tns <- extract_year(tennessee)


nashville <- read.fwf('berkeleyearth/Nashville-TAVG-Trend.txt',
                      widths=c(6,6,rep.int(c(10,7),5)),
                      comment.char='%', strip.white=T,
                      col.names=c('Year','Month','anom.mon', 'unc.mon',
                                  'anom.ann', 'unc.ann', 'anom.5', 'unc.5',
                                  'anom.10', 'unc.10', 'anom.20', 'unc.20'),
                      header=F, stringsAsFactors=F, na.strings=c('NA','NaN'),
                      blank.lines.skip=TRUE)

nashville <- nashville[! is.na(nashville$Year),]

nashs <- extract_year(nashville)

nash.stn <- read.fwf('berkeleyearth/161711-TAVG-Data.txt',
                     widths=c(6,6, 12,10,8,10,14,10,12,10),
                     comment.char='%', strip.white=T,
                     col.names = c('Year', 'Month', 'T.raw', 'anom.raw', 
                                   'qc.failed', 'cont.breaks', 
                                   'T.adj', 'anom.adj', 'T.reg', 'anom.reg'),
                     header=F, stringsAsFactors=F, na.strings=c('NA','NaN'),
                     blank.lines.skip=TRUE)

nash.stn <- nash.stn[!is.na(nash.stn$Year),]

nash.stns <- extract_year(nash.stn)
nz <- rollapply(zoo(nash.stns$anom.adj, nash.stns$Year), 10, 
                function(x) mean(x, na.rm=T), fill=NA)

nash.stns2 <- data.frame(Year = nash.stns$Year, anom.ann = nash.stns$anom.adj,
                         unc.ann = NA, 
                         anom.10 = nz)

plot_berkeley <- function(data, title) {  
  p <- ggplot(data=data, aes(x=Year)) + 
    geom_ribbon(aes(ymax=anom.ann + unc.ann, ymin = anom.ann - unc.ann), fill='gray') + 
    geom_line(aes(y=anom.ann, color='yearly')) + 
    geom_line(aes(y=anom.10, color="decadal"), size=I(2)) +
    labs(x='Year', 
         y=expression(paste("Temperature Anomaly ",~(degree*C))),
         title=title)
  
  p <- p + 
    scale_color_manual("Temperatures",
                       values=c("yearly"="black","decadal"="red"),
                       breaks=c("yearly","decadal"),
                       labels=c("Annual temp","10-year average"))
  
  p <- p + guides(color=guide_legend(override.aes=list(lwd=c(1,2)),
                                     keywidth=3))
  
  p <- p + theme_classic(base_size=20) +
    theme( legend.position = c(1.0, 0.0), legend.justification=c(1,0),
           legend.text = element_text(size=20),
           legend.title=element_text(face="bold"))
  
  print(p)
  invisible(p)
}


p <- plot_berkeley(ls, "Global Land-Sea Temperature (Berkeley Earth)")
png('Global.png', width=1200, height=800)
print(p)
dev.off()

p <- plot_berkeley(us.48s, "Contiguous US (Berkeley Earth)")
png('US48.png', width=1200, height=800)
print(p)
dev.off()

p <- plot_berkeley(tns, "Tennessee (Berkeley Earth)")
png('Tennessee.png', width=1200, height=800)
print(p)
dev.off()

p <- plot_berkeley(nashs, "Nashville Area (Berkeley Earth)")
png('Nashville.png', width=1200, height=800)
print(p)
dev.off()

p <- plot_berkeley(nash.stns2, "Nashville Metro Station (Berkeley Earth)")
png('Nash_Stn.png', width=1200, height=800)
print(p)
dev.off()

