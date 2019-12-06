if (! exists('tbr_earth')) source('lecture_utils.R')
if (! exists('planck')) source("planck.R")

library(ggplot2)
library(scales)

read_spectrum <- function(fname) {
  return(read.csv(fname))
}

if (FALSE) {
  thermal_lines <- function(ncolors, spec, tmin=220, tmax=300, max_color = 0.6) {
    dt = (tmax - tmin) / (ncolors - 1)
    dh = max_color/(ncolors-1)
    temperatures = tmin + dt * seq(0,ncolors-1)
    hues = hsv(max_color - (dh * seq(0,ncolors-1)),0.9,0.8)
    for (i in 1:ncolors) {
      lines(spec$k,planck(spec$k,temperatures[i]),lwd=2,col=hues[i])
    }
    return(list(t=temperatures,col=hues))
  }
  
  process_spectrum <- function(spectrum, descr, tmin=220, tmax = 300, nc = 5, max_color = 0.6, ymax=130) {
    par(mar=c(5,4,7,2))
    plot(spectrum, type='l',ylim=c(0,ymax), lwd=2,
         ylab="Radiance",xlab=bquote(.("Wavenumber")~(cm^{-1})),
         mar=c(5,4,7,2)+0.1)
    mtext(paste("Infrared radiance observed by satellite",descr,sep='\n'),3,padj=-2,cex=1.3)
    lamlist = seq(7,25)
    invlam = 1E+4/lamlist
    axis(3,at=invlam,lab=lamlist)
    mtext(expression(paste("Wavelength (",mu,"m)")),3,2)
    if (nc > 0) {
      params = thermal_lines(nc,spectrum,tmin,tmax,max_color)
      legend("topright", legend = c("Observed", paste(rev(params$t),"K")),
             col = c("black",rev(params$col)),lwd=2)
    }
    text(400,0,paste("Data source: NASA IRIS","ftp://disc1.gsfc.nasa.gov/data/legacy/codi/iris/"),
         cex=0.7,adj=c(0,0))
  }
} else {
  
  thermal_lines <- function(ncolors, spec, tmin=220, tmax=300, max_color = 0.6, add_fahrenheit = FALSE) {
    dt = (tmax - tmin) / (ncolors - 1)
    dh = max_color/(ncolors-1)
    temperatures = tmin + dt * seq(0,ncolors-1)
    temp_str <- paste(temperatures, "K")
    if (add_fahrenheit) {
      temp_str <- paste0(temp_str, " (", round(ktof(temperatures)), " F)")
    }
    hues = hsv(max_color - (dh * seq(0,ncolors-1)),0.9,0.8)
    names(hues) <- temp_str
    lines <- lapply(1:ncolors, function(i) {
      data <- data.frame(k = spec$k, i = planck(spec$k, temperatures[i]), t = temp_str[i])
      geom_line(data = data, size=1)
    })
    return(list(lines = lines, scale = hues))
  }
  
  
  process_spectrum <- function(spectrum, descr, tmin=220, tmax = 300, nc = 5, 
                               max_color = 0.6, ymax=130, add_fahrenheit = FALSE) {
    lambda <- c(1, 1.5, 2, 2.5, 3, 3.5, 4:10, 12, 14, 17, 20, 25, 30, 50, 100)
    spectrum$t <- "Observed"
    color_scale <- c(Observed = 'black')
    
    p <- ggplot(spectrum, aes(x = k, y = i, color = t)) + geom_line(size = 1) + 
      scale_y_continuous(limits=c(0,ymax), name = "Radiance", expand = c(0.01,0.0)) +
      scale_x_continuous(name = expression(paste("Wavenumber ", (cm^-1))), 
                         breaks = seq(400,1600,200), expand = c(0.01,0.0),
                         sec.axis = sec_axis(~ ., breaks = 1E4 / lambda, 
                                             labels = lambda,
                                             name = expression(paste("Wavelength ", (mu * m))))) +
      annotate('text', x = 400, y = 2, size=4, color = 'darkgray', hjust = 0, vjust = 0,
               label = paste("Data source: NASA IRIS IR spectrometer",
                             "ftp://acdisc.gsfc.nasa.gov/data/s4pa/Nimbus4_IRIS_Level1B/IRISN4RAD.001/",
                             sep="\n")) +
      theme_bw(base_size = 20)
    
    if (nc > 0) {
      thermal <- thermal_lines(nc,spectrum,tmin,tmax,max_color,add_fahrenheit)
      for (t in thermal$lines) p <- p + t
      p <- p + geom_line(size=1)
      breaks <- c(names(color_scale), rev(names(thermal$scale)))
      color_scale <- c(color_scale, rev(thermal$scale))
      p <- p + scale_color_manual(values = color_scale, breaks = names(color_scale), name="") + 
        theme(legend.position=c(1,1), legend.justification=c(1,1), 
              legend.title=element_blank(), 
              legend.background = element_rect(color="black", fill="white"), 
              legend.key = element_rect(fill="white",color="white"),
              panel.spacing=unit(0,"npc"))
    } else {
      p <- p + scale_color_manual(values = color_scale, guide=FALSE)
    }
    p
  }
}
# process_spectrum(Sahara,"(Sahara desert in April)",220,280,7,0.8)
