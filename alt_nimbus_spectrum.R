if (! exists('tbr_earth')) source('lecture_utils.R')
if (! exists('planck')) source("planck.R")

library(ggplot2)
library(grid)
library(gridExtra)
library(gtable)
library(scales)

read_spectrum <- function(fname) {
  return(read.csv(fname))
}

thermal_lines <- function(ncolors, spec, tmin=220, tmax=300, max_color = 0.6, add_fahrenheit = FALSE) {
  dt = (tmax - tmin) / (ncolors - 1)
  dh = max_color/(ncolors-1)
  temperatures = tmin + dt * seq(0,ncolors-1)
  temp_str <- paste(temperatures, "K")
  if (add_fahrenheit) {
    temp_str <- paste0(round(ktof(temperatures)), " F")
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
                             max_color = 0.6, ymax=130, add_fahrenheit = FALSE, base_size = 10) {
  spectrum$t <- "Observed"
  color_scale <- c(Observed = 'black')
  p <- ggplot(spectrum, aes(x = k, y = i, color = t)) + geom_line(size = 1) + 
    scale_y_continuous(limits=c(0,ymax), name = "Radiance", expand = c(0.01,0.0)) +
    annotate('text', x = 400, y = 2, size=base_size * 0.25, color = 'dark gray', hjust = 0, vjust = 0,
             label = paste("Data source: NASA IRIS IR spectrometer",
                           # "ftp://acdisc.gsfc.nasa.gov/data/s4pa/Nimbus4_IRIS_Level1B/IRISN4RAD.001/",
                           sep="\n")) +
    theme_bw(base_size = base_size) + 
    ggtitle(paste("Infrared radiance observed by satellite",descr,sep='\n')) +
    theme(panel.grid = element_blank(), plot.margin = unit(c(2,1,0.5,0.5), "lines"))
  
  b <- seq(7,25)
  labels <- b
  labels[labels %in% c(16,18,20,21,23,24)] <- ""
  labels <- as.character(labels)

    p1 <- p + scale_x_continuous(name = expression(paste("Wavelength ", (mu * m))), 
                                 breaks = 1E4/b, labels = labels, expand = c(0.01,0.0))
  
  
  if (nc > 0) {
    thermal <- thermal_lines(nc,spectrum,tmin,tmax,max_color,add_fahrenheit)
    for (t in thermal$lines) p1 <- p1 + t
    p1 <- p1 + geom_line(size=1)
    breaks <- c(names(color_scale), rev(names(thermal$scale)))
    color_scale <- c(color_scale, rev(thermal$scale), name = "Temperature")
    p1 <- p1 + scale_color_manual(values = color_scale, breaks = names(color_scale), name="") + 
      theme(legend.position=c(1,1), legend.justification=c(1,1), 
            legend.title=element_blank(), 
            legend.background = element_rect(color="black", fill="white"), 
            legend.key = element_rect(fill="white",color="white"),
            panel.margin=unit(0,"npc"))
  } else {
    p1 <- p1 + scale_color_manual(values = color_scale, guide=FALSE)
  }
  
  p1
}

# process_spectrum(Sahara,"(Sahara desert in April)",220,280,7,0.8)
make_leah_plot <- function(base_size = 10) {
  sahara <- read_spectrum('data/nimbus/sahara.csv')
  pdf(file = "leah_spectrum.pdf", width=6, height=3, family="Helvetica")
  p <- process_spectrum(sahara, "(Sahara desert in April 1971)", 220, 280, 7, 0.8, add_fahrenheit = T, base_size = base_size) + 
    theme(legend.key.height = unit(0.5, "lines"), legend.text.align = 1)
  print(p)
  dev.off()
}
