library(ggplot2)
library(ggthemes)

data = data.frame(CO2 = c(0,1,10,100,
                          200, 400, 800, 1600),
                  Iout.warm = c(358.274, 352.308, 342.574, 330.642,
                                327.188, 323.734, 319.966, 316.512  ))

data$delta <- data$Iout.warm - data$Iout.warm[1]


ggplot(data,aes(CO2,Iout.warm)) +
  geom_point(size=5) + geom_line() +
  geom_text(aes(x= CO2 + ifelse(CO2 > 500, 0, 30),
                y= Iout.warm + ifelse(CO2 > 500, 2, ifelse( CO2 > 50, 0.5, 0)),
                label=paste(formatC(Iout.warm, digits=2,format='f')),
                hjust= ifelse(CO2 > 500, 0.5, 0), vjust= ifelse(CO2 > 100, 0, 0.5))) +
  scale_x_continuous(limits=c(0,1600), breaks=seq(0,1600,200)) +
  labs(x=expression(paste(CO[2], " (ppm)")),
       y=expression(paste("Intensity (", W / m^2, ")")),
       title = expression(paste("Saturation of ", CO[2], " absorption"))) +
  xlim(0,1600) +
  theme_classic() + theme(text = element_text(size=20),
                          panel.grid.major = element_line(color='gray85',size=0.1),
                          panel.grid.minor = element_line(color="gray90", size=0.1))


ggplot(data,aes(CO2,delta)) +
  geom_point(size=5) + geom_line() +
  geom_text(aes(x= CO2 + ifelse(CO2 > 500, 0, 30),
                y= delta + ifelse(CO2 > 500, 2, ifelse( CO2 > 50, 0.5, 0)),
                label=paste(formatC(delta, digits=2,format='f')),
                hjust= ifelse(CO2 > 500, 0.5, 0), vjust= ifelse(CO2 > 100, 0, 0.5))) +
  scale_x_continuous(limits=c(0,1600), breaks=seq(0,1600,200)) +
  labs(x=expression(paste(CO[2], " (ppm)")),
       y=expression(paste(Delta * I[out], "(", W / m^2, ")")),
       title = expression(paste("Saturation of ", CO[2], " absorption"))) +
  theme_classic() + theme(text = element_text(size=20),
                          panel.grid.major = element_line(color='gray85',size=0.1),
                          panel.grid.minor = element_line(color="gray90", size=0.1))

