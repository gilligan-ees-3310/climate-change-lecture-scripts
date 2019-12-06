library(ggplot2)
library(ggthemes)
library(scales)
library(grid)

plot_t_dice <- function(t_data) {
  p <- ggplot(t_data, aes(x=year,y=value, color=Growth, shape=Growth, linetype=Growth)) +
    geom_line(size=I(1)) +
    geom_point(data=t_data[t_data$year %in% c(2005, 2100, 2200),], size=I(6)) +
    scale_color_brewer(type='qual',palette='Dark2') +
    labs(y=expression(paste("Temperature increase  ", (degree * C)))) +
    theme_classic(base_size=30) +
    theme(legend.title=element_blank(),legend.position=c(0.1,1),
          legend.justification=c(0,1), legend.key.size=unit(0.05,'npc'))
  p
}

plot_cons_dice <- function(g_data) {
  p <- ggplot(g_data, aes(x=year,y=value, color=Growth, shape=Growth, linetype=Growth)) +
    geom_line(size=I(1)) +
    geom_point(data=g_data[g_data$year %in% c(2005, 2100, 2200),], size=I(6)) +
    scale_color_brewer(type='qual',palette='Dark2') +
    labs(y="Consumption per capita (2005$)") +
    theme_classic(base_size=30) +
    theme(legend.title=element_blank(),legend.position=c(0.1,1),
          legend.justification=c(0,1), legend.key.size=unit(0.05,'npc'))
  p
}


