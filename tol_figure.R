#
# New fiture based on the IPCC WG2 table for figure 10-17 Figure 10.
#
library(tidyverse)
library(stringr)


read_tol_data <- function() {
  data <- read.csv(file.path(data_dir, 'tol_damages', 'tol_figures.csv'), 
                   header=TRUE, stringsAsFactors = F)
  data$class <- factor(data$age == 0 & data$error == 0, levels = c(FALSE, TRUE), 
                       labels = c('false', 'true'))
  data$age <- ordered(data$age, levels = 0:2, 
                      labels = c('original', 'gremlin omission', 'recent'))
  data$error <- ordered(data$error, levels = c(1,0,2), 
                        labels = c('gremlin error', 'valid', 'corrected'))
  invisible(data)
}

plot_tol_data <- function(tol_data, as.damages = TRUE) {
  if (as.damages) {
    tol_data$impact <- - tol_data$impact
    damage_caption = "Damages as percent of global GDP"
    yrange = c(-5,15)
    legend_pos = c(0.05,0.94)
    legend_just = c(0,1)
  } else {
    damage_caption = "Percent change in global GDP"
    yrange = c(-15,5)
    legend_pos = c(0.05, 0.06)
    legend_just = c(0,0)
  }
  p <- ggplot(tol_data, aes(x = warming, y = impact, 
                            color = age, shape = error, alpha = class)) +
    geom_point(size = 5) +
    geom_hline(yintercept = 0) +
    scale_x_continuous(limits = c(0,6), breaks = 0:6) +
    scale_y_continuous(limits = yrange, breaks = seq(yrange[1],yrange[2],5),
                       minor_breaks = seq(yrange[1],yrange[2],1)) +
    scale_color_manual(name = '', values = c('original' = 'darkblue', 
                                  'gremlin omission' = 'darkorange', 
                                  'recent' = 'darkred'), 
                       guide = guide_legend(reverse = TRUE)) +
    scale_shape_manual(values = c('valid' = 16, 'gremlin error' = 13, 'corrected' = 17),
                       name = '', guide = guide_legend(reverse = TRUE)) +
    scale_alpha_manual(values = c(false = 1, true = 0.3), guide = FALSE) + 
    labs(x = expression(paste("Warming ", (degree * C))),
         y = damage_caption) +
    theme(legend.position = legend_pos, legend.justification = legend_just,
          legend.box.just = "left", legend.title = element_blank())
  p
}

plot_original_tol_data <- function(tol_data, as.damages = TRUE) {
  if (as.damages) {
    tol_data$impact <- - tol_data$impact
    damage_caption = "Damages as percent of global GDP"
    yrange = c(-5,15)
  } else {
    damage_caption = "Percent change in global GDP"
    yrange = c(-15,5)
  }
  p <- ggplot(tol_data %>% filter(age == 'original'), 
              aes(x = warming, y = impact, 
                  color = age, alpha = class)) +
    geom_point(size = 5, alpha = 0.3) +
    geom_hline(yintercept = 0) +
    scale_x_continuous(limits = c(0,6), breaks = 0:6) +
    scale_y_continuous(limits = yrange, breaks = seq(yrange[1],yrange[2],5),
                       minor_breaks = seq(yrange[1],yrange[2],1)) +
    scale_color_manual(name = '', values = c('original' = 'darkblue', 
                                             'gremlins (2014)' = 'darkorange', 
                                             'recent' = 'darkred'), 
                       guide = FALSE) +
    scale_shape_manual(values = c('valid' = 16, 'gremlin error' = 13, 'corrected' = 17),
                       name = '', guide = FALSE) +
    labs(x = expression(paste("Warming ", (degree * C))),
         y = damage_caption)
  p
}
