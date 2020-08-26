#
# Draw figure for atmospheric layer model
#
library(tidyverse)

make_layer_diagram <- function(n_layers, boundary = TRUE,
                               label_i_vis = FALSE, label_atmos = FALSE,
                               vis_color = "darkblue", ir_color = "darkred",
                               base_size = 10) {
  layer_unit <- 9 / (2 * (n_layers + 1) + n_layers)
  layer_thickness <- layer_unit
  layer_spacing <- 2 * layer_unit

  visible <- tibble(x = 1, xend = 2, y = 10, yend = 1,
                    xlab = 1.05, ylab = 10, just = 0,
                    name = "Visible",
                    i_name = ifelse(is.logical(label_i_vis), "vis",
                                    label_i_vis))

  layers <- tibble(name = c("Earth", paste("Atmospheric Layer", seq(n_layers))),
                   class = c("Earth", rep("Atmosphere", n_layers)),
                   i_name = c("ground", seq(n_layers)),
                   xmin = 0, xmax = 10,
                   ymin = c(0, 1 + seq(n_layers) *
                              (layer_thickness + layer_spacing) -
                              layer_thickness),
                   ymax = c(1, 1 + seq(n_layers) *
                              (layer_thickness + layer_spacing)),
                   x = 5.5,
                   y = c(0.5, 1 + seq(n_layers) *
                           (layer_thickness + layer_spacing) -
                           0.5 * layer_thickness)
  )

  if (n_layers == 1) {
    layers$name[2] <- "Atmospheric Layer"
    if (! isFALSE(label_atmos))
      layers$i_name[2] <- ifelse(is.logical(label_atmos), "atmos",
                                 label_atmos)
  }

  ir <- tibble(x = 5.5, xend = 6.5, y = layers$ymax[n_layers + 1], yend = 10,
               xlab = 6.2, ylab = (y + yend) / 2,  just = 0,
               name = paste0("I['", tail(layers$i_name, 1), ",up']"))
  if (n_layers >= 1) {
    ir_up <- tibble(x = 4, xend = 5, y = head(layers$ymax, -1),
                    yend = tail(layers$ymin, -1),
                    xlab = 4.3, ylab = (y + yend) / 2, just = 1,
                    name = paste0("I['", head(layers$i_name, -1), ",up']"))
    ir_down <- tibble(x = 6, xend = 7, y =  tail(layers$ymin, -1),
                      yend = head(layers$ymax, -1),
                      xlab = 6.7, ylab = (y + yend) / 2, just = 0,
                      name = paste0("I['", tail(layers$i_name, -1), ",down']"))
    ir <- bind_rows(ir, ir_up, ir_down)
  } else {
    ir$name[1] <- "I['ground,up']"
  }

  ir <- na.omit(ir)
  layers <- na.omit(layers)

  p <- ggplot() +
    geom_rect(data = layers, mapping = aes(xmin = xmin, xmax = xmax,
                                           ymin = ymin, ymax = ymax,
                                           fill = class)) +
    geom_segment(data = visible, mapping = aes(x = x, y = y, xend = xend,
                                               yend = yend),
                 arrow = arrow(type = "closed"), size = 1, color = vis_color) +
    geom_segment(data = ir, mapping = aes(x = x, y = y, xend = xend,
                                          yend = yend),
                 arrow = arrow(type = "closed"), size = 1, color = ir_color,
                 linetype = "dashed") +
    geom_text(mapping = aes(x = x, y = y, color = class, label = name),
              data = layers, size = base_size * 1.5, hjust = 0.5,
              vjust = 0.5)  +
    geom_text(data = ir, mapping = aes(x = xlab, y = ylab, label = name,
                                       hjust = just),
              vjust = 0.5, size = base_size, color = ir_color, parse = TRUE) +
    annotate("text", x = visible$xlab, y = visible$ylab,
             label = visible$name, hjust = 0, vjust = 1,
             size = base_size * 1.2, color = vis_color)

  if (! isFALSE(label_i_vis)) {
    lab <- str_c("I['", visible$i_name, ",down']")
    lab_y <- head(ir$ylab, 1)
    lab_x <- visible$xlab + 0.25 + (visible$xend - visible$x) *
      (visible$y - lab_y) / (visible$y - visible$yend)
    p <- p + annotate("text", x = lab_x, y = lab_y, label = lab,
                      hjust = 0, vjust = 1, size = base_size,
                      color = vis_color, parse = TRUE)
  }

  p <- p +
    scale_fill_manual(values = c(Earth = "gray30", Atmosphere = "gray80"),
                      guide = "none") +
    scale_color_manual(values = c(Earth = "white", Atmosphere = "black"),
                       guide = "none") +
    scale_x_continuous(limits = c(0, 10), expand = expansion(0, 0)) +
    scale_y_continuous(limits = c(0,10), expand = expansion(c(0, 0.05), 0)) +
    theme_bw(base_size = base_size) +
    theme(axis.text = element_blank(), axis.title = element_blank(),
          axis.ticks = element_blank(), panel.grid = element_blank())

  if (boundary) {
    boundary_y <- 10 - 0.25 * layer_spacing
    p <- p +
      geom_hline(yintercept = boundary_y, linetype = "dotted", size = 1) +
      annotate("text", x = 9.95, y = boundary_y + 0.1, hjust = 1, vjust = 0,
               label = "Boundary to space", color = "black",
               size = base_size)
  }

  p
}


make_nuclear_winter_diagram <- function(boundary = TRUE) {
  n_layers <- 1
  layer_unit <- 9 / (2 * (n_layers + 1) + n_layers)
  layer_thickness <- layer_unit
  layer_spacing <- 2 * layer_unit

  layers <- tibble(name = c("Earth", "Dusty atmosphere"),
                   class = c("Earth", "Atmosphere"),
                   xmin = 0, xmax = 10,
                   ymin = c(0, 1 + seq(n_layers) *
                              (layer_thickness + layer_spacing) -
                              layer_thickness),
                   ymax = c(1, 1 + seq(n_layers) *
                              (layer_thickness + layer_spacing)),
                   x = 5.5,
                   y = c(0.5, 1 + seq(n_layers) *
                           (layer_thickness + layer_spacing) -
                           0.5 * layer_thickness)
  )

  visible <- tibble(x = 1, xend = 2, y = 10, yend = max(layers$ymax),
                    xlab = 1.05, ylab = 10, just = 0,
                    name = "Visible")


  ir <- tibble(x = 5.5, xend = 6.5, y = layers$ymax[n_layers + 1], yend = 10,
               xlab = 6.2, ylab = (y + yend) / 2,  just = 0,
               name = paste0("I['", n_layers, ",up']"))
  if (n_layers >= 1) {
    ir_up <- tibble(x = 4, xend = 5, y = head(layers$ymax, -1),
                    yend = tail(layers$ymin, -1),
                    xlab = 4.3, ylab = (y + yend) / 2, just = 1,
                    name = str_c("I['",
                                 c("ground", seq(n_layers - 1))[1:n_layers],
                                 ",up']"))
    ir_down <- tibble(x = 6, xend = 7, y =  tail(layers$ymin, -1),
                      yend = head(layers$ymax, -1),
                      xlab = 6.7, ylab = (y + yend) / 2, just = 0,
                      name = paste0("I['", seq(n_layers), ",down']"))
    ir <- bind_rows(ir, ir_up, ir_down)
  } else {
    ir$name[1] <- "I['ground,up']"
  }

  ir <- na.omit(ir)
  layers <- na.omit(layers)

  p <- ggplot() +
    geom_rect(data = layers, mapping = aes(xmin = xmin, xmax = xmax,
                                           ymin = ymin, ymax = ymax,
                                           fill = class)) +
    geom_segment(data = visible, mapping = aes(x = x, y = y, xend = xend,
                                               yend = yend),
                 arrow = arrow(type = "closed"), size = 1) +
    geom_segment(data = ir, mapping = aes(x = x, y = y, xend = xend,
                                          yend = yend),
                 arrow = arrow(type = "closed"), size = 1,
                 linetype = "dashed") +
    geom_text(mapping = aes(x = x, y = y, color = class, label = name),
              data = layers, size = 7, hjust = 0.5, vjust = 0.5)  +
    geom_text(data = ir, mapping = aes(x = xlab, y = ylab, label = name,
                                       hjust = just), vjust = 0.5, size = 5,
              parse = TRUE) +
    annotate("text", x = visible$xlab, y = visible$ylab, label = visible$name,
             hjust = 0, vjust = 1, size = 5) +
    xlim(0, 10) + ylim(0, 10) +
    scale_fill_manual(values = c(Earth = "gray30", Atmosphere = "gray60"),
                      guide = "none") +
    scale_color_manual(values = c(Earth = "white", Atmosphere = "black"),
                       guide = "none") +
    theme_bw() +
    theme(axis.text = element_blank(), axis.title = element_blank(),
          axis.ticks = element_blank(), panel.grid = element_blank())

  if (boundary) {
    boundary_y <- 10 - 0.25 * layer_spacing
    p <- p +
      geom_hline(yintercept = boundary_y, linetype = "dotted", size = 1) +
      annotate("text", x = 10, y = boundary_y + 0.1, hjust = 1, vjust = 0,
               label = "Boundary to space", color = "black", size = 5)
  }

  p
}
