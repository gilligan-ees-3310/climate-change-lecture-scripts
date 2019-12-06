library(tidyverse)
library(ggthemes)
library(RColorBrewer)
library(scales)

econ <- data.frame(lab = c('benefit', 'cost'), slope = c(-0.4, 0.2), zero = c(1, 0))
rownames(econ) <- econ$lab
econ$lab <- ordered(econ$lab, levels = c('benefit', 'cost', 'benefit.perc'))

base.size <- 30
line.width <- 1
point.size <- 6
text.size <- 8

xvjust <- 2.5
yvjust <- -1
rm.delta <- 0
bm.delta <- 0

aspect_ratio = 7

set_aspect_ratio = function(aspect_ratio) {
  assign("aspect_ratio", aspect_ratio, envir = globalenv())
}

fill_scale <- scale_fill_manual(values = c(benefit = RColorBrewer::brewer.pal(5, "Blues")[1],
                                           cost = RColorBrewer::brewer.pal(5,"Blues")[3],
                                           benefit.perc = RColorBrewer::brewer.pal(5, "Blues")[5]),
                                drop = FALSE)


icpt <- function(a, b) {
  x <- (b$zero - a$zero) / (a$slope - b$slope)
  y <- a$zero + x * a$slope
  data.frame(x = x, y = y)
}

calc_y <- function(x, params, which = NULL) {
  if (! is.null(which))
    params <- filter(params, lab %in% c(which))
  params$zero + params$slope * x
}

calc_limits <- function(params) {
  x0 <- icpt(params['benefit',], params['cost',])$x
  x <- min(2 * x0, - params['benefit','zero'] / params['benefit','slope'])
  y  <- max(params$zero, calc_y(x, params))
  data.frame(x = c(0,x), y = c(0,y))
}

g_limits <- calc_limits(econ)

angle <- function(params, which, limits = g_limits,
                  aspect = data.frame(x = 1, y = aspect_ratio)) {
  if (is.null(limits)) {
    limits <- calc_limits(params)
  }
  if (is.null(aspect)) {
    aspect <- data.frame(x = 1, y = 1)
  }

  message("Aspect: (", aspect$x, ", ", aspect$y, ")")
  yrange = do.call('-', as.list(rev(range(limits$y))))
  xrange = do.call('-', as.list(rev(range(limits$x))))
  slope = list(x = xrange * aspect$x,
               y = yrange * aspect$y * params[params$lab == which, 'slope'])
  message("Slope: x = ", slope$x, ", y = ", slope$y)
  angle = atan2(slope$y, slope$x) * 180 / pi
  message("Angle = ", angle)
  angle
}

insert_layer <- function(p, after = 0, ...) {
  if (after < 0)
    after <- after + length(p$layers)
  if (!length(p$layers))
    p$layers <- list(...)
  else
    p$layers <- append(p$layers, list(...), after)
  p
}


update_scale <- function(p, name, ...) {
  args <- list(...)
  index = which(p$scales$find(name))
  new = p$scales$scales[[index]]$clone()
  for(a in names(args)) {
    new[[a]] <- args[[a]]
  }
  p$scales$scales[[index]] <- new
  p
}


equilibrium <- icpt(econ[econ$lab == 'benefit',], econ[econ$lab == 'cost',])
xmax <- max(g_limits$x)

x.lab.b <- 0.07 * xmax
x.lab.c <- 0.95 * xmax

low_emissions <- equilibrium$x * 0.8
low_emissions_data <- data.frame(x = low_emissions,
                                 y = calc_y(low_emissions, econ),
                                 lab = econ$lab,
                                 zero = 0)
rownames(low_emissions_data) <- low_emissions_data$lab
high_emissions <- equilibrium$x * 1.2
high_emissions_data <- data.frame(x = high_emissions,
                                  y = calc_y(high_emissions, econ),
                                  lab = econ$lab,
                                  zero = 0)
rownames(high_emissions_data) <- high_emissions_data$lab


set_cb_theme <- function(base_size) {
  theme_set(theme_classic(base_size = base.size) +
              theme(axis.title.x = element_text(hjust = 1),
                    axis.title.y = element_text(hjust = 1)
              )
  )
}

set_cb_theme(base_size)

gen_ribbon <- function(params, xrange = c(NA,NA), zero = FALSE) {
  if (is.null(xrange)) {
    xrange <- c(0,equilibrium$x)
  } else {
    if (is.na(xrange[1])) xrange[1] <- 0
    if (is.na(xrange[2])) xrange[2] <- equilibrium$x
  }
  if (equilibrium$x > min(xrange) && equilibrium$x < max(xrange)) {
    xrange <- c(equilibrium$x, xrange)
    xrange <- xrange[order(xrange)]
  }
  if (zero || nrow(params) == 1) {
    df <- expand.grid(x = xrange, lab = params$lab) %>%  group_by(lab) %>%
      mutate(ymin = 0, ymax = calc_y(x, params, as.character(lab))) %>%
      ungroup()
  } else {
    df <- data.frame(x = xrange) %>% group_by(x) %>%
      mutate(ymin = min(calc_y(x, params)), ymax = max(calc_y(x, params))) %>%
      ungroup()
  }
  df
}


ribbon.z <- gen_ribbon(econ, zero=TRUE)
ribbon.low.z <- gen_ribbon(econ, c(0, low_emissions), TRUE)
ribbon.high.z <- gen_ribbon(econ, c(0, high_emissions), TRUE)

annotate_cb <- function(which = c("both", "benefit", "cost"),
                        aspect = aspect_ratio, dy_b = 0.03, dy_c = 0.01) {
  which = match.arg(which)
  aspect_df = data.frame(x = 1, y = aspect)
  annot_df <- tibble(x = numeric(0), y = numeric(0),
                     hjust = numeric(0), vjust = numeric(0),
                     angle = numeric(0), size = numeric(0),
                     label = character(0))
  if (which %in% c("both", "benefit")) {
    annot_df = annot_df %>% bind_rows(
      tibble(x = x.lab.b,
             y = dy_b + calc_y(x.lab.b, econ, 'benefit'),
             angle = angle(econ, 'benefit', aspect = aspect_df),
             size = text.size,
             hjust = 0, vjust = 0,
             label = "MB[society]"))
  }
  if (which %in% c("both", "cost")) {
    annot_df = annot_df %>% bind_rows(
      tibble(x = x.lab.c,
             y = dy_c + calc_y(x.lab.c, econ, 'cost'),
             angle = angle(econ, 'cost', aspect = aspect_df),
             size = text.size,
             hjust = 1, vjust = 0,
             label = "MC[society]"))
  }
  geom_text(aes(x = x, y = y, angle = I(angle),
                 size = I(size), hjust = I(hjust), vjust = I(vjust),
                 label = label), data = annot_df, parse = TRUE)
}

p0 <- function() {
  ggplot(equilibrium) +
    geom_point(aes(x = x, y = y), size = point.size) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    scale_x_continuous(expand = c(0,0), limits = c(0,xmax),
                       breaks = NULL, name = "Emissions") +
    scale_y_continuous(expand = c(0,0),
                       limits = c(0, max(econ$zero, calc_y(xmax, econ))),
                       breaks = NULL, name = "Marginal Cost, Benefit") +
    scale_color_manual(values = c(benefit = 'black', cost = 'black', benefit.perc = 'orchid')) +
    # coord_equal() +
    theme(legend.position = 'none')
}

fig.2.a <-function(){
  p0() %+% filter(low_emissions_data, lab == 'benefit') %>%
    update_scale('x', breaks = c(low_emissions), labels = c('E')) %>%
    update_scale('y', breaks = c(low_emissions_data['benefit','y']), labels = c('B')) +
    geom_ribbon(data = ribbon.low.z,
                aes(x = x, ymin = ymin, ymax = ymax,
                    color = NA, fill = lab,
                    alpha = lab)) +
    geom_abline(data = filter(econ, lab == 'benefit'),
                aes(slope = slope, intercept = zero, color = lab), size = line.width) +
    annotate_cb("benefit") +
    fill_scale +
    scale_alpha_manual(values = c(benefit = 1, cost = 0, benefit.perc = 0)) +
    geom_segment(aes(x = x, y = y, xend = I(0), yend = y),
                 color = "black", linetype = 'dashed', size = line.width) +
    geom_segment(aes(x = x, y = y, yend = I(0), xend = x),
                 color = "black", linetype = 'dashed', size = line.width) +
    # coord_equal() +
    theme(axis.title.x = element_text(vjust = xvjust), axis.title.y = element_text(vjust = yvjust),
          plot.margin = grid::unit(c(1,1,0.5+bm.delta,0.5+rm.delta), 'lines'))
}


fig.2.b <- function() {
  p0() %+% filter(low_emissions_data, lab == 'cost') %>%
    update_scale('x', breaks = c(low_emissions), labels = c('E')) %>%
    update_scale('y', breaks = c(low_emissions_data['cost','y']), labels = c('C')) +
    geom_ribbon(data = ribbon.low.z,
                aes(x = x, ymin = ymin, ymax = ymax,
                    color = NA, fill = lab,
                    alpha = lab)) +
    geom_abline(data = filter(econ, lab == 'cost'),
                aes(slope = slope, intercept = zero, color = lab), size = line.width) +
    annotate_cb("cost") +
    fill_scale +
    scale_alpha_manual(values = c(benefit = 0, cost = 1, benefit.perc = 0)) +
    geom_segment(aes(x = x, y = y, xend = I(0), yend = y),
                 color = "black", linetype = 'dashed', size = line.width) +
    geom_segment(aes(x = x, y = y, yend = I(0), xend = x),
                 color = "black", linetype = 'dashed', size = line.width) +
    # coord_equal() +
    theme(axis.title.x = element_text(vjust = xvjust), axis.title.y = element_text(vjust = yvjust),
          plot.margin = grid::unit(c(1,1,0.5+bm.delta,0.5+rm.delta), 'lines'))
}

p1 <- function() {
  ggplot(equilibrium) +
    geom_point(aes(x = x, y = y), size = 0) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    scale_x_continuous(expand = c(0,0), limits = c(0,xmax),
                       breaks = c(low_emissions), labels = c('E'),
                       name = "Emissions") +
    scale_y_continuous(expand = c(0,0),
                       limits = c(0, max(econ$zero, calc_y(xmax, econ))),
                       breaks = unlist(low_emissions_data$y),
                       labels = c('B','C'),
                       name = "Marginal Cost, Benefit") +
    scale_color_manual(values = c(benefit = 'black', cost = 'black', benefit.perc = 'gray40')) +
    # coord_equal() +
    theme(legend.position = 'none',
          axis.title.x = element_text(vjust = xvjust), axis.title.y = element_text(vjust = yvjust),
          plot.margin = grid::unit(c(1,1,0.5+bm.delta,0.5+rm.delta), 'lines'))
}

fig.2.c <- function() {
  p1() +
    geom_ribbon(data = ribbon.low.z,
                aes(x = x, ymin = ymin, ymax = ymax,
                    color = NA, fill = lab,
                    alpha = lab)) +
    geom_abline(data = econ,
                aes(slope = slope, intercept = zero, color = lab), size = line.width) +
    annotate_cb("both") +
    fill_scale +
    scale_alpha_manual(values = c(benefit = 1, cost = 1, benefit.perc = 0)) +
    geom_segment(aes(x = x, y = y, yend = y, xend = zero), data = low_emissions_data,
                 color = "black", linetype = 'dashed', size = line.width) +
    geom_segment(aes(x = x, y = y, yend = zero, xend = x), data = low_emissions_data['benefit',],
                 color = "black", linetype = 'dashed', size = line.width)
}

ribbon.low.i <- gen_ribbon(econ, c(0, low_emissions), FALSE)

fig.2.d <- function() {
  p1()  %>%
    update_scale('x', breaks = c(low_emissions, equilibrium$x),
                 labels = c('E', 'E*')) +
    geom_ribbon(data = ribbon.low.i,
                aes(x = x, ymin = ymin, ymax = ymax,
                    color = NA, fill = 'benefit')) +
    geom_point(aes(x = x, y = y), size = point.size, color = 'black') +
    geom_abline(data = econ,
                aes(slope = slope, intercept = zero, color = lab), size = line.width) +
    annotate_cb("both") +
    fill_scale +
    geom_segment(aes(x = x, y = y, yend = y, xend = zero), data = low_emissions_data,
                 color = "black", linetype = 'dashed', size = line.width) +
    geom_segment(aes(x = x, y = y, yend = zero, xend = x), data = low_emissions_data['benefit',],
                 color = "black", linetype = 'dashed', size = line.width) +
    geom_segment(aes(x = x, y = y, yend = I(0), xend = x), data = equilibrium,
                 color = "black", linetype = 'dashed', size = line.width)
}

fig.3 <- function() {
  p0() %>% update_scale('x', breaks = c(equilibrium$x), labels = c('E*')) +
    geom_abline(data = econ,
                aes(slope = slope, intercept = zero, color = lab), size = line.width) +
    annotate_cb("both") +
    geom_segment(aes(x = x, y = y, yend = I(0), xend = x), data = equilibrium,
                 color = "black", linetype = 'dashed', size = line.width) +
    theme(axis.title.x = element_text(vjust = xvjust),
          plot.margin = grid::unit(c(1,1, 0.5 + bm.delta, 0.5 + rm.delta), 'lines'))
}

fig.4.a <- function() {
  p0() %>% update_scale('x', breaks = c(equilibrium$x), labels = c('E*')) +
    geom_ribbon(data = ribbon.z,
                aes(x = x, ymin = ymin, ymax = ymax,
                    color = NA, fill = lab, alpha = lab)) +
    geom_point(aes(x = x, y = y), size = point.size, color = 'black') +
    geom_abline(data = econ,
                aes(slope = slope, intercept = zero, color = lab), size = line.width) +
    annotate_cb("both") +
    geom_segment(aes(x = x, y = y, yend = I(0), xend = x), data = equilibrium,
                 color = "black", linetype = 'dashed', size = line.width) +
    fill_scale +
    scale_alpha_manual(values = c(benefit = 1, cost = 0, benefit.perc = 0)) +
    theme(axis.title.x = element_text(vjust = xvjust),
          plot.margin = grid::unit(c(1,1, 0.5 + bm.delta, 0.5 + rm.delta), 'lines'))
}

fig.4.b <- function() {
  p0() %>% update_scale('x', breaks = c(equilibrium$x), labels = c('E*')) +
    geom_ribbon(data = ribbon.z,
                aes(x = x, ymin = ymin, ymax = ymax,
                    color = NA, fill = lab, alpha = lab)) +
    geom_point(aes(x = x, y = y), size = point.size, color = 'black') +
    geom_abline(data = econ,
                aes(slope = slope, intercept = zero, color = lab), size = line.width) +
    annotate_cb("both") +
    geom_segment(aes(x = x, y = y, yend = I(0), xend = x), data = equilibrium,
                 color = "black", linetype = 'dashed', size = line.width) +
    fill_scale +
    scale_alpha_manual(values = c(benefit = 1, cost = 1, benefit.perc = 0)) +
    theme(axis.title.x = element_text(vjust = xvjust),
          plot.margin = grid::unit(c(1,1, 0.5 + bm.delta, 0.5 + rm.delta), 'lines'))
}

ribbon.high.i.dw <- gen_ribbon(econ, c(equilibrium$x, high_emissions), FALSE)

fig.4.c <- function() {
  p0()  %>%
    update_scale('x', breaks = c(equilibrium$x, high_emissions),
                 labels = c('E*', 'E')) +
    geom_ribbon(data = ribbon.i,
                aes(x = x, ymin = ymin, ymax = ymax,
                    color = NA, fill = 'benefit')) +
    geom_ribbon(data = ribbon.high.i.dw,
                aes(x = x, ymin = ymin, ymax = ymax,
                    color = NA, fill = 'cost')) +
    geom_point(aes(x = x, y = y), size = point.size, color = 'black') +
    geom_abline(data = econ,
                aes(slope = slope, intercept = zero, color = lab), size = line.width) +
    annotate_cb("both") +
    fill_scale +
    geom_segment(aes(x = x, y = y, yend = zero, xend = x), data = high_emissions_data['benefit',],
                 color = "black", linetype = 'dashed', size = line.width) +
    geom_segment(aes(x = x, y = y, yend = I(0), xend = x), data = equilibrium,
                 color = "black", linetype = 'dashed', size = line.width)
}


ribbon.i <- gen_ribbon(econ, c(0, equilibrium$x), FALSE)

fig.4.d <- function() {
  p0()  %>%
    update_scale('x', breaks = c(equilibrium$x, high_emissions),
                 labels = c('E*', 'E')) +
    geom_ribbon(data = ribbon.i,
                aes(x = x, ymin = ymin, ymax = ymax,
                    color = NA, fill = 'benefit')) +
    geom_point(aes(x = x, y = y), size = point.size, color = 'black') +
    geom_abline(data = econ,
                aes(slope = slope, intercept = zero, color = lab), size = line.width) +
    annotate_cb("both") +
    fill_scale +
    geom_segment(aes(x = x, y = y, yend = zero, xend = x), data = high_emissions_data['benefit',],
                 color = "black", linetype = 'dashed', size = line.width) +
    geom_segment(aes(x = x, y = y, yend = I(0), xend = x), data = equilibrium,
                 color = "black", linetype = 'dashed', size = line.width)
}

fig.4.e <- function() {
  p1()  %>%
    update_scale('x', breaks = c(low_emissions, equilibrium$x),
                 labels = c("E'", 'E*')) +
    geom_ribbon(data = ribbon.low.i,
                aes(x = x, ymin = ymin, ymax = ymax,
                    color = NA, fill = 'benefit')) +
    geom_point(aes(x = x, y = y), size = point.size, color = 'black') +
    geom_abline(data = econ,
                aes(slope = slope, intercept = zero, color = lab), size = line.width) +
    annotate_cb("both") +
    fill_scale +
    geom_segment(aes(x = x, y = y, yend = zero, xend = x), data = low_emissions_data['benefit',],
                 color = "black", linetype = 'dashed', size = line.width) +
    geom_segment(aes(x = x, y = y, yend = I(0), xend = x), data = equilibrium,
                 color = "black", linetype = 'dashed', size = line.width)
}

ribbon.dw.4.f = gen_ribbon(econ, xrange = c(low_emissions,equilibrium$x), zero = FALSE)

fig.4.f <- function() {
  {
    p0()  %>%
      update_scale('x', breaks = c(low_emissions, equilibrium$x),
                   labels = c("E'", 'E*')) +
      geom_ribbon(data = ribbon.dw.4.f,
                  aes(x = x, ymin = ymin, ymax = ymax,
                      color = NA, fill = 'cost')) +
      geom_point(aes(x = x, y = y), size = point.size, color = 'black') +
      geom_abline(data = econ,
                  aes(slope = slope, intercept = zero, color = lab), size = line.width) +
      annotate_cb("both") +
      fill_scale +
      geom_segment(aes(x = x, y = y, yend = zero, xend = x), data = low_emissions_data['benefit',],
                   color = "black", linetype = 'dashed', size = line.width) +
      geom_segment(aes(x = x, y = y, yend = I(0), xend = x), data = equilibrium,
                   color = "black", linetype = 'dashed', size = line.width)
  }
}

econ_perc.1 <- rbind(econ,
                     benefit.perc = mutate(econ['benefit',],
                                           lab = 'benefit.perc', slope = 1.5 * slope,
                                           zero = 1.5 * zero))

econ_perc.2 <- rbind(econ,
                     benefit.perc = mutate(econ['benefit',],
                                           lab = 'benefit.perc', slope = 1.25 * slope))

e.perc.1 <- icpt(econ_perc.1['cost',],econ_perc.1['benefit.perc',])
ribbon.perc.1 <- gen_ribbon(econ_perc.1, xrange = c(0,e.perc.1$x), zero = TRUE)
ribbon.dw.1 <- gen_ribbon(econ, xrange = c(e.perc.1$x,equilibrium$x), zero = FALSE)
# x.perc.1 <- - with(econ_perc.1['benefit.perc',], zero / slope)
x.perc.1 <- (with(filter(econ_perc.1, lab == "benefit"), zero + x.lab.b * slope) -
               filter(econ_perc.1, lab == "benefit.perc")$zero) /
  filter(econ_perc.1, lab == "benefit.perc")$slope

a.coord.1 <- expand.grid(x = c(e.perc.1$x, equilibrium$x), lab = c('cost','benefit'),
                         stringsAsFactors = F) %>%
  group_by(lab) %>%  mutate(y = calc_y(x, econ_perc.1, lab)) %>% ungroup() %>%
  summarize(x = mean(x), y = mean(y) - 0.01)



e.perc.2 <- icpt(econ_perc.2['cost',],econ_perc.2['benefit.perc',])
ribbon.perc.2 <- gen_ribbon(econ_perc.2, xrange = c(0,e.perc.2$x), zero = TRUE)
ribbon.dw.2 <- gen_ribbon(econ, xrange = c(e.perc.2$x,equilibrium$x), zero = FALSE)
# x.perc.2 <- - with(econ_perc.2['benefit.perc',], zero / slope)
x.perc.2 <- (with(filter(econ_perc.2, lab == "benefit"), zero + x.lab.b * slope) -
               filter(econ_perc.2, lab == "benefit.perc")$zero) /
  filter(econ_perc.2, lab == "benefit.perc")$slope
a.coord.2 <- expand.grid(x = c(e.perc.2$x, equilibrium$x), lab = c('cost','benefit'),
                         stringsAsFactors = F) %>%
  group_by(lab) %>% mutate(y = calc_y(x, econ_perc.2, lab)) %>% ungroup() %>%
  summarize(x = mean(x), y = mean(y) - 0.01)

econ_perc <- econ_perc.2
e.perc <- e.perc.2
x.perc <- x.perc.2
ribbon.perc <- ribbon.perc.2
ribbon.dw <- ribbon.dw.2
a.coord <- a.coord.2


fig.5.a <- function() {
  econ_perc <- econ_perc.2
  e.perc <- e.perc.2
  x.perc <- x.perc.2
  ribbon.perc <- ribbon.perc.2
  ribbon.dw <- ribbon.dw.2
  a.coord <- a.coord.2

  p0() %>% update_scale('x', breaks = c(equilibrium$x, e.perc$x), labels = c('E*','E\'')) +
    geom_abline(data = econ_perc,
                aes(slope = slope, intercept = zero, color = lab), size = line.width) +
    geom_text(x = x.lab.b,
              y = 0.03 + calc_y(x.lab.b, econ_perc, 'benefit'),
              angle = angle(econ_perc, 'benefit'),
              size = text.size,
              hjust = 0, vjust = 0,
              label = "MB[society]", parse=TRUE) +
    geom_text(x = x.perc,
              y = -0.015 + calc_y(x.perc, econ_perc, 'benefit.perc'),
              angle = angle(econ_perc, 'benefit.perc'),
              size = text.size,
              hjust = 0, vjust = 1,
              label = "MB[perceived]", parse=TRUE) +
    geom_text(x = x.lab.c,
              y = 0.01 + calc_y(x.lab.c, econ_perc, 'cost'),
              angle = angle(econ_perc, 'cost'),
              size = text.size,
              hjust = 1, vjust = 0,
              label = "MC[society]", parse=TRUE) +
    geom_segment(aes(x = x, y = y, yend = I(0), xend = x), data = equilibrium,
                 color = "black", linetype = 'dashed', size = line.width) +
    geom_segment(aes(x = x, y = y, yend = I(0), xend = x), data = e.perc,
                 color = "black", linetype = 'dashed', size = line.width) +
    theme(axis.title.x = element_text(vjust = xvjust),
          plot.margin = grid::unit(c(1,1, 0.5 + bm.delta, 0.5 + rm.delta), 'lines'))
}

fig.5.b <- function() {
  econ_perc <- econ_perc.1
  e.perc <- e.perc.1
  x.perc <- x.perc.1
  ribbon.perc <- ribbon.perc.1
  ribbon.dw <- ribbon.dw.1
  a.coord <- a.coord.1

  p0() %>% update_scale('x', breaks = c(equilibrium$x, e.perc$x), labels = c('E*','E\'')) +
    geom_abline(data = econ_perc,
                aes(slope = slope, intercept = zero, color = lab), size = line.width) +
    geom_text(x = x.lab.b,
              y = -0.01 + calc_y(x.lab.b, econ_perc, 'benefit'),
              angle = angle(econ_perc, 'benefit'),
              size = text.size,
              hjust = 0, vjust = 1,
              label = "MB[society]", parse=TRUE) +
    geom_text(x = x.perc,
              y = 0.03 + calc_y(x.perc, econ_perc, 'benefit.perc'),
              angle = angle(econ_perc, 'benefit.perc'),
              size = text.size,
              hjust = 0, vjust = 0,
              label = "MB[perceived]", parse=TRUE) +
    geom_text(x = x.lab.c,
              y = 0.01 + calc_y(x.lab.c, econ_perc, 'cost'),
              angle = angle(econ_perc, 'cost'),
              size = text.size,
              hjust = 1, vjust = 0,
              label = "MC[society]", parse=TRUE) +
    geom_segment(aes(x = x, y = y, yend = I(0), xend = x), data = equilibrium,
                 color = "black", linetype = 'dashed', size = line.width) +
    geom_segment(aes(x = x, y = y, yend = I(0), xend = x), data = e.perc,
                 color = "black", linetype = 'dashed', size = line.width) +
    theme(axis.title.x = element_text(vjust = xvjust),
          plot.margin = grid::unit(c(1,1, 0.5 + bm.delta, 0.5 + rm.delta), 'lines'))
}

fig.6.a <- function() {
  econ_perc <- econ_perc.2
  e.perc <- e.perc.2
  x.perc <- x.perc.2
  ribbon.perc <- ribbon.perc.2
  ribbon.dw <- ribbon.dw.2
  a.coord <- a.coord.2
  x.lab.b <- 0.25 * equilibrium$x
  x.lab.c <- 0.99 * xmax

  p0() %>% update_scale('x', breaks = c(equilibrium$x, e.perc$x), labels = c('E*','E\'')) +
    geom_ribbon(data = ribbon.perc,
                aes(x = x, ymin = ymin, ymax = ymax,
                    color = NA, fill = lab, alpha = lab)) +
    geom_abline(data = econ_perc,
                aes(slope = slope, intercept = zero, color = lab), size = line.width) +
    geom_text(x = x.lab.b,
              y = 0.03 + calc_y(x.lab.b, econ_perc, 'benefit'),
              angle = angle(econ_perc, 'benefit'),
              size = text.size,
              hjust = 0, vjust = 0,
              label = "MB[society]", parse=TRUE) +
    geom_text(x = x.lab.b,
              y = -0.015 + calc_y(x.lab.b, econ_perc, 'benefit.perc'),
              angle = angle(econ_perc, 'benefit.perc'),
              size = text.size,
              hjust = 0, vjust = 1,
              label = "MB[perceived]", parse=TRUE) +
    geom_text(x = x.lab.c,
              y = 0.01 + calc_y(x.lab.c, econ_perc, 'cost'),
              angle = angle(econ_perc, 'cost'),
              size = text.size,
              hjust = 1, vjust = 0,
              label = "MC[society]", parse=TRUE) +
    geom_segment(aes(x = x, y = y, yend = I(0), xend = x), data = equilibrium,
                 color = "black", linetype = 'dashed', size = line.width) +
    geom_segment(aes(x = x, y = y, yend = I(0), xend = x), data = e.perc,
                 color = "black", linetype = 'dashed', size = line.width) +
    fill_scale +
    scale_alpha_manual(values = c(benefit = 1, cost = 0, benefit.perc = 0)) +
    theme(axis.title.x = element_text(vjust = xvjust),
          plot.margin = grid::unit(c(1,1, 0.5 + bm.delta, 0.5 + rm.delta), 'lines'))
}

fig.6.b <- function() {
  econ_perc <- econ_perc.2
  e.perc <- e.perc.2
  x.perc <- x.perc.2
  ribbon.perc <- ribbon.perc.2
  ribbon.dw <- ribbon.dw.2
  a.coord <- a.coord.2

  x.lab.b <- 0.25 * equilibrium$x
  x.lab.c <- 0.99 * xmax

  p0() %>% update_scale('x', breaks = c(equilibrium$x, e.perc$x), labels = c('E*','E\'')) +
    geom_ribbon(data = ribbon.perc,
                aes(x = x, ymin = ymin, ymax = ymax,
                    color = NA, fill = lab, alpha = lab)) +
    geom_abline(data = econ_perc,
                aes(slope = slope, intercept = zero, color = lab), size = line.width) +
    geom_text(x = x.lab.b,
              y = 0.03 + calc_y(x.lab.b, econ_perc, 'benefit'),
              angle = angle(econ_perc, 'benefit'),
              size = text.size,
              hjust = 0, vjust = 0,
              label = "MB[society]", parse=TRUE) +
    geom_text(x = x.lab.b,
              y = -0.015 + calc_y(x.lab.b, econ_perc, 'benefit.perc'),
              angle = angle(econ_perc, 'benefit.perc'),
              size = text.size,
              hjust = 0, vjust = 1,
              label = "MB[perceived]", parse=TRUE) +
    geom_text(x = x.lab.c,
              y = 0.01 + calc_y(x.lab.c, econ_perc, 'cost'),
              angle = angle(econ_perc, 'cost'),
              size = text.size,
              hjust = 1, vjust = 0,
              label = "MC[society]", parse=TRUE) +
    geom_segment(aes(x = x, y = y, yend = I(0), xend = x), data = equilibrium,
                 color = "black", linetype = 'dashed', size = line.width) +
    geom_segment(aes(x = x, y = y, yend = I(0), xend = x), data = e.perc,
                 color = "black", linetype = 'dashed', size = line.width) +
    fill_scale +
    scale_alpha_manual(values = c(benefit = 1, cost = 1, benefit.perc = 0)) +
    theme(axis.title.x = element_text(vjust = xvjust),
          plot.margin = grid::unit(c(1,1, 0.5 + bm.delta, 0.5 + rm.delta), 'lines'))
}

fig.7.a <- function() {
  econ_perc <- econ_perc.2
  e.perc <- e.perc.2
  x.perc <- x.perc.2
  ribbon.perc <- ribbon.perc.2
  ribbon.dw <- ribbon.dw.2
  a.coord <- a.coord.2
  x.lab.b <- 0.25 * equilibrium$x
  x.lab.c <- 0.99 * xmax

  p0() %>% update_scale('x', breaks = c(equilibrium$x, e.perc$x), labels = c('E*','E\'')) +
    geom_ribbon(data = ribbon.dw,
                aes(x = x, ymin = ymin, ymax = ymax,
                    color = NA, fill = 'benefit.perc', alpha = 'benefit.perc')) +
    geom_abline(data = econ_perc,
                aes(slope = slope, intercept = zero, color = lab), size = line.width) +
    geom_text(x = x.lab.b,
              y = 0.03 + calc_y(x.lab.b, econ_perc, 'benefit'),
              angle = angle(econ_perc, 'benefit'),
              size = text.size,
              hjust = 0, vjust = 0,
              label = "MB[society]", parse=TRUE) +
    geom_text(x = x.lab.b,
              y = -0.015 + calc_y(x.lab.b, econ_perc, 'benefit.perc'),
              angle = angle(econ_perc, 'benefit.perc'),
              size = text.size,
              hjust = 0, vjust = 1,
              label = "MB[perceived]", parse=TRUE) +
    geom_text(x = x.lab.c,
              y = 0.01 + calc_y(x.lab.c, econ_perc, 'cost'),
              angle = angle(econ_perc, 'cost'),
              size = text.size,
              hjust = 1, vjust = 0,
              label = "MC[society]", parse=TRUE) +
    geom_segment(aes(x = x, y = y, yend = I(0), xend = x, color = 'benefit'),
                 data = equilibrium, linetype = 'dashed', size = line.width) +
    geom_segment(aes(x = x, yend = I(0), xend = x, y = calc_y(x, econ, 'benefit'),
                     color = 'benefit.perc'),
                 data = e.perc, linetype = 'dashed', size = line.width) +
    fill_scale +
    annotate('text', x = equilibrium$x, y = 2 * equilibrium$y,
             label = 'Deadweight\nLoss', size = text.size, vjust = 0) +
    annotate('segment', x = equilibrium$x, y = 1.95 * equilibrium$y,
             xend = a.coord$x, yend = a.coord$y,
             arrow = grid::arrow(ends = 'last', type = 'closed',
                                 length = grid::unit(0.02, 'npc')),
             color = 'black') +
    scale_alpha_manual(values = c(benefit = 1, cost = 1, benefit.perc = 1)) +
    theme(axis.title.x = element_text(vjust = xvjust, hjust = 0),
          plot.margin = grid::unit(c(1,1, 0.5 + bm.delta, 0.5 + rm.delta), 'lines'))
}

fig.7.b <- function() {
  econ_perc <- econ_perc.1
  e.perc <- e.perc.1
  x.perc <- x.perc.1
  ribbon.perc <- ribbon.perc.1
  ribbon.dw <- ribbon.dw.1
  a.coord <- a.coord.1
  x.lab.b <- 0.25 * equilibrium$x
  x.lab.c <- 0.99 * xmax

  p0() %>% update_scale('x', breaks = c(equilibrium$x, e.perc$x), labels = c('E*','E\'')) +
    geom_ribbon(data = ribbon.dw,
                aes(x = x, ymin = ymin, ymax = ymax,
                    color = NA, fill = 'benefit.perc', alpha = 'benefit.perc')) +
    geom_abline(data = econ_perc,
                aes(slope = slope, intercept = zero, color = lab), size = line.width) +
    geom_text(x = x.lab.b,
              y = -0.015 + calc_y(x.lab.b, econ_perc, 'benefit'),
              angle = angle(econ_perc, 'benefit'),
              size = text.size,
              hjust = 0, vjust = 1,
              label = "MB[society]", parse=TRUE) +
    geom_text(x = x.lab.b * 2.3,
              y = -0.015 + calc_y(x.lab.b * 2.3, econ_perc, 'benefit.perc'),
              angle = angle(econ_perc, 'benefit.perc'),
              size = text.size,
              hjust = 0, vjust = 1,
              label = "MB[perceived]", parse=TRUE) +
    geom_text(x = x.lab.c,
              y = 0.01 + calc_y(x.lab.c, econ_perc, 'cost'),
              angle = angle(econ_perc, 'cost'),
              size = text.size,
              hjust = 1, vjust = 0,
              label = "MC[society]", parse=TRUE) +
    geom_segment(aes(x = x, y = y, yend = I(0), xend = x, color = 'benefit'),
                 data = equilibrium, linetype = 'dashed', size = line.width) +
    geom_segment(aes(x = x, yend = I(0), xend = x, y = calc_y(x, econ, 'benefit'),
                     color = 'benefit.perc'),
                 data = e.perc, linetype = 'dashed', size = line.width) +
    annotate('text', x = equilibrium$x, y = 2 * equilibrium$y,
             label = 'Deadweight\nLoss', size = text.size, vjust = 0) +
    annotate('segment', x = equilibrium$x, y = 1.95 * equilibrium$y,
             xend = a.coord$x, yend = a.coord$y,
             arrow = grid::arrow(ends = 'last', type = 'closed',
                                 length = grid::unit(0.02, 'npc')),
             color = 'black') +
    fill_scale +
    scale_alpha_manual(values = c(benefit = 1, cost = 1, benefit.perc = 1)) +
    theme(axis.title.x = element_text(vjust = xvjust, hjust = 0),
          plot.margin = grid::unit(c(1,1, 0.5 + bm.delta, 0.5 + rm.delta), 'lines'))
}

econ_perc.3 <- econ_perc.2
e.perc.3 <- e.perc.2
x.perc.3 <- x.perc.2
e.perc.dw.3 <- e.perc.3 %>%
  mutate(x = (y - econ['benefit','zero']) / econ['benefit','slope'])

ribbon.dw.3 <- gen_ribbon(econ, xrange = c(e.perc.dw.3$x, equilibrium$x), zero = FALSE)
a.coord.3 <- expand.grid(x = c(e.perc.dw.3$x, equilibrium$x), lab = c('cost','benefit'),
                         stringsAsFactors = F) %>%
  group_by(lab) %>% mutate(y = calc_y(x, econ_perc.3, lab)) %>% ungroup() %>%
  summarize(x = mean(x) + 0.01, y = mean(y) - 0.01)




fig.7.c <- function() {
  econ_perc <- econ_perc.3
  e.perc <- e.perc.3
  e.perc.dw <- e.perc.dw.3
  x.perc <- x.perc.3
  ribbon.perc <- NULL
  ribbon.dw <- ribbon.dw.3
  a.coord <- a.coord.3
  x.lab.b <- 0.25 * equilibrium$x
  x.lab.c <- 0.99 * xmax

  p0() %>% update_scale('x', breaks = c(equilibrium$x, e.perc.dw$x), labels = c('E*','E\'')) %>%
    update_scale('y', breaks = c(equilibrium$y, e.perc$y), labels = c('P*','P\'')) +
    geom_ribbon(data = ribbon.dw,
                aes(x = x, ymin = ymin, ymax = ymax,
                    color = NA, fill = 'benefit.perc', alpha = 'benefit.perc')) +
    geom_abline(data = econ_perc,
                aes(slope = slope, intercept = zero, color = lab), size = line.width) +
    geom_text(x = x.lab.b,
              y = 0.01 + calc_y(x.lab.b, econ_perc, 'benefit'),
              angle = angle(econ_perc, 'benefit'),
              size = text.size,
              hjust = 0, vjust = 0,
              label = "MB[society]", parse=TRUE) +
    geom_text(x = x.lab.b,
              y = -0.015 + calc_y(x.lab.b, econ_perc, 'benefit.perc'),
              angle = angle(econ_perc, 'benefit.perc'),
              size = text.size,
              hjust = 0, vjust = 1,
              label = "MB[perceived]", parse=TRUE) +
    geom_text(x = x.lab.c,
              y = 0.01 + calc_y(x.lab.c, econ_perc, 'cost'),
              angle = angle(econ_perc, 'cost'),
              size = text.size,
              hjust = 1, vjust = 0,
              label = "MC[society]", parse=TRUE) +
    geom_segment(aes(x = x, y = y, yend = I(0), xend = x, color = 'benefit'),
                 data = equilibrium, linetype = 'dashed', size = line.width) +
    geom_segment(aes(x = x, y = y, yend = I(0), xend = x, color = 'benefit.perc'),
                 data = e.perc.dw, linetype = 'dashed', size = line.width) +
    geom_segment(aes(x = x, y = y, xend = I(0), yend = y, color = 'benefit'),
                 data = equilibrium, linetype = 'dashed', size = line.width) +
    geom_segment(aes(x = x, y = y, xend = I(0), yend = y, color = 'benefit.perc'),
                 data = e.perc.dw, linetype = 'dashed', size = line.width) +
    geom_point(data = e.perc.3, aes(x = x, y = y, color = 'benefit.perc'), size = point.size) +
    fill_scale +
    scale_alpha_manual(values = c(benefit = 1, cost = 1, benefit.perc = 1)) +
    annotate('text', x = equilibrium$x, y = 2 * equilibrium$y,
             label = 'Deadweight\nLoss', size = text.size, vjust = 0) +
    annotate('segment', x = equilibrium$x, y = 1.95 * equilibrium$y,
             xend = a.coord$x, yend = a.coord$y + 0.04,
             arrow = grid::arrow(ends = 'last', type = 'closed',
                                 length = grid::unit(0.02, 'npc')),
             color = 'black') +
    scale_alpha_manual(values = c(benefit = 1, cost = 1, benefit.perc = 1)) +
    theme(axis.title.x = element_text(vjust = xvjust, hjust = 0),
          plot.margin = grid::unit(c(1,1, 0.5 + bm.delta, 0.5 + rm.delta), 'lines'))
}


simple_deadweight <- function() {
  econ_perc <- econ_perc.2
  e.perc <- e.perc.2
  x.perc <- x.perc.2
  ribbon.perc <- ribbon.perc.2
  ribbon.dw <- ribbon.dw.2
  a.coord <- a.coord.2
  x.lab.b <- 0.25 * equilibrium$x
  x.lab.c <- 0.99 * xmax

  p0() %>% update_scale('x', breaks = c(equilibrium$x, e.perc$x), labels = c('E*','E\'')) +
    geom_ribbon(data = ribbon.dw,
                aes(x = x, ymin = ymin, ymax = ymax,
                    color = NA, fill = 'benefit.perc', alpha = 'benefit.perc')) +
    geom_abline(data = filter(econ_perc, lab %in% c('benefit','cost')),
                aes(slope = slope, intercept = zero, color = lab), size = line.width) +
    geom_text(x = x.lab.b,
              y = 0.01 + calc_y(x.lab.b, econ_perc, 'benefit'),
              angle = angle(econ_perc, 'benefit'),
              size = text.size,
              hjust = 0, vjust = 0,
              label = "MB[society]", parse=TRUE) +
    geom_text(x = x.lab.c,
              y = 0.01 + calc_y(x.lab.c, econ_perc, 'cost'),
              angle = angle(econ_perc, 'cost'),
              size = text.size,
              hjust = 1, vjust = 0,
              label = "MC[society]", parse=TRUE) +
    geom_segment(aes(x = x, y = y, yend = I(0), xend = x, color = 'benefit'),
                 data = equilibrium, linetype = 'dashed', size = line.width) +
    geom_segment(aes(x = x, y = y, yend = I(0), xend = x, y = calc_y(x, econ, 'benefit'),
                     color = 'benefit.perc'),
                 data = e.perc, linetype = 'dashed', size = line.width) +
    fill_scale +
    annotate('text', x = equilibrium$x, y = 2 * equilibrium$y,
             label = 'Deadweight\nLoss', size = text.size, vjust = 0) +
    annotate('segment', x = equilibrium$x, y = 1.95 * equilibrium$y,
             xend = a.coord$x, yend = a.coord$y,
             arrow = grid::arrow(ends = 'last', type = 'closed',
                                 length = grid::unit(0.02, 'npc')),
             fill = 'black', color = 'black') +
    scale_alpha_manual(values = c(benefit = 1, cost = 1, benefit.perc = 1)) +
    theme(axis.title.x = element_text(vjust = xvjust, hjust = 0),
          plot.margin = grid::unit(c(1,1, 0.5 + bm.delta, 0.5 + rm.delta), 'lines'))
}

simple_deadweight.2 <- function() {
  econ_perc <- econ_perc.1
  e.perc <- e.perc.1
  x.perc <- x.perc.1
  ribbon.perc <- ribbon.perc.1
  ribbon.dw <- ribbon.dw.1
  a.coord <- a.coord.1
  x.lab.b <- 0.25 * equilibrium$x
  x.lab.c <- 0.99 * xmax

  p0() %>% update_scale('x', breaks = c(equilibrium$x, e.perc$x), labels = c('E*','E\'')) +
    geom_ribbon(data = ribbon.dw,
                aes(x = x, ymin = ymin, ymax = ymax,
                    color = NA, fill = 'benefit.perc', alpha = 'benefit.perc')) +
    geom_abline(data = filter(econ_perc, lab %in% c('benefit','cost')),
                aes(slope = slope, intercept = zero, color = lab), size = line.width) +
    geom_text(x = x.lab.b,
              y = -0.015 + calc_y(x.lab.b, econ_perc, 'benefit'),
              angle = angle(econ_perc, 'benefit'),
              size = text.size,
              hjust = 0, vjust = 1,
              label = "MB[society]", parse=TRUE) +
    geom_text(x = x.lab.c,
              y = 0.01 + calc_y(x.lab.c, econ_perc, 'cost'),
              angle = angle(econ_perc, 'cost'),
              size = text.size,
              hjust = 1, vjust = 0,
              label = "MC[society]", parse=TRUE) +
    geom_segment(aes(x = x, y = y, yend = I(0), xend = x, color = 'benefit'),
                 data = equilibrium, linetype = 'dashed', size = line.width) +
    geom_segment(aes(x = x, y = y, yend = I(0), xend = x, y = calc_y(x, econ, 'benefit'),
                     color = 'benefit.perc'),
                 data = e.perc, linetype = 'dashed', size = line.width) +
    annotate('text', x = equilibrium$x, y = 2 * equilibrium$y,
             label = 'Deadweight\nLoss', size = text.size, vjust = 0) +
    annotate('segment', x = equilibrium$x, y = 1.95 * equilibrium$y,
             xend = a.coord$x, yend = a.coord$y,
             arrow = grid::arrow(ends = 'last', type = 'closed',
                                 length = grid::unit(0.02, 'npc')),
             fill = 'black', color = 'black') +
    fill_scale +
    scale_alpha_manual(values = c(benefit = 1, cost = 1, benefit.perc = 1)) +
    theme(axis.title.x = element_text(vjust = xvjust, hjust = 0),
          plot.margin = grid::unit(c(1,1, 0.5 + bm.delta, 0.5 + rm.delta), 'lines'))
}
