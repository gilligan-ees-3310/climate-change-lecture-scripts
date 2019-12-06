if (!require(pacman)) install.packages('pacman')
library(pacman)

p_load(tidyverse)
p_load(readxl)
p_load(magrittr)
p_load(stringr)
p_load(grid)
p_load(lazyeval)
p_load(broom)

kaya_plot <- function(kaya, countries, v, y_lab = NULL) {
  labels <- c(P =  'Population (billions)',
              G =  'Gross Domestic Product ($ trillion)',
              E =  'Energy consumption (quads)',
              F =  'Fossil-fuel carbon emissions (million metric tons)',
              g =  'Per-capita GDP ($ thousand)',
              e =  'Energy intensity of economy (quads per $trillion)',
              f =  'Carbon intensity of energy supply (MMT per quad)',
              ef = 'Carbon intensity of economy (MMT per $trillion)'
  )

  if (is.null(y_lab)) y_lab <- labels[v]

  countries = as.list(countries)

  .dots = interp(~(country %in% countries & !is.na(v)), v = as.name(v))
  data <- kaya %>% dplyr::filter_(.dots = .dots)

  if (length(countries) > 1) {
    color_scale <- scale_color_brewer(palette = "Dark2")
    legend = guides(color = guide_legend(title = "Country"),
                    shape = guide_legend(title = "Country"))
  } else {
    color_scale = scale_color_manual(values = "dark blue")
    legend = guides(color = FALSE, shape = FALSE)
  }

  p <- ggplot(data, aes_string(x = "year", y = v, shape = "country", color = "country"))
  p + geom_point(size = 3) + geom_line(size = 1) +
    color_scale +
    legend +
    labs(x = "Year", y = y_lab) +
    theme_bw(base_size = 20) +
    theme(axis.title.y = element_text(vjust=1.2),
          axis.title.x = element_text(vjust=-0.1),
          legend.key = element_rect(color = NA))
}


kaya_plot_fit <- function(kaya, country, v, y_lab = NULL, fit_start = 1980) {
  labels <- c(P =  'Population (billions)',
              G =  'Gross Domestic Product ($ trillion)',
              E =  'Energy consumption (quads)',
              F =  'Fossil-fuel carbon emissions (million metric tons)',
              g =  'Per-capita GDP ($ thousand)',
              e =  'Energy intensity of economy (quads per $trillion)',
              f =  'Carbon intensity of energy supply (MMT per quad)',
              ef = 'Carbon intensity of economy (MMT per $trillion)'
  )
  
  if (is.null(y_lab)) y_lab <- labels[v]
  
  .dots = interp(~(country == c & !is.na(v)), c = country, v = as.name(v))
  data <- kaya %>% dplyr::filter_(.dots = .dots)
  data <- bind_rows(
    data %>% filter(year <= fit_start) %>% mutate(status = 'other'),
    data %>% filter(year >= fit_start) %>% mutate(status = 'fit')
  ) %>%
    mutate(status = factor(status, levels = c('fit','other','model')))
  
  model <- lm(substitute(log(v) ~ year, list(v = as.name(v))), 
                         data = data %>% dplyr::filter(status == 'fit'),
              na.action = na.exclude)

  model.df <- model %>% augment() %>% transmute(year, .fitted, status = 'model')
    
  color_scale = scale_color_manual(values = c(other = '#404080', fit = '#4040F0', model = 'black'))
  legend = guides(color = FALSE, shape = FALSE)
  
  p <- ggplot(data, aes_string(x = "year", y = paste("log(", v, ")"), color = "status"))
  p + geom_point(size = 3) + geom_line(size = 1) +
    geom_line(aes(x = year, y = .fitted, color = status), 
              data = model.df, size = 1) +
    color_scale +
    legend +
    labs(x = "Year", y = y_lab) +
    theme_bw(base_size = 20) +
    theme(axis.title.y = element_text(vjust=1.2),
          axis.title.x = element_text(vjust=-0.1),
          legend.key = element_rect(color = NA))
}


implied_decarb_plot <- function(kaya, country, ref_year, reduction, target_year, this_year = NA, fit_start = 1980) {
  kaya <- kaya %>% dplyr::filter_(substitute(country == x, list(x = country)))
  if (is.na(this_year)) this_year = max(kaya$year)
  
  G_model <- lm(log(G) ~ year, data = dplyr::filter(kaya, year >= fit_start))
  ef_model <- lm(log(ef) ~ year, data = dplyr::filter(kaya, year >= fit_start))
  r_ef <- tidy(ef_model, quick=T) %>% dplyr::filter(term == 'year') %>% dplyr::select(estimate) %>% simplify()
  r_G <- tidy(G_model, quick=T) %>% dplyr::filter(term == 'year') %>% dplyr::select(estimate) %>% simplify()
  
  target_F <- kaya %>% dplyr::filter(year == ref_year) %>% dplyr::select(F) %>% simplify() * (1 - reduction)
  current_F <- kaya %>% dplyr::filter(year == this_year) %>% dplyr::select(F) %>% simplify()
  current_ef <- kaya %>% dplyr::filter(year == this_year) %>% dplyr::select(ef) %>% simplify()
  
  implied_r_F <- log(target_F / current_F) / (target_year - this_year)
  implied_r_ef <- implied_r_F - r_G
  
  df <- kaya %>% dplyr::select(year, ef) %>%
    mutate(category = 'Historical Data') %>%
    bind_rows(
      ef_model %>% 
        augment(newdata = data_frame(year = seq(fit_start, target_year))) %>%
        mutate(ef = exp(.fitted), category = 'Historical Trend') %>% 
        dplyr::select(year, ef, category),
      data_frame(year = seq(this_year, target_year)) %>%
        mutate(ef = current_ef * exp(implied_r_ef * (year - this_year)),
               category = 'Implied Decarbonization')
    ) %>%
    mutate(category = ordered(category, levels = c('Historical Trend',
                                                   'Historical Data', 
                                                   'Implied Decarbonization')))
  
  p <- ggplot(df, aes(x = year, y = ef, color = category)) +
    geom_line(size = 1) + geom_point(size = 1.5) +
    labs(x = "Year", y = expression(paste(CO[2], " intensity  ", ( tonnes / "$1000 GDP" )))) +
    scale_color_brewer(palette = 'Dark2', name = NULL, direction = -1,
                       breaks = c('Historical Data', 'Historical Trend', 'Implied Decarbonization')) +
    theme(legend.position = c(1,1), legend.justification = c(1,1), legend.key.width = unit(2, 'lines'))
  p 
}

