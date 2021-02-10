library(tidyverse)

theme_set(theme_bw())

p_eq <- function(t) {
  # T in Kelvin
  tc = t - 273.15
  c1 = 0.61121
  c2 = 18.678
  c3 = 234.5
  c4 = 257.14
  p  = c1 * exp((c2 - tc / c3) * (tc / (c4 + tc)))
  p * 10
}

gh_288 = 15.55
dt_2x = 3.54

p_288 <- p_eq(288)

gh <- function(p0, p1) {
  dt_2x * (p1 - p0) / p_288
}

vert <- function(df) {
  x <- tail(df, 1)
  df <- df %>% bind_rows(mutate(x, t = t1, p = p_eq(x$t), p1 = p_eq(t1),
                                dir = "V"))
  invisible(df)
}

hor <- function(df) {
  x <- tail(df, 1)
  df <- df %>% bind_rows(mutate(x, t = t1, t1 = t + gh(p, p1),
                                p = p1, dir = "H"))
  invisible(df)
}

make_arrows <- function(t0, label, p_min, limit = NA) {
  df <- tibble(t = t0, t1 = t0, p = p_min, p1 = p_eq(t1),
               dir = "V", name = label)

  df <- df %>%
    bind_rows(mutate(df, t1 = t0 + gh_288 + gh(p_288, p1), p = p1,
                     dir = "H")) %>%
    vert() %>%
    hor() %>%
    vert() %>%
    hor()
  if (! is.na(limit)) {
    df <- df %>% filter(t <= limit) %>%
      mutate(t1 = pmin(t1, limit))
  }
  invisible(df)
}

plot_runaway <- function(t_vec, t_lim = 500) {
  f <- function(p, df) {
    p + geom_segment(data = df, aes(x = t, y = p, xend = t1, yend = p1,
                                    color = name),
                     arrow = arrow(length = unit(0.02, "npc"), type = "closed"),
                     size = 1)
  }

  if (! is.na(t_lim)) {
    t_max = t_lim
  } else {
    t_max = 400
  }
  df_eq = tibble(t = seq(200, t_max, 10), p = p_eq(t))

  p_min = 0.003
  p_max = max(4000, df_eq$p)

  p <- ggplot(df_eq, aes(x = t, y = p)) +
    geom_line(color = "darkblue", size = 1) +
    geom_ribbon(aes(ymin = p, ymax = p_max), fill = "darkblue", alpha = 0.2) +
    labs(x = "Temperature (K)", y = "Vapor pressure (mb)") +
    scale_y_log10(limits = c(p_min, p_max), expand = expansion(0,0),
                  labels = scales::label_comma()) +
    scale_x_continuous(expand = expansion(0, 0)) +
    annotate("label", x = 250, y = 100, label = "Ice or Liquid Water",
             color = "darkblue")

  df_list <- map2(t_vec, names(t_vec), make_arrows,
                  p_min = {{ p_min }}, limit = t_lim)
  p <- reduce(df_list, f, .init = p)
  p
}
