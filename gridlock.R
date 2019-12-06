library(pacman)
p_load(tidyverse)
p_load("ggfittext")
p_load("treemapify")

theme_set(theme_bw(base_size = 20))

nations <- data_frame(country=c('USA','Japan','EU','Canada','Australia','Other Enthusiastic',
                                'Brazil', 'China','India','Indonesia','Other Reluctant',
                                'Small Islands', 'Other Vulnerable',
                                'Russia', 'Other Exporters'),
                      group = c(rep_len('Enthusiastic', 6), rep_len('Reluctant',5),
                                rep_len('Vulnerable',2), rep_len('Exporters',2)),
                      emissions = c(17.3, 3.9, 14.2, 2.0, 1.7, 0.6,
                                    5.10001, 20.5, 5.4, 1.7, 5.1,
                                    0.9, 7.3,
                                    6.1, 8.1))

nations <- nations %>%
  mutate(group = ordered(group, levels = c('Enthusiastic', 'Reluctant', 'Exporters', 'Vulnerable')),
         country = ordered(country, levels = country),
         color = NA) %>%
  arrange(group, desc(emissions), country)

ug <- levels(nations$group)
lug <- length(ug)

for (i in 1:lug) {
  g <- ug[i]
#  cat('group = ', as.character(g), ',')
  h <- 1 - ((i - 1) / lug)
#  cat('hue = ', h, '\n')
  nj <- sum(nations$group == g)
  for(j in 1:nj) {
    c <- nations$country[nations$group == g][j]
#    cat("country = ", as.character(c), ',')
    v <- j / nj
#    cat('hue = ', h, ' value = ', v, '\n')
    nations$color[nations$country == c] <- hsv(h, 0.75 - 0.5 * v, v)
  }
}

plot_gridlock <- function(nations) {
  colors <- set_names(nations$color, nations$country)

  breaks <- nations$country[order(nations$group,nations$emissions,nations$country)]

  p <- nations %>% arrange(group, emissions) %>%
    mutate(country = ordered(country, levels = country)) %>%
    ggplot(aes(y=emissions,fill=country,x=group)) +
    geom_bar(stat='identity') +
    geom_bar(stat='identity', color='black',show.legend=FALSE) +
    scale_y_continuous(limits=c(0,41), expand=c(0,0)) +
    labs(x="Group",y="% Emissions") +
    scale_fill_manual(name="Country", values=colors, breaks=breaks) +
    theme(panel.grid.major.x = element_blank(),
          legend.position = c(0.99,0.99), legend.justification = c(1,1))
  p
}

treemap_gridlock <- function(nations) {
  colors <- set_names(nations$color, nations$country)

  text_colors <-
    set_names(ifelse(nations$country %in%
                       c("Australia", "Other Enthusiastic",
                         "Other Reluctant", "Indonesia",
                         "Russia", "Small Islands"),
                     "black",
                     ifelse(nations$group %in% c("Exporters", "Vulnerable"),
                            "white",
                            "#FFFF00")),
              nations$country)

  tm <- treemapify(nations, area = "emissions",
                   subgroup = "group") %>%
    mutate(label = as.character(country) %>%
             ifelse(. == "Indonesia", "Indo- nesia", .))

  ggplot(tm, aes(xmin = xmin, ymin = ymin, xmax = xmax, ymax = ymax)) +
    geom_rect(aes(fill = country)) +
    geom_fit_text(aes(label = label, color = country), reflow = TRUE,
                  place = "centre", size = 30) +
    scale_fill_manual(values = colors, name = "Country") +
    scale_color_manual(values = text_colors, guide = "none") +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    theme(axis.line = element_blank(), axis.text = element_blank(),
          axis.ticks = element_blank(), axis.title = element_blank())
}
