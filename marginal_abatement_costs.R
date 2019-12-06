library(tidyverse)
library(grid)
library(gtable)
library(gridExtra)

mac <- data.frame(x = seq(0, 1, 0.01)) %>% 
  mutate(today = 0.05 + 0.5 * x + 2 * x^2, future = 0.05 + 0.3 * x + 0.6 * x^2) %>% 
  gather(key = time, value = mac, -x)

pal <- brewer.pal(3, 'Dark2')

p1 <- ggplot(data.frame(x = c(0,1), y = c(0.2,0.8)), aes(x = x, y = y)) + 
  geom_line(size = 2, color = '#A00000') +
  scale_x_continuous(limits = c(0,1), breaks = NULL, name = "Time") +
  scale_y_continuous(limits = c(0,1), breaks = NULL, name = "Social cost of carbon") +
  theme(axis.title.y = element_text(angle = 90, vjust = 1, hjust = 1),
        axis.title.x = element_text(hjust = 1),
#        plot.margin = unit(c(0.05, 0, 0, -0.2), 'npc'),
        legend.position = "none")

p2 <- ggplot(mac, aes(x = x, y = mac, color = time)) + 
  geom_line(size = 2) + 
  annotate('text', x = 0.55 - 0.05,
           y = filter(mac, x == 0.55 & time == 'today')$mac, 
           label = 'Today', hjust = 1, size = 10, color = pal[1]) +
  scale_color_brewer(breaks = c('today', 'future'), palette ="Dark2") + 
  scale_y_continuous(limits = c(0,1), expand = c(0,0), breaks = NULL, name = "Marginal abatement costs") + 
  scale_x_continuous(limits = c(0,1), breaks = NULL, name = "Emissions reduction") + 
  theme(axis.title.y = element_text(angle = 90, vjust = 1, hjust = 1), 
        axis.title.x = element_text(hjust = 1),
#        plot.margin = unit(c(0.05,0,0,-0.23), "npc"),
        legend.position = "none")

grid.arrange(p1, p2, ncol = 2, padding = unit(0, 'npc'))

