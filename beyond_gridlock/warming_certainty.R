library(ggplot2)
library(dplyr)
library(tidyr)

df <- data.frame(year = c(1995, 2001, 2007, 2013), p_lower = c(50, 66, 90, 95),
                 p_upper = c(50, 90, 100, 100))

# Gallup Global Warming Trends survey 150325
beliefs <- data.frame(year = c(2001, 2003, 2006, 2007, 2008, 2010, 2011, 2012, 2013, 2014, 2015, 2016),
                      Public =   c(61, 61, 58, 61, 58, 50, 52, 53, 57, 57, 55, 65),
                      Democrats= c(72, 68, 64, 74, 73, 65, 71, 65, 78, 79, 74, 85),
                      Republicans = c(52, 52, 44, 44, 42, 35, 36, 36, 39, 41, 34, 38))
# beliefs <- beliefs %>% filter(year <= 2013)

p <- ggplot(df, aes(x = year, y = p_lower)) +
  geom_ribbon(aes(ymin = p_lower, ymax = p_upper),
              alpha = 0.3, fill = "dark gray") +
  geom_line(size = 1, color = "black") +
  geom_point(size = 5, color = "black") +
  annotate(geom = "text", x = 1990, y = 40, label = "?\n(no attribution)",
           hjust = 0, vjust = 1, size = 8) +
  annotate(geom = "text", x = 1995, y = 47, label = "50%+\n(balance of evidence)",
           hjust = 0, vjust = 1, size = 8) +
  annotate(geom = "text", x = 2001, y = 62, label = "66-90%\n(likely)",
           hjust = 0, vjust = 1, size = 8) +
  annotate(geom = "text", x = 2007, y = 87, label = ">90%\n(very likely)",
           hjust = 0, vjust = 1, size = 8) +
  annotate(geom = "text", x = 2013, y = 92, label = "95-100%\n(extremely\nlikely)",
           hjust = 0, vjust = 1, size = 8) +
  scale_x_continuous(breaks = seq(1990,2015, 5), limits = c(1990,2015.5)) +
  scale_y_continuous(breaks = seq(0,100,25), labels = paste0(seq(0,100,25), "%"),
                     limits = c(0,100), expand = c(0,0)) +
  labs(x = "Year", y = NULL,
       title = "IPCC: Likelihood that Observed Climate Change\nIs Mostly Anthropogenic") +
  theme_bw(base_size = 30)

print(p)
png('Fig_2_1.png', width = 1024, height = 600)
print(p)
dev.off()


partisan <- beliefs %>% gather(key = party, value = belief, -year) %>%
  bind_rows(df %>% select(year, belief = p_lower) %>% mutate(party = "Scientists")) %>%
  mutate(party = factor(party)) #  %>%
#  filter(party != 'Public')

p <- ggplot(df, aes(x = year, y = p_lower)) +
  geom_ribbon(aes(ymin = p_lower, ymax = p_upper),
              alpha = 0.3, fill = "dark green") +
  geom_line(aes(x = year, y = belief, color = party, linetype = party, size = party),
            data = partisan) +
  geom_point(aes(x = year, y = belief, shape = party, color = party),
             data = partisan,
             size = 5) +
  annotate(geom = "text", x = 1990, y = 40, label = "?\n(observed\nwaming)",
           hjust = 0, vjust = 1, size = 8) +
  annotate(geom = "text", x = 1994.9, y = 55, label = "50%+ (balance\nof evidence)",
           hjust = 1, vjust = 0, size = 8, lineheight = 0.8) +
  annotate(geom = "text", x = 2000.5, y = 65, label = "66-90%\n(likely)",
           hjust = 1, vjust = 0, size = 8, lineheight = 0.8) +
  annotate(geom = "text", x = 2006.5, y = 99, label = ">90%\n(very likely)",
           hjust = 1, vjust = 1, size = 8, lineheight = 0.8) +
  annotate(geom = "text", x = 2012, y = 92, label = "95-100%\n(extremely likely)",
           hjust = 0.5, vjust = 1, size = 8, lineheight = 0.8) +
  scale_linetype_manual(breaks = c('Scientists', 'Public', 'Democrats', 'Republicans'),
                        values = c(Scientists = 'solid', Democrats = 'dashed', Republicans = 'dashed', Public = 'dashed'),
                        name = NULL)+
  scale_size_manual(breaks = c('Scientists', 'Public', 'Democrats', 'Republicans'),
                        values = c(Scientists = 1, Democrats = 0.5, Republicans = 0.5, Public = 0.5),
                        name = NULL)+
  scale_shape_manual(breaks = c('Scientists', 'Public', 'Democrats', 'Republicans'),
                     values = c(Scientists = 16, Democrats = 17, Republicans = 15, Public = 1),
                     name = NULL)+
  scale_color_manual(breaks = c('Scientists', 'Public', 'Democrats', 'Republicans'),
                     values = c(Scientists = "dark green", Democrats = "dark blue", Republicans = "dark red", Public = "purple"),
                     name = NULL) +
  scale_x_continuous(breaks = seq(1990,2016, 5), limits = c(1990,2016.5)) +
  scale_y_continuous(breaks = seq(0,100,25), labels = paste0(seq(0,100,25), "%"),
                     limits = c(0,100), expand = c(0,0)) +
  labs(x = "Year", y = NULL,
       title = "Beliefs about Whether Global Temperature Change\nIs Mostly Anthropogenic") +
  theme_bw(base_size = 30) +
  theme(legend.position = c(1,0), legend.justification = c(1,0),
        legend.key.width = unit(3, "line"))

print(p)
png('Fig_7_1.png', width = 1024, height = 600)
print(p)
dev.off()

