options(tidyverse.quiet = TRUE)
library(tidyverse)
library(RColorBrewer)

behavioral_wedge_plot <- function() {
  
  tp.c <- c(25.2, 12.2, 1.4, 6.7, 14.7, 7.4, 56.3, 8.7, 3.0, 8.6, 0.5, 2.9, 9.2, 10.1, 6.0, 24.1, 36.1)
  raer.c <- c(21.2, 10.7, 1.1, 5.4, 11.7, 6.5, 31.4, 3.7, 1.4, 4.1, 0.2, 1.0, 3.2, 4.5, 2.2, 7.7, 6.4)
  
  actions <- c('Weatherization', 'HVAC equipment', 'Low-flow showerheads', 'Efficient water heater', 'Energy Star appliances', 'LRR tires', 'Fuel-efficient vehicle', 'Change HVAC filters', 'Tune-up AC', 'Routine auto maintenance', 'Laundry temperature', 'Water heater temperature', 'Standby electricity', 'Thermostat setbacks', 'Line drying', 'Driving behavior', 'Carpooling & trip-chaining')
  
  
  action.names <- c('weatherization', 'HVAC', 'showerheads', 'efficient.water.heater', 'appliances', 'lrr.tires', 'fuel.efficient.vehicle', 'change.hvac.filters', 'tune.up.ac', 'auto.maintenance', 'laundry.temp', 'water.heater.temp', 'vampire.electricity', 'thermostat', 'line.drying', 'eco.driving', 'carpooling.trip.chaining')
  
  names(tp.c) <- action.names
  names(raer.c) <- action.names
  names(actions) <- action.names
  
  tp <- data_frame(action = actions, c = tp.c, co2 = tp.c * 44/12)
  
  raer <- data_frame(action = actions, c = raer.c, co2 = raer.c * 44/12)
  
  # print(mutate_if(tp, is.numeric, round))
  # print(summarize_at(tp, c("c", "co2"), sum))
  # 
  # print(mutate_if(raer, is.numeric, round))
  # print(summarize_at(raer, c("c", "co2"), sum))
  
  
  p <- bind_rows( mutate(tp, class = "TP"), mutate(raer, class = "RAER")) %>%
    select(-c) %>%
    spread(key = "class", value = "co2") %>% mutate(TP = TP - RAER) %>%
    arrange(desc(RAER)) %>% mutate(action = ordered(action, levels = action)) %>%
    gather(key = class, value = co2, TP, RAER) %>%
    mutate(class = ordered(class, levels = c('TP','RAER'))) %>%
    ggplot(aes(x = action, y = co2, color = class, fill = class)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(values = brewer.pal(3, "Blues")[-2], name = NULL, 
                      labels = c(TP = "Technical Potential", RAER = "Reasonably Achievable")) +
    scale_color_manual(values = brewer.pal(3, "Blues")[-2], name = NULL, 
                       labels = c(TP = "Technical Potential", RAER = "Reasonably Achievable")) +
    # scale_color_manual(values = c(TP = rgb(195/255,214/255,155/255), RAER = "dark blue"), name = NULL) +
    # scale_fill_manual(values = c(TP = rgb(195/255,214/255,155/255), RAER = "dark blue"), name = NULL) +
    scale_y_continuous(limits = c(0,220), breaks = seq(0,250,50), expand = c(0,0)) +
    labs(y = expression(paste("Million Metric Tons ", CO[2])), x = NULL) +
    coord_flip() + theme_bw(base_size = 25) +
    theme(legend.position = c(0.99,0.95), legend.justification = c(1,1),
          panel.grid.major.y = element_blank())
  
  p
}
