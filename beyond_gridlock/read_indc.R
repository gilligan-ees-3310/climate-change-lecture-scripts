library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)

data <- read.csv('data/PBL_INDCs.csv', header = F, as.is=T,
                 na.strings = c('',' ','-','NA'),
                 colClasses="character", fill=TRUE, strip.white = TRUE)

n <-  data %>% head(1) %>% unlist %>% unname %>% str_replace_all("^\\.|\\.$","")

heads <- data %>% head(4) %>% select(-(1:2))
heads <- heads[,order(n[-(1:2)])]

emissions <- data %>% tail(-4)
names(emissions) <-  n

emissions <- emissions %>%
  select(-lulucf.co2) %>%
  filter(! is.na(country.name)) %>%
  gather(key = col, value = val, -country.name)

emissions <- emissions %>%
  spread(key = country.name, value = val)

emissions$level <- slice(heads,4) %>% unlist %>% ifelse(is.na(.), "Median", .)
emissions$scenario <- slice(heads,2) %>% unlist
emissions$year <- slice(heads,3) %>% unlist %>% as.numeric

emissions <- emissions %>%
  gather(key = country.name, value = val, -col, -level, -scenario, -year) %>%
  select(-col) %>% filter(! is.na(val)) %>%
  mutate(val = (val %>% str_replace_all(",","") %>% as.numeric()))

emissions <- emissions %>%
  spread(key = level, value = val)

ge <- emissions %>% filter(str_detect(country.name, "^World"))

