if (! exists('data.dir')) {
  source('semester_config.R')
}

load_dice_data <- function() {
  data <- read.csv(file.path(data.dir, 'dice', 'dice2010.csv'),header=T, blank.lines.skip=T)
  data <- data %>% filter(! is.na(Growth))
  data <- data %>% gather(key = 'year', value = 'value', -Var, -Growth, convert=TRUE)
  data <- data %>% filter(!is.na(value))
  data <- data %>% mutate(year = as.numeric(substring(as.character(year),2)))
  x <- data.frame( year =  c(2100,2200), Growth = c(TRUE, FALSE), Var = c('Temp', 'Consumption'),
                   stringsAsFactors = FALSE)
  x <- x %>% expand(year, Growth, Var)
  
  data_mean <- function(data, y, g, v, fuzz = 6) {
    (data %>% filter(Var == v & Growth == g & abs(year - y) <= fuzz) %>% 
       summarize(value = mean(value)))$value
  }
  
  x <- x %>% rowwise() %>% mutate(value = data_mean(data, y = year, g = Growth, v = Var))

  data <- rbind(data, x) %>% arrange(year, Growth, Var)
  data$Growth <- ordered(data$Growth, levels = c(TRUE, FALSE), labels = c('Growth','No growth'))
  temp <- data %>% filter(Var == 'Temp' & year <= 2200) %>% select(Growth, value, year)
  cons <- data %>% filter(Var == 'Consumption' & year <= 2200) %>% select(Growth, value, year) %>% 
    mutate(value = value * 1000)
  invisible(list(data = data, temp = temp, cons=cons))
}

