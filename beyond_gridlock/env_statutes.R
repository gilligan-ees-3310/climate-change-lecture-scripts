options(tidyverse.quiet = TRUE)
library(tidyverse)

epa_statute_plot <- function() {
  statutes <- data.frame( year = NA, statute = NA, included = NA)
  
  statutes <- rbind(statutes,
                    c(year = 1970, statute = "Clean Air Act", included = TRUE),
                    c(year = 1970, statute = "National Environmental Policy Act", included = TRUE),
                    c(year = 1972, statute = "Federal Water Pollution Control Act", included = TRUE),
                    c(year = 1972, statute = "Coastal Zone Management Act", included = TRUE),
                    c(year = 1974, statute = "Safe Drinking Water Act", included = TRUE),
                    c(year = 1976, statute = "Resource Conservation and Recovery Act", included = TRUE),
                    c(year = 1976, statute = "Toxic Substances Control Act", included = TRUE),
                    c(year = 1977, statute = "Clean Air Act Amendments", included = TRUE),
                    c(year = 1977, statute = "Clean Water Act", included = TRUE),
                    c(year = 1980, statute = "Comprehensive Environmental Response, Compensation, and Liability Act", included = TRUE),
                    c(year = 1984, statute = "Hazardous and Solid Waste Amendments", included = TRUE),
                    c(year = 1986, statute = "Emergency Planning and Community Right-to-Know Act", included = TRUE),
                    c(year = 1986, statute = "Superfund Amendments and Reauthorization Act", included = TRUE),
                    c(year = 1990, statute = "Oil Pollution Act", included = TRUE),
                    c(year = 1990, statute = "Clean Air Act Amendments", included = TRUE),
                    
                    c(year = 1986, statute = "Safe Drinking Water Act Amendments", included = FALSE),
                    c(year = 1987, statute = "Clean Water Act Amendments", included = FALSE),
                    c(year = 1988, statute = "Federal Insecticide, Fungicide, and Rodenticide Act Amendments", included = FALSE),
                    c(year = 1996, statute = "Safe Drinking Water Act Amendments", included = FALSE),
                    c(year = 1996, statute = "Food Quality Protection Act", included = FALSE),
                    c(year = 2002, statute = "Comprehensive Environmental Response, Compensation, and Liablity Act Amendments", included = FALSE),
                    c(year = 2016, statute = "Toxic Substances Control Act Amendments", included = TRUE)
  )
  
  p <- ggplot(statutes %>% filter(included == TRUE), aes(x = as.numeric(year))) +
    geom_bar() +
    scale_x_continuous(breaks = seq(1970,2025,5), limits=c(1969,2018), expan=c(0,0)) +
    scale_y_continuous(breaks = 0:2, expand=c(0,0)) +
    labs(x = "Year", y = NULL) + #, title = "Major Pollution Control Statutes: 1970-2014") +
    theme_classic(base_size = 25) +
    theme(line = element_line(size=1))
  p
}
