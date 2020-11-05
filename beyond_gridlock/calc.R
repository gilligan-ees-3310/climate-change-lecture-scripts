#
# Calculations
#
library(rvest)

m_c <- 12
m_co2 <- (12 + 2 * 16)

anthro_c_emissions <- 10 # GT/year
natural_c_emissions <- 200 # GT/year

c_to_co2 <- function(x) {
  x * m_co2 / m_c
}


co2_to_c <- function(x) {
  x * m_c / m_co2
}

cat("Human CO2 emissions = ", c_to_co2(anthro_c_emissions), " = ",
    round(100 * anthro_c_emissions / (anthro_c_emissions + natural_c_emissions)),
    '%.\n', sep='')

url <- 'http://www.epa.gov/ozone/science/indicat/techsupp.html'
url <- 'documents/CFC_emissions/Environmental Indicators  Ozone Depletion _ Ozone Layer Protection _ US EPA.htm'
html <- read_html(url)
x <- html_nodes(html, 'table')
# emissions <- html_table(x[[3]], fill=TRUE) # if using original NASA URL
emissions <- html_table(x[[6]], fill=TRUE) # if using archived version on Wayback machine
names(emissions) <- c('Year', 'US', 'Rest.of.world', 'Total')
for( i in 2:ncol(emissions)) {
  emissions[,i] <- as.numeric(gsub(',','', emissions[,i]))
}
delta <- emissions[-1,]
delta[,-1] <- delta[,-1] - emissions[-nrow(emissions),-1]

cat("US CFC emissions drop from 1974-1975 = ",
    delta$US[delta$Year == 1975] - delta$US[delta$Year == 1974],
    " thousand metric tons, or ",
    round(100 * (1.0 - (delta$US[delta$Year == 1975]/delta$US[delta$Year == 1974])),1),
    "%\n", sep='')
