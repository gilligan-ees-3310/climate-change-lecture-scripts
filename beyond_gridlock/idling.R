#
# Idling
#

n_drivers <- 190E+6 # Nielsen 2007
co2_rate <- 1.4E-6 * 60 # metric tons per minute (1.4 gram/second)
idle_time <- 16 # minutes per person per day

indiv_annual_emissions <- idle_time * 365.25 * co2_rate
total_annual_emissions <- indiv_annual_emissions * n_drivers

