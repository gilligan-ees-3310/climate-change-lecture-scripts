data {
  int<lower=0> N;
  vector[N] year;
  vector[N] temp;
}

transformed data {
  real mean_year;
  real mean_data;
  real<lower = 0> sd_data;
  real<lower = 0> sd_year;
  
  mean_year <- mean(year);
  sd_year <- sd(year);
  mean_data <- mean(temp);
  sd_data <- sd(temp);
}

parameters {
  real intercept;
  real slope;
  real<lower = 0> sigma;
}


model {
  sigma ~ cauchy(sd_data, sd_data);
  intercept ~ cauchy(mean_data, mean_data);
  slope ~ cauchy(0, 1);
  
  temp ~ normal(year * slope + intercept, sigma);
}
