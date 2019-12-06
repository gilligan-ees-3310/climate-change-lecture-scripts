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
  
  vector[N] x;
  vector[N] y;
  
  mean_year <- mean(year);
  sd_year <- sd(year);
  mean_data <- mean(temp);
  sd_data <- sd(temp);
  
  x <- (year - mean_year) / sd_year ;
  y <- (temp - mean_data) / sd_data;
}

parameters {
  real pseudo_intercept;
  real pseudo_slope;
  real<lower = 0> pseudo_sigma;
}

transformed parameters {
  real intercept;
  real slope;
  real sigma;
  
  slope <- pseudo_slope * sd_data / sd_year;
  intercept <- pseudo_intercept * sd_data - mean_year * slope + mean_data;
  sigma <- pseudo_sigma * mean_data;
}


model {
  pseudo_sigma ~ cauchy(1, 1);
  pseudo_intercept ~ cauchy(0, 1);
  pseudo_slope ~ cauchy(0, 1);
//  y ~ normal(x * pseudo_slope + pseudo_intercept, pseudo_sigma);
  
  temp ~ normal(year * slope + intercept, sigma);
}
