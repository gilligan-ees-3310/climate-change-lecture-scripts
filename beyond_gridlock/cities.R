cities <- data.frame(co2 = c(1.2,0.5,0.9,0.2,0.5,0.2,0.7,0.2,0.5,1.0,0.9,0.2,0.3,0.3,0.3),
npv = c(2.1,-0.3,3.7,0.2,-2.1,-0.7,3.0,0.0,2.9,1.4,3.8,0.4,2.2,0.0,0.0))
print(colSums(cities %>% filter(npv >= 0)))
print(colSums(cities %>% filter(npv > 0)))
