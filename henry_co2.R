#
# Solubility of CO2 in water
# From J.J. Carroll, J.D. Spulsky, and A.E. Mather,
# "The Solubility of Carbon Dioxide in Water at Low Pressure"
# J. Phys. Chem. Ref. Data 20, 1201 (1991)
#


#
# T in Kelvin, k
#
h_cp <- function(t) {
  # ln(H21 / MPa) = -6.8346 + 1.2817E4 / T - 3.7668E6 / T^2 + 2.997E8 / T^3
  # Where T is in Kelvin
  # J. Phys. Chem Ref. Data 20, 1201 (1991), Eq. 4.
  x <- -6.8346 + 1.2817E+4 / t - 3.7668E+6 / t^2 + 2.997E+8 / t^3
  5.506912 * exp(-x) 
}

#
# T in C
# pCO2 in atm
#
sol <- function(pCO2,t) {
  # x_2 * H_21 = y_2 phi_2 P
  # Where 
  # x_2 = mole fraction CO2 in solution
  # H_21 = Henry's Law constant for temperature T
  # y_2 * P is partial pressure of CO2 in MPa
  # phi_2 = fugacity coefficient for CO2 (assumed to be unity)
  # 
  # Returns dissolved CO2 in grams per liter
  # 
  pCO2 * h_cp(t + 273.15)
}

#
# Calculate partitioning of dissolved oceanic carbon between 
# CO2/H2CO3, HCO3, and CO3.
# Also calculates equilibrium vapor pressure of dissolved CO2.
# 
# Taken from geocarb_web.F, from David Archer.
#
#
# t_atm = atmospheric temperature in Celsius
# alk = ocean alkalinity, in moles per cubic meter, as reported by GEOCARB
# tco2 = total mineral carbon in the ocean, in moles per cubic meter, as reported by GEOCARB
# sal = ocean salinity in parts per thousand
# 
# returns a vector of dissolved co2, hco3 and co3, in millimoles per cubic meter
# (the units GEOCARB uses), and the saturation vapor pressure of CO2 based on the 
# dissolved CO2, in parts per million.
partition <- function(t_atm, alk, tco2, sal = 35) {
  ocn_vol = 1.2E+6 # 10^12 cubic meters
  kb = 1.e-9
  t_si = 0.
  t_p = 0.
  t_bor = 4.106E-4 * sal / 35.
  t_kt = t_atm +  273.15
  t_k = t_kt / 100.
  
  alk_conc = 1.0E-3 * alk   # moles per liter
  tco2_conc = 1.0E-3 * tco2 # moles per liter
  
  k1 = 13.7201 - 0.031334 * t_kt - 3235.76 / t_kt - 1.3E-5 * sal * t_kt + 0.1032 * sqrt(sal)
  
  k1 = 10^k1
  
  k2 = - 5371.9645 - 1.671221 * t_kt + 128375.28 / t_kt + 
    2194.3055 * log10( t_kt ) - 0.22913 * sal - 18.3802 * log10( sal ) +
    8.0944E-4 * sal * t_kt + 5617.11 * log10( sal ) / t_kt - 2.136 * sal / t_kt
  
  k2 = 10^k2

  k_si = 1.E-10
  k_p2 = exp( -9.039 - 1450. / t_kt)
  k_p3 = exp(  4.466 - 7276. / t_kt)
  k_w  = exp( 148.9802 - 13847.26 / t_kt - 23.6521 * log(t_kt) - 0.019813 * sal +
                sqrt(sal) * ( - 79.2447 + 3298.72 / t_kt + 12.0408 * log(t_kt) ) )

  f_h = 1.29 - 0.00204 * t_kt + 4.6 * 1.E-4 * sal^2 - 1.48 * 1.E-6 * sal^2 * t_kt
  
  c1 = k1 / 2.0
  c2 = 1.0 - 4.0 * k2 / k1
  c4 = t_bor * -kb
  
  # message("t = ", t_atm, ", tkt = ", t_kt, ", tk = ", t_k, ", alk = ", alk_conc, ", tco2 = ", tco2_conc)
  # message("k1 = ", k1, ", k2 = ", k2, ", ksi = ", k_si, ", kp2 = ", k_p2, ", kp3 = ", k_p3, ", kw = ", k_w)
  # message("fh = ", f_h, ", c1 = ", c1, ", c2 = ", c2, ", c4 = ", c4)
  # 
  a_ht = 1.E-8
  
  for (i_cnt in seq(100)) {
    bm = t_bor * kb / (a_ht + kb)
    si_m = t_si * 4 * 1.0E-10 / (a_ht + 4 * 1.0E-10)
    p_m = t_p * ( 1.0 / ( 1.0 + k_p2 / a_ht + k_p2 * k_p3 / a_ht^2 ) +
                    2.0 / ( 1.0 + a_ht / k_p2 + k_p3 / a_ht ) +
                    3.0 / (1.0 + a_ht / k_p3 + a_ht^2 / (k_p2 * k_p3))
                  )
    w_m = k_w * f_h / a_ht - a_ht / f_h
    a = alk_conc - bm - si_m - p_m - w_m
    x = a / tco2_conc
    a_h1 = c1 / x * (1.0 - x + sqrt(1.0 + c2 * x * (x - 2.0)))
    
    # message(i_cnt, ": a = ", a, ", bm = ", bm, ", sim = ", si_m, ", pm = ", p_m, ", wm = ", w_m, ", x = ", x, ", a_ht = ", a_ht, ", a_h1 = +", a_h1)
    
    if (abs(1.0 - a_ht / a_h1) <= 5.0E-5) break
    a_ht = a_h1
  }

  co3 = (a - tco2_conc) / (1.0 - a_h1^2 / (k1 * k2))
  hco3 = tco2_conc / (1.0 + a_h1 / k1 + k2 / a_h1)
  co2 = tco2_conc / (1.0 + k1 / a_h1 + k1 * k2 / a_h1^2)

  khco2 = exp ( -60.2409 + 9345.17 / t_kt + 
                  23.3585 * log (t_kt / 100.) +
                  sal * ( 0.023517 - 2.3656e-4 * t_kt + 4.7036e-7 * t_kt^2 ) )
  
  pco2 = 1E6 * co2 / khco2 # saturation vapor pressure of CO2
  # Unit conversions
  # co2 = 1E6 * co2 # millimoles per cubic meter
  co3 = 1E6 * co3 # millimoles per cubic meter
  # hco3 = 1E6 * hco3 # millimoles per cubic meter
  
  c(co2 = co2, hco3 = hco3, co3 = co3, pco2 = pco2, khco2 = khco2)
}



#
# pCO2 = atmospheric saturation in parts per million
# alk = Ocean alkalinity in moles per cubic meter, as reported by the GEOCARB program.
# tco2 = Total mineral carbon in the ocean, in moles per cubic meter, as reported by the GEOCARB program.
# t_atm = atmospheric temperature in degrees C
# 
# Returns the fraction of ocean CO2 saturation, from zero to one.
#
co2_saturation <- function(pco2, alk, tco2, t_atm) {
  # Note: the TCO2 and ALK columns in the Chicago model are transposed, so fix them here
  correct_alk = tco2
  correct_tco2 = alk
  if (is.vector(pco2)) {
    vapor_pressure = lapply(seq_along(pco2), function(i) {
      partition(t_atm[i], correct_alk[i], correct_tco2[i])['pco2']
    }) %>% unlist() %>% unname()
  } else {
    part = partition(t_atm = t_atm, alk = correct_alk, tco2 = correct_tco2)
    vapor_pressure = part['pco2']
  }
  vapor_pressure / pco2
}
