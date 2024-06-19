Compute_LiquidWater_DD <- function(tas, pr, snostor, ndays) {
  
  # Computation based on https://pubs.usgs.gov/of/2007/1088/pdf/of07-1088_508.pdf
  
  ##### FIXED PARAMETERS #####
  
  # Temperatures when precipitations are snow
  T_snow <- 0
  
  # Beta parameter = degreday factor in mm water equivalent per °C
  dd_fact <- 5.5
  
  
  ##### COMPUTE LIQUID WATER #####
  
  # Compute liquid and snow precipitations (snow if T =< T_snow, rain otherwise)
  pr_snow <- ifelse(tas > 0, 0, pr)
  pr_liquid <- pr - pr_snow
  
  # Compute maximum snow that will melt into liquid available water
  melted_water_max <- dd_fact * ndays * pmax(tas - T_snow, 0)
  
  # Add snow precipitations and remove snow melting from an infinite reservoir of snow water equivalent precipitaions
  # Melted water does not depend on sow storage, thus limit snow storage to 0)
  snostor_new <- pmax(snostor + pr_snow - melted_water_max, 0)
  
  # Get true melted water from new snow storage
  melted_water <- pmax(snostor - snostor_new, 0)
  
  # Total liquid water are the total precipitations - snow precipitations + water from snow melting 
  liquid_water <- pr_liquid + melted_water
  
  
  # Return variables
  out_month <- data.frame(
    pr.snow = pr_snow, pr.liquid = pr_liquid,
    lw = liquid_water, mw = melted_water,
    snostor = snostor_new
  )
  
  return(out_month)
  
}


Compute_LiquidWater_MM <- function(tas, pr, snostor, altitude) {
  
  # From McCabe and Markstrom
  # Computation based on https://pubs.usgs.gov/of/2007/1088/pdf/of07-1088_508.pdf
  
  ##### FIXED PARAMETERS #####
  
  # Temperatures when precipitations are full rain (T_rain)
  # or full snow over or below 1000m altitude (T_snow)
  T_rain <- 3.3
  T_snow_inf1000 <- -10
  T_snow_sup1000 <- -1
  
  # Maximum snow melt fraction in a month
  snowmelt_frac_max <- 0.5
  
  
  ##### COMPUTE WATER BALANCE #####
  
  ### Compute snow accumulation 
  # (estimation of the amount of monthly precipitation (pr) that is rain (pr_rain) or snow (pr_snow), in millimeters)
  
  # Temperature above which all precipitations are snow, according to altitude
  T_snow <- ifelse(altitude > 1000, T_snow_sup1000, T_snow_inf1000)
  
  
  # Compute fraction of precipitations that will be snow
  # Linear function between Tsnow and Train, and then 0 if T is greater than T_rain or 1 if T is lower than T_snow
  # Fraction for snow that will melt and become available water is 1 - fraction of snow precipitations
  snowfrac <- pmax(0, pmin(1, (T_rain - tas) / (T_rain - T_snow)))
  
  # Compute snow precipitations as a fraction of total precipitations
  pr_snow <- snowfrac * pr
  pr_liquid <- pr - pr_snow
  
  # Compute snow that will melt from the snow reservoir into liquid available water
  # Cannot melt more than "snowmelt_frac_max" proportion of the snow reservoire
  snowmelt_frac <- pmin( (1-snowfrac), snowmelt_frac_max)
  melted_water <- snowmelt_frac * snostor
  
  
  # Add snow precipitations and remove snow melting from an infinite reservoir of snow water equivalent precipitaions
  snostor <- snostor + pr_snow - melted_water
  
  
  # Total liquid water are the total precipitations - snow precipitations + water from snow melting 
  liquid_water <- pr - pr_snow + melted_water
  
  
  # Return variables
  out_month <- data.frame(
    snowfrac = snowfrac, snowmeltfrac = snowmelt_frac,
    pr.snow = pr_snow, pr.liquid = pr_liquid,
    lw = liquid_water, mw = melted_water,
    snostor = snostor
  )
  
  return(out_month)
  
}