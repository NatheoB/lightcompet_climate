# From : 
# Phenological maps of Europe
# Thomas Rötzer*, Frank-M. Chmielewski
# Climate research Vol. 18: 249–257, 2001

# From https://www.worlddata.info
mean_alt_countries <- c(
  "ES" = 660, "WA" = 181, #Belgium
  "DE" = 263, "SW" = 320, "FI" = 164,
  "FR" = 375, "PL" = 173, "SI" = 492
)


Compute_GrowingSeason_BegDate <- function(country, x, y, z) {
  # If no available altitude, set alt as the mean country one
  z[is.na(z)] <- mean_alt_countries[country[is.na(z)]]
  
  round(-32.6 + 0.5*x + 2.3*y + 3.1*z/100)
}

Compute_GrowingSeason_EndDate <- function(country, x, y, z) {
  # If no available altitude, set alt as the mean country one
  z[is.na(z)] <- mean_alt_countries[country[is.na(z)]]
  
  round(310.6 + -0.2*x + -0.1*y + -1.0*z/100)
}


Compute_VegetPeriod_LocationDependent <- function(data_plots) {
  veget_period_df <- data_plots %>% summarize(
    plotcode,
    leaf_on_doy = Compute_GrowingSeason_BegDate(country, longitude, latitude, altitude),
    leaf_off_doy = Compute_GrowingSeason_EndDate(country, longitude, latitude, altitude),
  )
  
  return(veget_period_df)
}