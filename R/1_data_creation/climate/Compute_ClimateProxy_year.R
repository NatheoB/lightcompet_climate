Compute_sgdd_weightedsum_year <- function(tas_months, days, sgdd_threshold = 5.5) {
  
    # Substract sgdd threshold to all tas values
  tas_months <- tas_months - sgdd_threshold
  
    # Set tas to 0 if tas < sgdd threshold
  tas_months[tas_months<0] <- 0
  
    # Weighted Sum of all columns
  return(sum(tas_months*days))
} 


Compute_ClimateProxy_year <- function(chelsa_years, liquid_water_years, year, save_file) {
  
  print(year)
  
  # Liquid water models
  lw_mods <- c("MM", "DD")
  
  # Bind chelsa vars and computed available water dataframes
  data_all <- dplyr::left_join(chelsa_years %>% purrr::keep(~ .x$year == year) %>% purrr::map_df(~ .x$data), 
                               liquid_water_years %>% purrr::keep(~ .x$year == year) %>% purrr::map_df(~ .x$data), 
                               by = "plotcode")
  
  # Init output dataframe
  data_climate_proxy <- data.frame(plotcode = data_all$plotcode)
  
  
  ### COMPUTE MONTHLY VARIABLES, wa = water availability, wd = water deficit, petd = pet deficit
  
  # Get month as character (e.g. 01 for January)
  months <- list(
    "all" = c(paste0("0", 1:9), 10:12),
    ".summer" = c("06", "07", "08"),
    ".winter" = c("12", "01", "02"),
    ".spring" = c("03", "04", "05"),
    ".autumn" = c("09", "10", "11"),
    ".gs" = c("05", "06", "07", "08", "09")
  )
  
  # Get number of days in each month of given year (my)
  mys <- paste0(months[["all"]], "_", year) 
  days <- lubridate::days_in_month(lubridate::my(mys))
  
  
  print("Computing month variables...")
  for (month in months[["all"]]) {
    
    ## USING CHELSA PRECIPITATIONS
      # wa = Pr - PET (monthly water balance)
    data_climate_proxy[,paste("wa", month, year, sep="_")] <-
      data_all[,paste("pr", month, year, sep="_")] - 
      data_all[,paste("pet_penman", month, year, sep="_")]
    
      # wd = abs(wa) if wa negative, 0 otherwise (water stress)
    data_climate_proxy[,paste("wd", month, year, sep="_")] <- 
      pmax(- data_climate_proxy[,paste("wa", month, year, sep="_")], 0)
    
    
    ## USING COMPUTED LIQUID WATER lw
    
    for (mod in lw_mods) {
      
      # lwa = lw - PET (monthly water balance) = liquid water availability
      data_climate_proxy[,paste("lwa", mod, month, year, sep="_")] <-
        data_all[,paste("lw", mod, month, year, sep="_")] - 
        data_all[,paste("pet_penman", month, year, sep="_")]
      
      # lwd = abs(lwa) if wa negative, 0 otherwise (water stress)
      data_climate_proxy[,paste("lwd", mod, month, year, sep="_")] <- 
        pmax(- data_climate_proxy[,paste("lwa", mod, month, year, sep="_")], 0)
      
    }
    
  }
  
  print("Computing variables for each period...")
  ### COMPUTE YEARLY VARIABLES : sgdd, tasmin, tasmax, (snow/liquid) precipitations, snow storage, (liquid) water availability (index)/deficit
  
  # print("sgdd")
    # Compute yearly sgdd : sum of growing degree days above a 5.5°C threshold during a given year
  data_climate_proxy[,paste0("sgdd_", year)] <- apply(as.matrix(select(data_all, contains("tascorrect_"))),
                                                                1, Compute_sgdd_weightedsum_year, days)
  
  # For each year period
  for (i in 1:length(months)) {
    
    # Get period name and months
    period <- ifelse(names(months)[i] == "all", "", names(months)[i])
    period_months <- months[[i]]
    print(period)
    
    # print("minmax")
    # Compute yearly tasmin and tasmax : minimum and maximum temperature of the year
    data_climate_proxy[,paste0("tasmin", period, "_", year)] <- 
      rowMeans(data_all[,paste("tasmin", period_months, year, sep="_")])
    
    data_climate_proxy[,paste0("tasmax", period, "_", year)] <- 
      rowMeans(data_all[,paste("tasmax", period_months, year, sep="_")])

    
    # print("chelsa raw")
      # Compute total precipitations
    data_climate_proxy[,paste0("pr", period, "_", year)] <- 
      rowSums(data_all[,paste("pr", period_months, year, sep="_")])

      # Compute wai : water availability index
    data_climate_proxy[,paste0("wai", period, "_", year)] <- 
      rowSums(data_climate_proxy[,paste("wa", period_months, year, sep="_")]) /
      rowSums(data_all[,paste("pet_penman", period_months, year, sep="_")])
    
      # Compute wa : water availability
    data_climate_proxy[,paste0("wa", period, "_", year)] <- 
      rowSums(data_climate_proxy[,paste("wa", period_months, year, sep="_")])
    
      # Compute wd : water deficit
    data_climate_proxy[,paste0("wd", period, "_", year)] <- 
      rowSums(data_climate_proxy[,paste("wd", period_months, year, sep="_")])
    
    
    # print("liquid water")
    # For McCabe Markstrom and Degree day models 
    
    for (mod in lw_mods) {
      
      # Compute liquid precipitations
      data_climate_proxy[,paste0("lpr_", mod, period, "_", year)] <- 
        rowSums(data_all[,paste("pr.liquid", mod, period_months, year, sep="_")])
      
      # Compute solid precipitations
      data_climate_proxy[,paste0("spr_", mod, period, "_", year)] <- 
        rowSums(data_all[,paste("pr.snow", mod, period_months, year, sep="_")])
      
      # Compute snow storage
      data_climate_proxy[,paste0("snostor_", mod, period, "_", year)] <- 
        rowMeans(data_all[,paste("snostor", mod, period_months, year, sep="_")])
      
      # Compute lw : liquid water
      data_climate_proxy[,paste0("lw_", mod, period, "_", year)] <- 
        rowSums(data_all[,paste("lw", mod, period_months, year, sep="_")])
      
      # Compute lwai : liquid water availability index
      data_climate_proxy[,paste0("lwai_", mod, period, "_", year)] <- 
        rowSums(data_climate_proxy[,paste("lwa", mod, period_months, year, sep="_")]) /
        rowSums(data_all[,paste("pet_penman", period_months, year, sep="_")])
      
      # Compute lwa : liquid water availability
      data_climate_proxy[,paste0("lwa_", mod, period, "_", year)] <- 
        rowSums(data_climate_proxy[,paste("lwa", mod, period_months, year, sep="_")])
      
      # Compute lwd : liquid water deficit
      data_climate_proxy[,paste0("lwd_", mod, period, "_", year)] <- 
        rowSums(data_climate_proxy[,paste("lwd", mod, period_months, year, sep="_")])
      
    }
    
    
  }
  
  ### Save the file
  if (save_file) {
    print("Saving dataframe...")
    write.table(data_climate_proxy, file.path("output", paste0("data_climateproxy_", year, ".csv")),
                row.names = FALSE, sep = ";", dec = ".")
  }
  
  return(list(year = year, data = data_climate_proxy))
}
