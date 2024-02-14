# source("Compute_LiquidWater.R")

Compute_LiquidWater_year <- function(data_chelsa_yearsList, year,
                                     save_file) {
  
  print(paste("Computing year:", year))
  
  # Find altitude of each plot
  # (if no given altitude (e.g. finland or norway), take worldclim mean altitude of the cell
  altitudes <- data_chelsa_yearsList[[1]]$data %>% 
    dplyr::select(plotcode, altitude, alt_wc) %>% 
    dplyr::mutate(alt = ifelse(is.na(altitude), alt_wc, altitude)) %>% 
    dplyr::select(plotcode, alt)
  
  
  # Get first year of the climate dataset
  first_year_chelsa <- data_chelsa_yearsList[[1]]$year
  
  # Vector of months strings
  months_all <- c(paste0("0", 1:9), 10:12)
    
  
  # Init the snow storage at 0
  snow_storage_MM <- rep(0, length(first_year_chelsa))
  snow_storage_DD <- rep(0, length(first_year_chelsa))
  
  
    
  # Compute water balance from two years before the given year to have an estimate of the snow reservoir in january
  for (yr in (year-2):year) {
    
    # Be careful to the first two years (chelsa year is the taken year for chelsa variables)
    if (yr < first_year_chelsa) {yr_chelsa <- first_year_chelsa}
    else {yr_chelsa <- yr}
    
    # Get the precipitations and pet_penman for the considered year
    chelsa_yearsList_bool <- sapply(data_chelsa_yearsList, function(X) {return(X$year == yr_chelsa)})
    data_chelsa_year <- data_chelsa_yearsList[chelsa_yearsList_bool][[1]]$data %>% 
      dplyr::select(plotcode, contains(c("pr", "pet_penman", "tascorrect")))
    
    # Bind climate and altitude data
    data_all <- data_chelsa_year %>% 
      dplyr::left_join(altitudes, by = "plotcode")
    
    # For each month, compute the soil water balance
    out_year <- vector(mode = "list", length = length(months_all))
    for (month in months_all) {
      
      ## from McCabe and Markstrom
      # Compute available water (precipitations considering snow) 
      out_month_MM <- Compute_LiquidWater_MM(
        data_all[[paste("tascorrect", month, yr_chelsa, sep="_")]],
        data_all[[paste("pr", month, yr_chelsa, sep="_")]], 
        snow_storage_MM,
        data_all[["alt"]])
      
      # Update snow storage
      snow_storage_MM <- out_month_MM$snostor
      
      
      ## From degree day model
      # Compute available water (precipitations considering snow) 
      out_month_DD <- Compute_LiquidWater_DD(
        data_all[[paste("tascorrect", month, yr_chelsa, sep="_")]],
        data_all[[paste("pr", month, yr_chelsa, sep="_")]], 
        snow_storage_DD,
        as.integer(lubridate::days_in_month(lubridate::my(paste0(month, year))))
        )
      
      # Update snow storage
      snow_storage_DD <- out_month_DD$snostor
      
      # Store and rename columns (add month) only if third year (the considered one)
      if (yr == year) {
        names(out_month_MM) <- paste(names(out_month_MM), "MM", month, year, sep = "_")
        names(out_month_DD) <- paste(names(out_month_DD), "DD", month, year, sep = "_")
        
        out_year[[month]] <- dplyr::bind_cols(out_month_MM, out_month_DD)
      }
      
    }
      
  }
      
  # Bind all month as columns and add plotcode
  out_year <- dplyr::bind_cols(plotcode = data_all$plotcode, 
                               dplyr::bind_cols(out_year))
  
  
  # Save the file
  if (save_file) {
    print("Saving file...")
    write.table(out_year, file.path("output", paste0("data_liquidwater_", year, ".csv")),
                row.names = FALSE, sep = ";", dec = ".")
  }
  
  return(list(year = year, data = out_year))
}