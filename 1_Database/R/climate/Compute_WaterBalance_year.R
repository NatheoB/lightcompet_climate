# source("Compute_LiquidWater.R")

Compute_NewSWC_Linear <- function(swc, swhc, pr, pet) {
  
  # Be careful, pr - pet if negative if we enter in this function
  # Thus, do not forget to take the absolute value of pr-pet (i.e. -(pr-pet) = pet-pr)
  swc - ( (pet - pr) * (swc / swhc) )
  
}


Compute_NewSWC_Exponential <- function(swc, swhc, pr, pet) {
  
  swc * exp( (pr - pet) / swhc )
  
}


Compute_WaterBalance_Piedallu_month <- function(swc, swhc,
                                                pr, pet,
                                                link_function) {
  
  # Soil water balance performs better than climatic water variables in tree species distribution modelling
  # https://doi.org/10.1111/geb.12012
  # Piedallu et al. 2012
  
  ### Compute the soil water content
  water_balance <- pr - pet
  
  # If total water added pr_tot is greater than pet
  # ==> all water needed is taken from water entry and water in excess go to soil water content
  
  # When during the month, there are less precipitations than the potential evapotranspiration
  # The lack of water is taken from the swc as a function of water deficiency and soil water holding capacity
  # The more water is lacking, the more swc is reduced
  swc_new <- ifelse(water_balance > 0, 
                    swc + water_balance,
                    link_function(swc, swhc, pr, pet))
  
  ### Limit soil water content to soil water holding capacity and to 0
  # And compute soil water surplus as water in excess compared to swhc
  swc_new <- pmax(swc_new, 0)
  water_surplus <- pmax( swc_new - swhc, 0 ) 
  swc_new <- pmin(swc_new, swhc)
  
  ### Compute actual evapotranspiration
  # If more precipitations than PET thus, aet is equal to pet
  # Otherwise, AET is the precipitations + the water that has been pumped from the soil water reservoir
  aet <- ifelse(water_balance > 0,
                pet,
                pmin(pet, pr + swc - swc_new))
  
  
  ### Compute soil water indicators

  # Soil water deficit (difference between pet and aet)
  water_deficit <- pet - aet
  
  # Compute aet to pet ratio (be careful if pet = 0)
  aet2pet_ratio <- ifelse(pet == 0, 1, aet/pet)
  
  # Soil water content index
  swci <- swc_new / swhc
  
  
  ### Store results in a list
  out_month <- list(swc = swc_new, swci = swci,
                    aet2pet = aet2pet_ratio,
                    swd = water_deficit)
  
  return(out_month)
}



Compute_WaterBalance_McCandM_month <- function(swc, swhc, 
                                               pr, pet, tas,
                                               altitude, snow_storage,
                                               link_function) {
  
  
  out_liquid_water <- Compute_LiquidWater_MM(tas, pr, snow_storage, altitude)
  
  ### Compute the soil water content as Piedallu method but with new liquid water from snow and precipitations
  out_month <- Compute_WaterBalance_Piedallu_month(swc, swhc,
                                                   out_liquid_water$lw, pet,
                                                   link_function)
    # Add snow storage to outputs
  out_month[["snow_storage"]] <- out_liquid_water$snostor
  
  return(out_month)
}



Compute_WaterBalance_year <- function(data_chelsa_yearsList, data_soil, 
                                      water_balance_params, year, 
                                      save_file) {
  
  print(paste("Computing year:", year))
  
  # Find altitude of each plot and join it to data_soil
  # (if no given altitude (e.g. finland or norway), take worldclim mean altitude of the cell
  data_soil <- data_chelsa_yearsList[[1]]$data %>% 
    dplyr::select(plotcode, altitude, alt_wc) %>% 
    dplyr::mutate(alt = ifelse(is.na(altitude), alt_wc, altitude)) %>% 
    dplyr::select(plotcode, alt) %>% 
    dplyr::left_join(data_soil, by = "plotcode")


    # Get first year of the climate dataset
  first_year_chelsa <- data_chelsa_yearsList[[1]]$year

  # Vector of months strings
  months_all <- c(paste0("0", 1:9), 10:12)
  months_summer <- c("06", "07", "08")
  
  # Loop over computation parameters (add snow or use computed or fixed (60cm) soil depth)
  water_balance_df <- vector(mode = "list", length = length(water_balance_params))
  for (i_param in 1:nrow(water_balance_params)) {
    
    print(water_balance_params$id[i_param])
    
    # Use computed soil water content at FC and WP from fixed or variable soil depth (60cm)
    if (water_balance_params$locDepDepth[i_param]) {swhc <- data_soil$swhcTot}
    else {swhc <- data_soil$swhcTot_60cm}
    
    
    # Use the given link function (exponential or linear to compute new swc)
    link_function <- ifelse(water_balance_params$expAET[i_param],
                            Compute_NewSWC_Exponential,
                            Compute_NewSWC_Linear)

    # Init the soil water content at t=0 as if the reservoir was at maximum = soil water holding capacity
    swc <- swhc
    
    # Init the snow storage at 0 (for McCandM model)
    snow_storage <- rep(0, nrow(data_soil))
    
    # Compute water balance from two years before the given year to have an estimate of the reservoir in january
    for (yr in (year-2):year) {
      
      # Be careful to the first two years (chelsa year is the taken year for chelsa variables)
      if (yr < first_year_chelsa) {yr_chelsa <- first_year_chelsa}
      else {yr_chelsa <- yr}
      
      # Get the precipitations and pet_penman for the considered year
      chelsa_yearsList_bool <- sapply(data_chelsa_yearsList, function(X) {return(X$year == yr_chelsa)})
      data_chelsa_year <- data_chelsa_yearsList[chelsa_yearsList_bool][[1]]$data %>% 
        dplyr::select(plotcode, contains(c("pr", "pet_penman", "tascorrect")))
      
      # Bind climate data to soil data
      data_all <- data_soil %>% 
        dplyr::left_join(data_chelsa_year, by = "plotcode")
      
      # For each month, compute the soil water balance
      water_balance_df_param <- vector(mode = "list", length = length(months_all))
      for (month in months_all) {
        
        # Compute monthly soil balance given the computation function 
        # (Piedallu : raw precipitations or McCandM : modif precipitations with runoff and snow)
        if (water_balance_params$PrModif[i_param]) {
          
          # Compute Mc Cabe and Markstrom water balance
          water_balance_df_param_month <- Compute_WaterBalance_McCandM_month(
            swc, swhc,
            data_all[[paste("pr", month, yr_chelsa, sep="_")]], 
            data_all[[paste("pet_penman", month, yr_chelsa, sep="_")]],
            data_all[[paste("tascorrect", month, yr_chelsa, sep="_")]],
            data_all[["alt"]], snow_storage,
            link_function
          )
          
          # Update snow storage
          snow_storage <- water_balance_df_param_month$snow_storage
          
        } else {
          
          # Compute Piedallu water balance
          water_balance_df_param_month <- Compute_WaterBalance_Piedallu_month(
            swc, swhc,
            data_all[[paste("pr", month, yr_chelsa, sep="_")]], 
            data_all[[paste("pet_penman", month, yr_chelsa, sep="_")]],
            link_function
          )
          
        }
        
        # Get the new swc at the beginning of the next month
        swc <- water_balance_df_param_month$swc
        
        # Store and rename columns (add month) only if third year (the considered one)
        if (yr == year) {
          names(water_balance_df_param_month) <- paste0(names(water_balance_df_param_month),
                                                         ".", water_balance_params$id[i_param],
                                                         "_", month, "_", year)
          water_balance_df_param[[month]] <- water_balance_df_param_month
        }
        
      }
      
    }
    
    # Bind all month as columns
    water_balance_df_param <- bind_cols(water_balance_df_param)
    
    # Compute yearly water balance variables : mean of swci and aet to pet monthly ratio and sum of year water deficit)
    water_balance_df_param[[paste0("swci",
                                   ".", water_balance_params$id[i_param],
                                   "_", year)]] <- rowMeans(water_balance_df_param[,paste0("swci",
                                                                                           ".", water_balance_params$id[i_param],
                                                                                           "_", months_all, "_", year)], na.rm = T)
    
    water_balance_df_param[[paste0("aet2pet", 
                                    ".", water_balance_params$id[i_param],
                                    "_", year)]] <- rowMeans(water_balance_df_param[,paste0("aet2pet", 
                                                                                             ".", water_balance_params$id[i_param],
                                                                                             "_", months_all, "_", year)], na.rm = T)
    water_balance_df_param[[paste0("swd",
                                    ".", water_balance_params$id[i_param],
                                    "_", year)]] <- rowSums(water_balance_df_param[,paste0("swd",
                                                                                            ".", water_balance_params$id[i_param],
                                                                                            "_", months_all, "_", year)], na.rm = T)
   
    
    
    # Add output with the given parameters into the list of outputs
    water_balance_df[[i_param]] <- water_balance_df_param
  }
  
  # Bind all water balance variables for all parameters and add plotcode
  water_balance_df <- dplyr::bind_cols(plotcode = data_soil$plotcode, water_balance_df)

  # Save the file
  if (save_file) {
    print("Saving file...")
    write.table(water_balance_df, file.path("output", paste0("data_waterbalance_", year, ".csv")),
                row.names = FALSE, sep = ";", dec = ".")
  }
  
  return(list(year = year, data = water_balance_df))
}