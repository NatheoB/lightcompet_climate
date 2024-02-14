Compute_Climate_MeanIncrementPeriod <- function(waterbalance_years, water_balance_params,
                                                climate_proxy_years, 
                                                chelsa_years, data_plots) {
  
  
  print("Binding years dataframes...")
  # Select the interest variables for each yearly dataframe and bind them into a single one
  
    ## Get Water balance variables for each water balance parameters (i.e. var.param)
    ## And climatic proxy variables for each years
  vars_wb <- expand.grid(var = c("swci", "aet2pet", "swd"),
                         param = water_balance_params$id) %>%
    tidyr::unite("vars", c("var", "param"), sep = ".") %>%
    dplyr::pull(vars)
    
  vars_proxy <- c("sgdd",
                  expand.grid(var = c("tasmin", "tasmax",
                                      "pr", 
                                      "wa", "wai", "wd",
                                      "lpr_MM", "spr_MM", "snostor_MM",
                                      "lpr_DD", "spr_DD", "snostor_DD",
                                      "lw_MM", "lwa_MM", "lwai_MM", "lwd_MM",
                                      "lw_DD", "lwa_DD", "lwai_DD", "lwd_DD"),
                            param = c("", ".summer", ".winter", ".spring", ".autumn", ".gs")) %>%
    tidyr::unite("vars", c("var", "param"), sep = "") %>%
    dplyr::pull(vars)
  )
  
  vars <- c(vars_proxy, vars_wb)
  
  
    ## Create a variable for each year (var_year)
  vars_years <- expand.grid(var = vars, year = chelsa_years) %>%
    dplyr::arrange(var) %>% 
    tidyr::unite("vars", c(var, year), sep = "_") %>%
    dplyr::pull(vars)
  
  
    ## Select variables
  df_wb <- waterbalance_years %>%
    purrr::map(~.$data) %>% 
    purrr::map(dplyr::select, plotcode, dplyr::any_of(vars_years)) %>%
    purrr::reduce(dplyr::left_join, by = "plotcode")
  
  df_proxy <- climate_proxy_years %>%
    purrr::map(~.$data) %>% 
    purrr::map(dplyr::select, plotcode, dplyr::any_of(vars_years)) %>%
    purrr::reduce(dplyr::left_join, by = "plotcode") 
  
  
    ## Bind the two dataframes
  data_climate <- dplyr::left_join(df_wb, df_proxy, by = "plotcode")
  
  
  
  print("Creating weights matrix...")
  # Create a dataframe with years as columns and plots as rows
  # With weights of each year in the plot's increment period
  # I.e. 0 if year is not in increment period, 1 if year is in increment period
  # If year is not in chelsa range, consider the closest year
  weights_years <- matrix(0,
                          nrow = nrow(data_plots), 
                          ncol = length(chelsa_years),
                          dimnames = list(data_plots$plotcode,
                                          chelsa_years))

  increment_periods <- data_plots %>% 
    dplyr::select(plotcode, surveydate1, surveydate2) %>% 
    dplyr::mutate(surveydate1 = lubridate::year(surveydate1),
                  surveydate2 = lubridate::year(surveydate2))
  
  min_year <- min(chelsa_years)
  max_year <- max(chelsa_years)
  for (i in 1:nrow(increment_periods)) {

    # Get year range of the increment period of the given plot (take also the two years before)
    year_range <- seq(increment_periods$surveydate1[i]-2,
                      increment_periods$surveydate2[i])
    year_range[year_range < min_year] <- min_year
    year_range[year_range > max_year] <- max_year
    
    year_range <- table(year_range)
    
    # Fill the weights matrix
    weights_years[i, names(year_range)] <- year_range
    
  }
  
  # Create a matrix with columns as vars_years (ordered first in years and after in vars)
  # Duplicate the dataframe of yearly weights for each variable
  weights <- matrix(0,
                    nrow = nrow(data_plots), 
                    ncol = length(vars_years),
                    dimnames = list(data_plots$plotcode,
                                    vars_years))
  n_years <- length(chelsa_years)
  for (i in 0:(length(vars)-1)) {

    weights[,(i*n_years+1):((i+1)*n_years)] <- weights_years
    
  }
 
  
  print("Combining yearly variables with weight matrix...")
  # For each plotcode, multiply the variables by the weight of the given year in the weight matrix
  data_climate <- data_climate %>% dplyr::select(plotcode, colnames(weights))
  data_climate[, -1] <- data_climate[, -1] * weights
    
    
  print("Computing variables on increment period...")
  # For each plotcode, compute the mean of each variables on the increment period
  for (var in vars) {
    print(var)
    data_climate[,var] <- rowSums(data_climate[,paste(var, chelsa_years, sep = "_")]) /
      rowSums(weights_years)

  }
  
  
  print("Selecting interest variables...")
  # Select only mean and sd variables
  data_climate %>% 
    dplyr::select(plotcode, all_of(vars)) %>% 
    dplyr::rename_at(vars(all_of(vars)), ~paste0(.x,"_incrperiod"))
}
 



# Old function (but quite long ==> try vectorizing the function)

# Compute_Climate_MeanIncrementPeriod_plot <- function(row_id, 
#                                                      waterbalance_years, water_balance_params,
#                                                      climate_proxy_years,
#                                                      data_plots) {
# 
#   print(row_id)
#   
#     # Compute variables from first survey year - 2 years to second survey year
#   year_range <- seq(lubridate::year(data_plots$surveydate1[row_id])-2,
#                     lubridate::year(data_plots$surveydate2[row_id]))
#   year_range[year_range < 1983] <- 1983
#   year_range[year_range > 2018] <- 2018
#   
#   
#     # Variables on which we want to apply mean on increment period (proxy and water balance df)
#   vars_wb <- expand.grid(c("aaet2pet", "aswd", "asws",
#                            "saet2pet", "sswd", "ssws"), 
#                          water_balance_params$id) %>% 
#     tidyr::unite("vars", c("Var1", "Var2"), sep = ".") %>% 
#     dplyr::pull(vars)
#   
#   vars_wb_years <- expand.grid(vars_wb, year_range) %>% 
#     tidyr::unite("vars", c("Var1", "Var2")) %>% 
#     dplyr::pull(vars)
#   
#   vars_proxy <- c("sgdd", "tasmin", "tasmax",
#                   "swai", "awai", "swa", "awa",
#                   "swdi", "awdi", "swd", "awd")
#   vars_proxy_years <- expand.grid(vars_proxy, year_range) %>% 
#     tidyr::unite("vars", c("Var1", "Var2")) %>% 
#     dplyr::pull(vars)
#     
#     
#   
#     # Extract corresponding year, keep interect variables and bind dataframes
#     # For waterbalance variables and climate proxies
#   df_wb <- waterbalance_years %>% 
#     purrr::keep(function(x) x$year %in% year_range) %>% 
#     purrr::map(~.$data[row_id,]) %>% 
#     purrr::map(dplyr::select, plotcode, dplyr::any_of(vars_wb_years)) %>%
#     purrr::reduce(dplyr::left_join, by = "plotcode")
#   
#   df_all <- climate_proxy_years %>% 
#     purrr::keep(function(x) x$year %in% year_range) %>% 
#     purrr::map(~.$data[row_id,]) %>% 
#     purrr::map(dplyr::select, plotcode, dplyr::any_of(vars_proxy_years)) %>% 
#     purrr::reduce(dplyr::left_join, by = "plotcode") %>% 
#     dplyr::left_join(df_wb, by = "plotcode")
#   
#   
#   # Compute mean and sd of years in increment epriod for each var 
#   data_climate_plot <- data.frame("plotcode" = df_all$plotcode)
#   for (var in c(vars_wb, vars_proxy)) {
#     
#     data_climate_plot[,paste0(var, "_mean")] <- mean(unlist(df_all[1,paste(var, year_range, sep="_")]))
#     data_climate_plot[,paste0(var, "_sd")] <- sd(unlist(df_all[1,paste(var, year_range, sep="_")]))
#     
#   }
#   
#   return(data_climate_plot)
# }
# 
# 
# Compute_Climate_MeanIncrementPeriod <- function(waterbalance_years, water_balance_params,
#                                                 climate_proxy_years, 
#                                                 data_plots) {
#     # Compute for all plotcodes
#   data_climate <- lapply(1:nrow(data_plots),
#                          Compute_Climate_MeanIncrementPeriod_plot, 
#                          waterbalance_years, water_balance_params, 
#                          climate_proxy_years, 
#                          data_plots)
#   
#     # Merge data for each plotcode
#   data_climate <- dplyr::bind_rows(data_climate)
#   
#   return(data_climate)
# }



