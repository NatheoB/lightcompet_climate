Compute_Radiation_MonthlyMean <- function(data_radiation) {
  
  # Init variables and months
  vars <- c("Hrad", "ratio")
  months <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
  
  # Init list output
  out <- replicate(length(vars), vector(mode = "list", length = length(months)), simplify = F)
  
  # For each variable and each month
  for (v in seq_along(vars)) {
    
    for (m in seq_along(months)) {
      
      var <- vars[[v]]
      month <- months[[m]]
      
      out[[v]][[m]] <- data_radiation %>% 
        dplyr::select(plotcode, dplyr::matches(paste(var, "(.*)", month, sep="_"))) %>% 
        dplyr::mutate("{var}" := rowMeans(dplyr::select(., grep(var, names(.)))),
                      month = m) %>% 
        dplyr::select(plotcode, month, all_of(var))
                      
    }
    
  }
  
  # Bind months by rows and Left join radiation variables
  out %>% 
    purrr::map(dplyr::bind_rows) %>% 
    purrr::reduce(dplyr::left_join, by = c("plotcode", "month")) %>% 
    dplyr::arrange(plotcode, month)
  
}


#### OLD FUNCTION BUT TOO SLOW ####

# Compute_Radiation_MonthlyMean_plot <- function(row_id, pvgis_all) {
#   print(row_id)
#     # Get data pvgis for the given plotcode
#   pvgis_plot <- pvgis_all[row_id,]
#   
#     # For both vars : horizontal radiation Hrad and ratio diffuse/global ratio
#   pvgis_plot.mean <- sapply(c("Hrad", "ratio"), function(var) {
#       # For each month
#     months <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
#     sapply(months, function(m) {
#         # Compute mean of years
#       mean(unlist(pvgis_plot[paste(var, 2005:2020, m, sep="_")]), na.rm = T)
#     })
#   })
#   
#     # Add numeric month column
#   pvgis_plot.mean <- cbind(month = 1:12, pvgis_plot.mean)
#   
#     # Convert dataframe into string
#   data_str <- paste(apply(pvgis_plot.mean, MARGIN=1, paste, collapse = "\t"), collapse = "\n")
# 
#   return(c(plotcode = pvgis_plot[["plotcode"]], data = data_str))
# }
# 
# 
# Compute_Radiation_MonthlyMean <- function(pvgis_all) {
#     # Compute for all plotcodes
#   pvgis_plots_means <- lapply(1:nrow(pvgis_all),
#                               Compute_Radiation_MonthlyMean_plot, pvgis_all)
#   
#   return(pvgis_plots_means)
# }
