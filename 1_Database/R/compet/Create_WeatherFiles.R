Create_WeatherFiles <- function(df_monthly_rad, df_vegetperiod,
                                use_turbid_medium = TRUE) {

  # For each plot, create and save weather file
  df_monthly_rad %>% 
  
    # Convert monthly radiation dataframe into a string and create final radiation string
    tidyr::unite(df_rad, c(month, Hrad, ratio), sep = "\t") %>% 
    dplyr::group_by(plotcode) %>%
    dplyr::summarise(df_rad = paste0(df_rad, collapse = "\n")) %>% 
    dplyr::mutate(str_rad = paste(
                    "#Radiation data",
                    "#Month Global  Diffus/G",
                    df_rad,
                    sep = "\n")) %>%
    
    # Add veget period final string
    dplyr::left_join(df_vegetperiod, by = "plotcode") %>% 
    dplyr::mutate(str_vegetperiod = paste0(
          "#Vegetation period", "\n",
          "#leaf_on_doy, leaf_off_doy : day of year (see the calendar, fc)", "\n",
          "leaf_on_doy = ", leaf_on_doy, "\n",
          "leaf_off_doy = ", leaf_off_doy, "\n")) %>% 
    
    # SamsaraLight params string
    dplyr::mutate(str_params = paste0(
      "#light model options", "\n",
      "turbid_medium = ", tolower(as.character(use_turbid_medium)), "\n",
      "trunk_interception = true", "\n", 
      "direct_angle_step = 15", "\n",
      "height_angle_min = 10", "\n",
      "#use_diffuse = true", "\n",
      "diffuse_angle_step = 15", "\n",
      "#SOC or UOC" , "\n",
      "soc = false", "\n",
      "GMT = -1"
    )) %>% 
      

    # Bind all string together to obtain final string
    tidyr::unite("out_str", 
                 str_params, str_vegetperiod, str_rad,
                 sep = "\n\n") %>% 
    
    # Select variables
    dplyr::select(plotcode, out_str)
    
}


##### OLD METHOD BUT TOO LONG #####

# Create_and_Save_WeatherFiles_plot <- function(plotcode, weather_filename, vegetperiod_df, 
#                                               rad_list,
#                                               output_folderpath) {
#   
#   # Vegetation period
#   file_str <- paste0(
#     "#Vegetation period", "\n",
#     "#leaf_on_doy, leaf_off_doy : day of year (see the calendar, fc)", "\n",
#     "leaf_on_doy = ", vegetperiod_df$leaf_on_doy[which(vegetperiod_df$plotcode==plotcode)], "\n",
#     "leaf_off_doy = ", vegetperiod_df$leaf_off_doy[which(vegetperiod_df$plotcode==plotcode)], "\n",
#     "\n")
#   
#   # Turbid medium
#   file_str <- paste0(
#     file_str,
#     "# Other params", "\n",
#     "turbid_medium = ", "true", "\n",
#     "\n"
#   )
#   
#   # Radiation data
#   rad <- rad_list[sapply(rad_list, function(X) X[["plotcode"]]==X)][[1]]
#   file_str <- paste0(
#     file_str,
#     "#Radiation data", "\n",
#     "#Month	Global	Diffus/G", "\n",
#     rad[["data"]]
#   )
#   
#   
#   
#   ##### SAVE FILE #####
#   
#   # Create a weather folder specific to the plot in output/simus_tmp/"plotcode" if it does not exists
#   weather_folderpath <- file.path(output_folderpath, "weather", weather_filename, plotcode)
#   if (!file.exists(weather_folderpath)) {
#     dir.create(weather_folderpath, recursive = TRUE)
#   }
#   
#   # Save file as .txt
#   path <- file.path(weather_folderpath, paste0("weather_", weather_filename, "_", plotcode, ".txt"))
#   writeLines(file_str, path)
#   
#   return(path)
# }
# 
# 
# 
# Create_and_Save_WeatherFiles <- function(plotcodes, 
#                                          weather_filename, weather_files_vegetperiod,
#                                          rad_list,
#                                          output_folderpath) {
#     
#   # Create output folder if it does not exist
#   if (!file.exists(output_folderpath)) {
#     dir.create(output_folderpath, recursive = TRUE)
#   }
#   
#   # For each plot, create and save weather file
#   weatherfiles_fps <- do.call("c", lapply(plotcodes, Create_and_Save_WeatherFiles_plot,
#                                           weather_filename, weather_files_vegetperiod[[weather_filename]],
#                                           rad_list,
#                                           output_folderpath))
#   
#   return(list(name = weather_filename, val = weatherfiles_fps))
# }