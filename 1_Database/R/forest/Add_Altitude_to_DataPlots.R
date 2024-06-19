Add_Altitude_to_DataPlots <- function(data_list, data_altitude, plotcode_colname = "plotcode") {
  
  data_list$plots <- data_list$plots %>% 
    left_join(data_altitude, by = plotcode_colname)
  
  return(data_list)
}