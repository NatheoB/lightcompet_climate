Compute_VegetPeriod_Fixed <- function(data_plots, day_start, day_end) {
  
  data_plots %>% 
    dplyr::group_by(plotcode) %>% 
    dplyr::summarize(
      leaf_on_doy = day_start,
      leaf_off_doy = day_end
  )
  
}