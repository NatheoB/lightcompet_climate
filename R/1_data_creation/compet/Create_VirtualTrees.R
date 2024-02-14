Create_VirtualTrees <- function(data_NFIs, plot_dim_m) {
  
  # Compute weight of plots per ha
  virtualplot_weight <- plot_dim_m**2/10000
  
  # Duplicate NFI trees based on tree weight and plot weight and assign a unique id
  data_virtual_trees <- data_NFIs$trees %>% 
    tidyr::uncount(as.integer(round(weight1*virtualplot_weight)), .remove = FALSE) %>% 
    dplyr::group_by(plotcode) %>% 
    dplyr::mutate(lilo_id = row_number()) %>% 
    dplyr::ungroup()
  
  return(list(plots = data_NFIs$plots, trees = data_virtual_trees))
}