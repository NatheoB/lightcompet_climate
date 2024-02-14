Filter_NFIsDataset_noManagement <- function(data_NFIs, Nharvested_threshold) {
  
  # Get plots where there is less or equals harvested trees than given Nharvested_threshhold
  plots_noManagament <- data_NFIs[["trees"]] %>% 
    mutate(isHarvested = treestatus==3) %>% 
    group_by(plotcode) %>% 
    summarize(Nharvested = sum(isHarvested)) %>% 
    filter(Nharvested <= Nharvested_threshold) %>% 
    pull(plotcode)
  
  # Filter trees and plots datasets according to kept plotcodes
  data_trees_filtered <- data_NFIs[["trees"]] %>% filter(plotcode %in% plots_noManagament)
  data_plots_filtered <- data_NFIs[["plots"]] %>% filter(plotcode %in% plots_noManagament)
  
  return(list(trees = data_trees_filtered, plots = data_plots_filtered))
}
