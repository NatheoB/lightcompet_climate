Filter_NFIsDataset_removeSp <- function(data_NFIs, species_to_remove) {
  
  data_trees_filtered <- data_NFIs[["trees"]] %>% 
    dplyr::group_by(plotcode) %>% 
    dplyr::mutate(remove = sum(species %in% species_to_remove) > 0) %>% 
    dplyr::filter(!remove) %>% 
    dplyr::select(-remove) %>% 
    dplyr::ungroup()
  
  
  data_plots_filtered <- data_NFIs[["plots"]] %>% 
    dplyr::filter(plotcode %in% unique(data_trees_filtered$plotcode))
  
  return(list(trees = data_trees_filtered, plots = data_plots_filtered))
}