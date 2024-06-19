#' @description Merge list of datasets for Samsara calibration into a single one
#' 
#' @param data_trees data.frame - Dataset with NFIs trees
#' @param data_plots data.frame - Dataset with NFIs plots
#' @param data_compet data.frame - Dataset with computed competition indicators for each NFI tree
#' @param data_climate data.frame - Dataset with mean climate of the given NFI plot between the two surveys
#' 
#' @return data.frame - Merged dataset from trees, plots, compet and climate datasets
#' 
Merge_Datasets_from_DataTrees <- function(data_trees, data_plots,
                                          data_compet, data_climate) {
  
  data_trees %>% 
    left_join(data_compet, by = "treecode") %>% 
    left_join(data_plots, by = "plotcode") %>% 
    left_join(data_climate, by = "plotcode")
  
}