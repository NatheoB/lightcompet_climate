Filter_NFIsDataset_treestatus <- function(data_NFIs, keep_treestatus) {
  
  data_NFIs$trees <- data_NFIs$trees %>% 
    dplyr::filter(treestatus %in% keep_treestatus)
  
  data_NFIs$plots <- data_NFIs$plots %>% 
    dplyr::filter(plotcode %in% unique(data_NFIs$trees$plotcode))
  
  data_NFIs
}