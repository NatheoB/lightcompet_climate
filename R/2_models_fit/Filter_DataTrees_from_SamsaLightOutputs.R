#' @description Filter datasets by keeping only trees where light has been computed from SamsaraLight
#' 
#' @param data_trees data.frame - Dataset with NFI trees
#' @param data_compet data.frame - Dataset with computed competition indicators (need only light competition index) computed with SamsaraLight
#' 
#' @return data.frame - Filtered data_trees dataset, removing trees without light computation
#' 
Filter_DataTrees_from_SamsaLightOutputs <- function(data_trees, data_compet) {
  
  treecodes <- data_compet %>% 
    dplyr::filter(!is.na(lci_mean)) %>% 
    dplyr::pull(treecode)
  
  data_trees %>%
    dplyr::filter(treecode %in% treecodes)
  
}







