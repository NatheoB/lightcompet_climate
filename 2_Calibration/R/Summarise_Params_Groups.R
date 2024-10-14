Summarise_Params_Groups <- function(params_all) {
  
  # Summarise parameters of each sampleXfold
  params_all %>% 
    dplyr::group_by(group, type_compet, type_sgdd) %>% 
    dplyr::select(-sample, -fold) %>% 
    dplyr::summarise_all(mean)
  
}