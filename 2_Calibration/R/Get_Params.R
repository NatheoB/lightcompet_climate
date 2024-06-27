Get_Params <- function(out_fits) {
  
  # Find the best sgdd form for each species
  sgdd_type_sps <- out_fits %>% 
    dplyr::group_by(species, sample, fold, type_compet, type_sgdd) %>% 
    dplyr::summarise(aic = weighted.mean(aic, weight)) %>% 
    dplyr::group_by(species, type_compet, type_sgdd) %>% 
    dplyr::summarise(aic = mean(aic)) %>% 
    dplyr::group_by(species, type_compet) %>% 
    dplyr::arrange(aic) %>% 
    dplyr::filter(row_number() == 1) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(species, type_compet, best_type_sgdd = type_sgdd)
  
  # Get params for the given sgdd form for each species
  out_fits %>% 
    dplyr::left_join(sgdd_type_sps, by = c("species", "type_compet")) %>% 
    dplyr::filter(type_sgdd == best_type_sgdd) %>%
    
    # Replace non fitted parameters by 0
    replace(., is.na(.), 0) %>%   
    
    # Summarise parameters of each submodel
    dplyr::group_by(species, sample, fold, type_compet, type_sgdd) %>% 
    dplyr::summarise(across(c(any_of(c("auc_roc", "mae", "rmse")), contains(c(".est", "sigma"))), ~weighted.mean(., w = weight))) %>% 
    dplyr::rename_all(~gsub(".est", "", .)) %>% 
    dplyr::rename_all(~gsub("coef.", "coef_", .)) %>% 
    dplyr::rename_all(~gsub("_inv", "inv", .)) %>% 
    dplyr::rename_all(~gsub("_log", "log", .))
  
}
