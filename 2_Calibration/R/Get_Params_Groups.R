Get_Params_Groups <- function(out_fits, species_info) {
  
  # Find if each species have a group
  sp_groups <- out_fits %>% 
    dplyr::select(species) %>% 
    dplyr::distinct() %>% 
    dplyr::left_join(species_info %>% dplyr::select(species, order, shadetol), by = "species") %>% 
    dplyr::mutate(group = paste0(shadetol, order))
  
  sp_missing_order <- sp_groups %>% dplyr::filter(is.na(order)) %>% dplyr::pull(species)
  if (length(sp_missing_order) > 0) stop(paste("missing order info for", paste(sp_missing_order, collapse = " - ")))
  
  sp_missing_shadetol <- sp_groups %>% dplyr::filter(is.na(shadetol)) %>% dplyr::pull(species)
  if (length(sp_missing_shadetol) > 0) stop(paste("missing shadetol info for", paste(sp_missing_shadetol, collapse = " - ")))

  out_fits <- out_fits %>% 
    dplyr::left_join(sp_groups, by = "species")
  
  # Find the best sgdd form for each order
  sgdd_type_order <- out_fits %>% 
    dplyr::group_by(species, order, sample, fold, type_compet, type_sgdd) %>% 
    dplyr::summarise(aic = weighted.mean(aic, weight),
                     order = unique(order)) %>% 
    dplyr::group_by(order, type_compet, type_sgdd) %>% 
    dplyr::summarise(aic = mean(aic)) %>% 
    dplyr::group_by(order, type_compet) %>% 
    dplyr::arrange(aic) %>% 
    dplyr::filter(row_number() == 1) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(order, type_compet, best_type_sgdd = type_sgdd)
  
  sgdd_type_group <- out_fits %>% 
    dplyr::group_by(group, sample, fold, type_compet, type_sgdd) %>% 
    dplyr::summarise(aic = weighted.mean(aic, weight),
                     group = unique(group)) %>% 
    dplyr::group_by(group, type_compet, type_sgdd) %>% 
    dplyr::summarise(aic = mean(aic)) %>% 
    dplyr::group_by(group, type_compet) %>% 
    dplyr::arrange(aic) %>% 
    dplyr::filter(row_number() == 1) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(group, type_compet, best_type_sgdd = type_sgdd)
  
  # Get params for the given sgdd form for each order and each group
  out_order <- out_fits %>% 
    dplyr::left_join(sgdd_type_order, by = c("order", "type_compet")) %>% 
    dplyr::filter(type_sgdd == best_type_sgdd) %>%
    
    # Replace non fitted parameters by 0
    replace(., is.na(.), 0) %>%   
    
    # Summarise parameters of each submodel
    dplyr::group_by(order, sample, fold, type_compet, type_sgdd) %>% 
    dplyr::summarise(across(c(any_of(c("auc_roc", "mae", "rmse")), contains(c(".est", "sigma"))), ~weighted.mean(., w = weight))) %>% 
    dplyr::rename_all(~gsub(".est", "", .)) %>% 
    dplyr::rename_all(~gsub("coef.", "coef_", .)) %>% 
    dplyr::rename_all(~gsub("_inv", "inv", .)) %>% 
    dplyr::rename_all(~gsub("_log", "log", .)) %>% 
    dplyr::ungroup()
  
  
  out_group <- out_fits %>% 
    dplyr::left_join(sgdd_type_group, by = c("group", "type_compet")) %>% 
    dplyr::filter(type_sgdd == best_type_sgdd) %>%
    
    # Replace non fitted parameters by 0
    replace(., is.na(.), 0) %>%   
    
    # Summarise parameters of each submodel
    dplyr::group_by(group, sample, fold, type_compet, type_sgdd) %>% 
    dplyr::summarise(across(c(any_of(c("auc_roc", "mae", "rmse")), contains(c(".est", "sigma"))), ~weighted.mean(., w = weight))) %>% 
    dplyr::rename_all(~gsub(".est", "", .)) %>% 
    dplyr::rename_all(~gsub("coef.", "coef_", .)) %>% 
    dplyr::rename_all(~gsub("_inv", "inv", .)) %>% 
    dplyr::rename_all(~gsub("_log", "log", .)) %>% 
    dplyr::ungroup()
  
  dplyr::bind_rows(
    out_order %>% 
      dplyr::mutate(order = if_else(order == "a", "angiosperm", "gymnosperm")) %>% 
      dplyr::rename(group = order),
    out_group
  ) %>% 
    dplyr::relocate(group)
}
