Find_Allomgroup_sp <- function(speciesinfo, params_alloms_sp_list) {
  
  for (var_allom in names(params_alloms_sp_list)) {
    
    sp_allom <- unique(params_alloms_sp_list[[var_allom]]$species)
    
    speciesinfo <- speciesinfo %>% 
      dplyr::mutate("{var_allom}_spspe" := species %in% sp_allom,
                    "{var_allom}_corresp" := ifelse(species %in% sp_allom,
                                                    species, NA))
    
  }
  
  speciesinfo %>% 
    dplyr::mutate(remarks_alloms = "")
}