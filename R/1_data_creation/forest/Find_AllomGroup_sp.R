Find_AllomGroup_sp <- function(speciesinfo, species_alloms_spspecific) {
  
  for (var_allom in names(species_alloms_spspecific)) {
    
    speciesinfo <- speciesinfo %>% 
      dplyr::mutate("{var_allom}_spspe" := species %in% species_alloms_spspecific[[var_allom]],
                    "{var_allom}_corresp" := ifelse(species %in% species_alloms_spspecific[[var_allom]],
                                                    species, NA))
    
  }
  
  speciesinfo %>% 
    dplyr::mutate(remarks_alloms = "")
}