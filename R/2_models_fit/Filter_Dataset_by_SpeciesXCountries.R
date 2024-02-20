#' Remove countries with less than specified living and dead individuals for each species
#'
#' @param nliving_min_per_country Minimum number of living individuals per species and country 
#' @param ndead_min_per_country Minimum number of dead individuals per species and country 
#' 
Filter_Dataset_by_SpeciesXCountries <- function(dataset,
                                                nliving_min_per_country = 100,
                                                ndead_min_per_country = 20) {
  
  sp_country_to_remove <- dataset %>% 
    dplyr::group_by(species, country, dead) %>% 
    dplyr::summarise(nind = n()) %>% 
    dplyr::ungroup() %>% 
    tidyr::complete(species, country, dead) %>% # In case 0 observation
    replace(is.na(.), 0) %>% 
    dplyr::mutate(to_remove = 
                    (dead == 0 & nind < nliving_min_per_country) | 
                    (dead == 1 & nind < ndead_min_per_country)) %>% 
    dplyr::group_by(species, country) %>% 
    dplyr::summarise(to_remove = sum(to_remove) > 0)
  
  dataset %>% 
    dplyr::left_join(sp_country_to_remove, by = c("country", "species")) %>% 
    dplyr::filter(!to_remove) %>% 
    dplyr::select(-to_remove)
  
}