# Need function Get_NandV_Groups_sp in R/Compet folder

Assign_Alloms_sp <- function(params_alloms_sp_list, params_alloms_groups, 
                             data_NFIs, speciesinfo) {
  
  # Assign allometric parameters (species-specific, otherwise by group of order and shade tolerance)
  for (allom in names(params_alloms_sp_list)) {
    
      # Create allometric file (bind species and group specific allometries)
    params_alloms_var <- params_alloms_sp_list[[allom]] %>%
      dplyr::rename(species_allom = species) %>% 
      dplyr::bind_rows(params_alloms_groups %>% 
                         dplyr::select(group, contains(allom)) %>% 
                         dplyr::rename(species_allom = group))


      # Assign allometry parameters for each tree
    data_NFIs$trees <- data_NFIs$trees %>% 
      
        # Get allometry to take for the given species from the species info file
      dplyr::left_join(speciesinfo %>% 
                         dplyr::select(species, species_allom = paste0(allom, "_corresp")), 
                       by = "species") %>% 
    
        # Left join correct allometry
      dplyr::left_join(params_alloms_var, by = "species_allom") %>% 
      dplyr::select(-species_allom)
    
  }
  
  data_NFIs
}