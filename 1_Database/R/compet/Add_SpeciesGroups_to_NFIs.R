Add_SpeciesGroups_to_NFIs <- function(data_NFIs, species_groups) {
  
  data_NFIs$trees <- data_NFIs$trees %>% 
    dplyr::left_join(species_groups, by = "species") %>% 
    dplyr::unite()
  
  
  
}