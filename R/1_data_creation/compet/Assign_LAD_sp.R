Assign_LAD_sp <- function(data_lad, data_NFIs, species_ladgroup) {
  
  data_NFIs$trees <- data_NFIs$trees %>% 
    dplyr::left_join(species_ladgroup %>% dplyr::select(species, lad_group), by = "species") %>% 
    dplyr::left_join(data_lad %>% dplyr::select(group, lad = lad_mean),
                     by = c("lad_group" = "group")) %>% 
    dplyr::select(-lad_group)
 
  data_NFIs
}