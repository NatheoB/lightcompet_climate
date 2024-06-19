Assign_Order_sp <- function(data_NFIs, species_order) {
  
  data_NFIs$trees <- data_NFIs$trees %>% 
    dplyr::left_join(species_order %>% dplyr::select(species, order), by = "species")
  
  data_NFIs
}