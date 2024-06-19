Homogenize_Latinnames <- function(data_NFIs, data_sp_std_corresp) {

      # Join harmonized latinname by raw latin name and remove raw species name column
  data_NFIs$trees <- data_NFIs$trees %>% 
    dplyr::left_join(data_sp_std_corresp, by = c("species" = "rawname")) %>% 
    dplyr::select(-species) %>%
    dplyr::rename(species = latinname)
  
  return(data_NFIs)
}