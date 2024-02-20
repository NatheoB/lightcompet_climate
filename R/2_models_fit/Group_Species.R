#' @description Group species into a single one and keep specified ones
#' 
#' @param dataset_ungrouped data.frame - Dataset with ungrouped species
#' @param species_to_group_list named list of string vectors - 
#' Each string vector are the consider species names to group into the same name specified in the list element name
#' @param species_to_keep string vector - Species name to keep before grouping
#'
#' @return data.frame - New dataset with keeped and duplicated species
#'
#' @example 
#' species_to_group_list <- list(
#'  "Betula spp." = c("Betula pendula", "Betula pubescens", "Betula spp."),
#'  "Populus spp." = c("Populus nigra", "Populus spp.", "Populus tremula"))
#' dataset <- Group_Species(dataset, species_to_group_list, c("Populus tremula", "Betula pendula"))
#'
Group_Species <- function(dataset_ungrouped,
                          species_to_group_list, species_to_keep) {
  
  # Create a copy of the dataset
  dataset_grouped <- data.frame(dataset_ungrouped)
  
  
  # Group species given in the list and set group name
  for (i in seq_along(species_to_group_list)) {
    
    dataset_grouped$species[dataset_grouped$species %in% species_to_group_list[[i]]] <- 
      names(species_to_group_list)[i]
    
  }
  
  
  # Add species given in species_to_keep
  dataset_grouped <- dataset_grouped %>%
    dplyr::add_row(dataset_ungrouped %>% dplyr::filter(species %in% species_to_keep))
  
  
  return(dataset_grouped)
}