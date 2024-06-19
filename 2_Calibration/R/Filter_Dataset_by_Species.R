#' @description Filter dataset by keeping only given species
#' 
#' @param dataset data.frame - Calibration dataset with a "species" column
#' @param species_to_keep integer vector - Species one want to keep
#' 
#' @return data.frame - Filtered dataset
#' 
Filter_Dataset_by_Species <- function(dataset, species_to_keep) {
  
  dataset %>% 
    dplyr::filter(species %in% species_to_keep)
}
