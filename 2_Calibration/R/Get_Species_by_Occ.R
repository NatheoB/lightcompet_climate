#' @description Search for species to keep with at least a given occurences of trees and plots, for the given status
#'
#' @param species_occurences data.frame - For each species, for each status, number of trees and plots
#' @param nplots_min integer - Minimum number of plots where the species occurs at least once threshold to keep the species
#' @param nliving_min integer -  Minimum number of observed living trees to keep the species
#' @param ndead_min integer - Minimum number of dead observed to keep the species
#' 
#' @return string vector - Species names to keep
#'
Get_Species_by_Occ <- function(species_occurences, 
                               nplots_min, 
                               nliving_min, 
                               ndead_min) {

  # Vector of columns to add if no dead trees
  cols_dead <- c(ntrees_dead = 0, nplots_dead = 0)
  
  # Get species names to keep
  species_occurences %>% 
    tidyr::pivot_wider(everything(), 
                       names_from = status,
                       values_from = c(ntrees, nplots)) %>% 
    tibble::add_column(!!!cols_dead[!names(cols_dead) %in% names(.)]) %>% 
    replace(is.na(.), 0) %>% 
    dplyr::filter(nplots_alive + nplots_dead >= nplots_min & 
                    ntrees_alive >= nliving_min &
                    ntrees_dead >= ndead_min) %>%
    dplyr::arrange(desc(ntrees_alive)) %>%
    dplyr::select(species) %>%
    dplyr::pull(species)
  
}