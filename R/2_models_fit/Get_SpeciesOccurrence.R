#' @description Get number of individuals and plots where there is at least one occurrence for each species
#' 
#' @param dataset data.frame - Global dataset for Samsar growth and mortality calibration (need columns plotcode, species and status)
#' 
#' @return data.frame - For each species, for each status, number of trees and plots
#' 
Get_SpeciesOccurrence <- function(dataset) {
  
  dataset %>% 
    
    # Keep only growing and dead trees
    dplyr::filter(treestatus %in% c(2, 4)) %>% 
    dplyr::mutate(status = ifelse(treestatus == 2, "alive", "dead")) %>% 

    # Compute number of individuals per species, per status, per plot
    dplyr::group_by(plotcode, species, status) %>% 
    dplyr::summarize(ntreesplot = n()) %>% 
    dplyr::ungroup() %>% 
    
    # Compute number of trees per species
    # and number of plots with at least one occurrence of the given species
    dplyr::group_by(species, status) %>% 
    dplyr::summarize(
      nplots = length(ntreesplot),
      ntrees = sum(ntreesplot)
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::arrange(status, desc(ntrees))
  
}