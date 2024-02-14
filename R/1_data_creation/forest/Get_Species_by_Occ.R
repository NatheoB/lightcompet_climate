Get_Species_by_Occ <- function(data_occ, min_plots, min_trees) {
  
  data_occ %>% 
    dplyr::filter(nplots >= min_plots & ntrees >= min_trees) %>% 
    dplyr::pull(species)
  
}
