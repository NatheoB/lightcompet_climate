Filter_NFIsDataset_sp_prop <- function(data_NFIs, species_vect, prop_threshold) {
  
  
  plotcodes <- data_NFIs[["trees"]] %>% 
  
  ### Compute basal area per hectare for each tree
    dplyr::mutate(
      G1 = pi * ( ( dbh1 / 1000 ) ** 2 ) / 4, # Be careful, dbh in mm
      G1_ha = G1 * weight1) %>% 
  
  ### Compute total basal area per hectare for each plot
    dplyr::group_by(plotcode) %>%
    dplyr::mutate(G1_tot_ha = sum(G1_ha)) %>% 
    
  ### Keep only interest species
  ### and compute % of total basal area represented by each species within each plot
    dplyr::filter(species %in% species_vect) %>%
    dplyr::group_by(plotcode, species) %>% 
    dplyr::summarize(propG1_sp = sum(G1_ha) / G1_tot_ha) %>% 
    dplyr::ungroup() %>% 
  
  ### Get plots for which sum of propBA of the given species_vect is higher than the given prop_threshold
    dplyr::group_by(plotcode) %>% 
    dplyr::summarize(propG1 = sum(propG1_sp)) %>% 
    dplyr::filter(propG1 >= prop_threshold) %>% 
    
  ### Get plotcodes to keep
    dplyr::pull(plotcode)
  
  
  ### Filter data_trees and data_plots according to plotcodes to keep
  data_trees_filtered <- data_NFIs[["trees"]] %>% filter(plotcode %in% plotcodes)
  data_plots_filtered <- data_NFIs[["plots"]] %>% filter(plotcode %in% plotcodes)
  
  return(list(trees = data_trees_filtered, plots = data_plots_filtered))
}
