# Need function R/NFIs/Filter_NFIsDataset_RemoveSp

Filter_NFIsDataset_rareSpecies <- function(data_NFIs, min_plots, min_trees) {
  
  i <- 0
  
  while (TRUE) {
    
    i <- i + 1
    print(paste("Filter number", i))
    
    ### Number of trees per plot
    ntreesplot <- data_NFIs$trees %>% 
      dplyr::left_join(data_NFIs$plots, by = "plotcode") %>% 
      dplyr::filter(treestatus %in% c(2, 4)) %>% 
      
      ### Number of plots with one occurence at least
      dplyr::group_by(plotcode, species, country) %>% 
      dplyr::summarize(ntreesplot = n()) %>% 
      dplyr::ungroup()
    
    
    ### Occurences in the whole dataset
    occ_all <- ntreesplot %>%  
      dplyr::group_by(species) %>% 
      dplyr::summarize(
        nplots = length(ntreesplot),
        ntrees = sum(ntreesplot)
      ) %>% 
      dplyr::ungroup()
    
    
    ### Remove rare species if there are still some
    species_to_remove <- occ_all %>% 
      dplyr::filter(nplots < min_plots | ntrees < min_trees) %>% 
      dplyr::pull(species)
    
    if (length(species_to_remove) == 0) break
    
    print(species_to_remove)
    data_NFIs <- Filter_NFIsDataset_removeSp(data_NFIs, species_to_remove)
  
  }
  
  return(data_NFIs)
}