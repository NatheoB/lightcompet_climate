Get_Species_Occurence_treestatus <- function(data_NFIs, treestatus_to_keep) {
  
  ### Number of trees per plot
  ntreesplot <- data_NFIs$trees %>% 
    dplyr::left_join(data_NFIs$plots, by = "plotcode") %>% 
    dplyr::filter(treestatus %in% treestatus_to_keep) %>% 
    
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
  
  ### Frequence of occurences in the main country
  freq_country <- ntreesplot %>% 
    dplyr::group_by(species, country) %>% 
    dplyr::summarize(ntrees = sum(ntreesplot)) %>% 
    dplyr::left_join(occ_all %>% 
                       dplyr::select(species, ntrees_all = ntrees), 
                     by = "species") %>%
    dplyr::mutate(freqtrees = round(ntrees/ntrees_all*100)) %>% 
    dplyr::select(-ntrees, -ntrees_all) %>%
    tidyr::pivot_wider(names_from = country, values_from = freqtrees,
                       names_sep = "_", values_fill = 0) %>%
    dplyr::ungroup()


  dplyr::left_join(occ_all, freq_country, by = "species") %>%
    dplyr::arrange(desc(ntrees))
}
