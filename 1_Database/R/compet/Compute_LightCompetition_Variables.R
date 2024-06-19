Compute_LightCompetition_Variables <- function(lilo_outputs, data_NFIslight) {
  
  lilo_outputs %>% 
    dplyr::left_join(data_NFIslight$trees %>% 
                       dplyr::select(plotcode, treecode, lilo_id), 
                     by = c("site" = "plotcode", "id" = "lilo_id")) %>% 
    dplyr::select(-site, -id) %>% 
    
    dplyr::group_by(treecode) %>%
    
    dplyr::mutate(lci = 1 - e/epot) %>% 
    dplyr::summarise(
      e_mean = mean(e),
      e_sd = sd(e),
      
      epot_mean = mean(epot),
      epot_sd = sd(epot),
      
      lci_mean = mean(lci),
      lci_sd = sd(lci)
    )
  
}