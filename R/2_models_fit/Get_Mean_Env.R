Get_Mean_Env <- function(data) {
  
  data %>% 
    dplyr::group_by(species) %>% 
    dplyr::summarise(across(c("dbh", "aet2pet", "sgdd", "lci", "bat"),
                            list("mean" = mean,
                                 "qt025" = ~quantile(., 0.025),
                                 "qt1" = ~quantile(., 0.1),
                                 "qt9" = ~quantile(., 0.9),
                                 "qt975" = ~quantile(., 0.975)))) %>% 
    dplyr::ungroup()
  
}