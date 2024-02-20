#' @description Clean global dataset by removing individuals with incoherent attributes
#' 
#' @param dataset_global data.frame - Global Samsara calibration dataset (merged raw datasets) 
#'  
#' @return data.frame - Cleaned global dataset
#'  
Clean_GlobalDataset <- function(dataset_global) {
  
  dataset_clean <- dataset_global %>% 
    
    # Remove trees with dyears = 0
    dplyr::filter(dyears > 0) %>% 
    
    # Remove growing trees with negative or null increment (cannot be logged)
    dplyr::filter(!(treestatus == 2 & dD_mm_year <= 0)) %>%
    
    # Remove rows with 0 < dbh < 100 (different recruitment dbh between countries)
    dplyr::filter(!(dbh_mm >= 0 & dbh_mm < 100)) %>% 
    
    # Remove rows with NAs (especially aet2pet: lack of data during computing)
    tidyr::drop_na(sgdd, aet2pet) %>% 
    
    # Remove Wallonia
    dplyr::filter(country != "WA")
  
  
  ### REMOVE DISTURBED PLOTS
  
    # Compute annual death rate and 
  plots_disturbed <- dataset_clean %>% 
    dplyr::group_by(plotcode, country) %>% 
    dplyr::summarise(prop_dead = sum(treestatus == 4) / sum(treestatus %in% c(2, 4)),
                     death_rate_year = prop_dead/dyears) %>% 
    dplyr::mutate(disturbed = death_rate_year >= 0.1 | prop_dead >= 0.5) %>% 
    dplyr::ungroup()
  
  print("Disturbed plots:")
  print(table(plots_disturbed$disturbed, plots_disturbed$country))
  
    # Remove disturbed plots
  plotcodes_disturbed <- plots_disturbed %>%
    dplyr::filter(disturbed) %>% 
    dplyr::pull(plotcode) 
  
  dataset_clean %>% 
    dplyr::filter(!plotcode %in% plotcodes_disturbed)
  
  
}