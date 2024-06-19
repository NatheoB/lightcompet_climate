# Sort the raw FunDiv dataset to :
#   - remove France NFI
#   - get only interest variables
#   - add countrycode before treecode and plotcode (unique ID for sure)
#   - transform FunDiv weights (radius of plots) into IFN weights (number of equivalent trees in 1ha plot)


Harmonize_Datasets_FunDiv <- function(data_fundiv) {
  
  data_plots <- data_fundiv$plots
  data_trees <- data_fundiv$trees
  
  
  ##### PLOTS DATASET #####

    # Add country code before plotcode
  data_plots <- data_plots %>% 
    mutate(plotcode = paste0(case_when(
      country == "ES" ~ 1,
      country == "WA" ~ 2,
      country == "SW" ~ 4,
      country == "FI" ~ 5
    ), "_", plotcode))
  
  # Set altitude variable as numerical avriable
  data_plots$altitude <- as.numeric(data_plots$altitude)
  
  # Change type of variables
  data_plots$surveydate1 <- as.Date(data_plots$surveydate1)
  data_plots$surveydate2 <- as.Date(data_plots$surveydate2)
  
  
  # Select only interest variables
  data_plots <- data_plots %>% 
    select(plotcode, country,
           longitude, latitude, altitude,
           surveydate1, surveydate2)
  
  
  ##### TREES DATASET #####

    # Add country code before plotcode and treecode
  data_trees <- data_trees %>% 
    mutate(plotcode = paste0(case_when(
      country == "ES" ~ 1,
      country == "WA" ~ 2,
      country == "SW" ~ 4,
      country == "FI" ~ 5
    ), "_", plotcode),
    treecode = paste0(case_when(
      country == "ES" ~ 1,
      country == "WA" ~ 2,
      country == "SW" ~ 4,
      country == "FI" ~ 5
    ), "_", treecode))
  
    # Set type of speciesid variable as character
  data_trees$latinname <- as.character(data_trees$latinname)
  
    # Treestatus
  data_trees$treestatus <- as.integer(data_trees$treestatus_th)
  
    # Change modalities of Spanish trees between dead stem present and dead stem absent (4 and 5) (ERROR IN DATA)
  plotcodes <- data_plots %>% filter(country == "ES") %>% pull(plotcode)
  
  esp_4_mod <- which(data_trees$treestatus == 4 & data_trees$plotcode %in% plotcodes)
  esp_5_mod <- which(data_trees$treestatus == 5 & data_trees$plotcode %in% plotcodes)
  
  data_trees[esp_4_mod,]$treestatus = 5
  data_trees[esp_5_mod,]$treestatus = 4
  
    # DBH variable
  data_trees$dbh1 <- as.integer(data_trees$dbh1)
  data_trees$dbh2 <- as.integer(data_trees$dbh2)
  
    # Weight variable (in the dataset, weight is the radius of the sampled plot)
  data_trees <- data_trees %>% 
    dplyr::mutate(
      weight1 = case_when(
        weight1 == 0 ~ 0,
        TRUE ~ 10000 / (pi * weight1^2)
      ),
      weight2 = case_when(
        weight2 == 0 ~ 0,
        TRUE ~ 10000 / (pi * weight2^2)
      )
    )
  
    # Get only interest variables
  data_trees <- data_trees %>% 
    select(treecode, plotcode,
           species = latinname, treestatus,
           dbh1, dbh2, weight1, weight2)
  
  
  ##### RETURN DATASETS AS A LIST ##### 
  return(list(trees = data_trees, plots = data_plots))
}