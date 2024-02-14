Harmonize_Datasets_Poland <- function(data_poland_filtered) {
  
  # Unlist filtered datasets
  data_trees  <- data_poland_filtered$trees
  data_plots  <- data_poland_filtered$plots
  
  
  #######################################
  ######### CREATE TREE DATASET #########
  #######################################
  
  # Bind the plot area for each tree
  data_trees <- data_trees %>% 
    left_join(data_plots %>% dplyr::select(id_plot, smaller_area),
              by = "id_plot")
  
  # Calculate the area column (of the sampled plot in m2) and weight (number of equivalent trees per ha)
  data_trees <- data_trees %>% 
    dplyr::mutate(
      weight1 = case_when(
        is.na(dbh2) ~ 0,
        TRUE ~ 10000/smaller_area),
      weight2 = case_when(
        is.na(dbh3) ~ 0,
        TRUE ~ 10000/smaller_area))
  
  data_trees$dbh2 <- as.integer(data_trees$dbh2)
  data_trees$dbh3 <- as.integer(data_trees$dbh3)
  
  
  # Treestatus 
  data_trees$treestatus <- as.integer(data_trees$treestatus)
  
  # Add the country and country code variables, and create plotcode and treecode variables to add countrycode as prefixe
  data_trees <- data_trees %>% 
    mutate(country = "PL", countrycode = 7,
           plotcode = paste0(countrycode, "_", id_plot),
           treecode = paste0(countrycode, "_", id_tree))
  
  
  # Convert species variable as character
  data_trees$latinname <- as.character(data_trees$latinname)
  
  
  
  #######################################
  ######### CREATE PLOT DATASET #########
  #######################################

  # Prepare country, countrycode, plotcode  variables
  data_plots <- data_plots %>% 
    mutate(country = "PL", countrycode = 7,
           plotcode = paste0(countrycode, "_", id_plot))
  
  # Change comma to point as decimal in elevation column
  data_plots$elevation <- as.numeric(gsub(",", "\\.", data_plots$elevation))
  
  # Check NA years and if NA add or substract 5 years (systematic survey with 5y interval)
  data_plots$year3[is.na(data_plots$year3)] <- data_plots$year2[is.na(data_plots$year3)] + 5
  data_plots$year2[is.na(data_plots$year2)] <- data_plots$year3[is.na(data_plots$year2)] - 5
  
  # Add yearsbetweensurveys variables and modify date from year to date-01-01
  data_plots <- data_plots %>% mutate(
    surveydate1 = as.Date(paste0(year2, "-01-01", format = "%Y-%m-%d")),
    surveydate2 = as.Date(paste0(year3, "-01-01", format = "%Y-%m-%d"))
  )
  
  
  #######################################
  ########### SELECT VARIABLES ##########
  #######################################
  
  # Select variables for trees dataset according to FunDivEUROPE conventions
  data_trees <- data_trees%>% 
    ungroup() %>% 
    select(treecode, plotcode,
           species = latinname, treestatus,
           dbh1 = dbh2, dbh2 = dbh3, weight1, weight2)
  
  
  # Select variables for plots dataset according to FunDivEUROPE conventions
  data_plots <- data_plots %>%
    ungroup() %>% 
    select(plotcode, country,
           longitude, latitude, altitude = elevation,
           surveydate1, surveydate2)
  
  
  ##### RETURN DATASETS AS A LIST #####
  return(list(trees = data_trees, plots = data_plots))
}