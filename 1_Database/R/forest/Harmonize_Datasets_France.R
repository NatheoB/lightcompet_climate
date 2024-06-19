Harmonize_Datasets_France <- function(filtered_data_france) {
  
  # Unlist filtered datasets
  data_trees_filtered   <- filtered_data_france$trees
  data_stands_filtered  <- filtered_data_france$plots
  
  
  ################################################################################
  ############################# CREATE TREE DATASET ##############################
  ################################################################################
  
  # Select interest variable
  data_trees_harmonized <- data_trees_filtered %>% 
    group_by() %>% 
    select(CAMPAGNE, IDP, firstVisit, A, latinName, C13, VEGET, VEGET5, W)
  
  print(head(data_trees_harmonized))
  
  # Make the dataframe wider (one row per tree)
  data_trees_harmonized <- data_trees_harmonized %>% 
    tidyr::pivot_wider(names_from = firstVisit,
                names_sep = "_",
                values_from = c(CAMPAGNE, latinName, C13, VEGET, VEGET5, W))
  
  
  # Country variables
  data_trees_harmonized <- data_trees_harmonized %>% 
    mutate(country = "FR", countrycode = 6)
  
  
  # Plotcode and treecode variables
  data_trees_harmonized <- data_trees_harmonized %>% 
    mutate(plotcode = paste0(countrycode, "_", IDP),
           treecode = paste0(countrycode, "_", IDP, "_", A))
  
  
  # Select again interest variable after making the df wider
  data_trees_harmonized <- data_trees_harmonized %>%
    select(country, countrycode,
           plotcode, treecode,
           VEGET5 = VEGET5_FALSE,
           c13_1 = C13_TRUE, c13_2 = C13_FALSE,
           weight = W_TRUE,
           latinName = latinName_TRUE
    )
  
  
  # Create treestatus variable (from VEGET5) (cf documentation)
  # table(data_trees_harmonized$VEGET5, useNA = "always")
  
  data_trees_harmonized <- data_trees_harmonized %>% 
    mutate(treestatus = case_when(
      VEGET5 == "0" ~ 2,
      VEGET5 %in% c("6", "7") ~ 3,
      VEGET5 %in% c("1", "2", "5", "A", "C", "M", "T") ~ 4,
      VEGET5 == "N" ~ 5
    ))
  data_trees_harmonized$treestatus <- as.integer(data_trees_harmonized$treestatus)
  
  
  # Convert C13 (in m) into DBH (in mm)
  data_trees_harmonized <- data_trees_harmonized %>% 
    mutate(dbh1 = round(c13_1/pi * 1000),
           dbh2 = round(c13_2/pi * 1000))
  data_trees_harmonized$dbh1 <- as.integer(data_trees_harmonized$dbh1)
  data_trees_harmonized$dbh2 <- as.integer(data_trees_harmonized$dbh2)
  
  # Set latinName as character variable
  data_trees_harmonized$latinName <- as.character(data_trees_harmonized$latinName)
  
  
  # Select final variables
  data_trees_harmonized <- data_trees_harmonized %>%
    select(treecode, plotcode,
           species = latinName, treestatus,
           dbh1, dbh2,
           weight1 = weight, weight2 = weight)
  
  
  
  ################################################################################
  ############################# CREATE STAND DATASET #############################
  ################################################################################
  
  # Select interest variables
  data_stands_harmonized <- data_stands_filtered %>% 
    group_by() %>% 
    select(IDP, CAMPAGNE, firstVisit, XL, YL, altitude)
  
  
  # Wider the table to transform the two rows for each visit into one row with two colums for each visit
  data_stands_harmonized <- data_stands_harmonized %>% 
    pivot_wider(names_from = firstVisit,
                names_sep = "_",
                values_from = c(CAMPAGNE, XL, YL, altitude))
  
  
  # Check if coord variables are the same from first to second visit
  data_stands_harmonized <- data_stands_harmonized %>% 
    mutate(XL_SAME = (XL_TRUE==XL_FALSE),
           YL_SAME = (YL_TRUE==YL_FALSE)
    )
  
  sapply(X = c("XL_SAME", "YL_SAME"), function(X) {
    table(data_stands_harmonized[,X], useNA = "always")
  }) # 5 have different XL, 2 have different YL : keep second visit coords
  
  
  # Reselect variables after wider transformation
  data_stands_harmonized <- data_stands_harmonized %>% 
    select(IDP, altitude = altitude_FALSE,
           surveydate1 = CAMPAGNE_TRUE, surveydate2 = CAMPAGNE_FALSE,
           XL = XL_FALSE, YL = YL_FALSE)
  
  
  # Create country variables
  data_stands_harmonized <- data_stands_harmonized %>% 
    mutate(country = "FR", countrycode = 6)
  
  
  # Create poltcode variable
  data_stands_harmonized <- data_stands_harmonized %>% 
    mutate(plotcode = paste0(countrycode, "_", IDP))
  
  
  # Convert coord from Lambert93 French national projection system to WGS84 projection system
  coordPlots_L93 <- terra::vect(data_stands_harmonized %>% 
                                  select(XL, YL), 
                                geom = c("XL", "YL"), crs = "EPSG:2154")
  coordPlots_WGS84 <- as.data.frame(terra::geom(terra::project(coordPlots_L93, "EPSG:4326")))
  
  data_stands_harmonized$longitude <- coordPlots_WGS84$x
  data_stands_harmonized$latitude <- coordPlots_WGS84$y
  
    # OLD VERSION WITH SP
  # sp::coordinates(coordPlots_L93) <- c("XL", "YL")
  # 
  # sp::proj4string(coordPlots_L93) <- sp::CRS("+init=epsg:2154") #https://epsg.io/2154 : corresponding EPSG for Lambert93
  # coordPlots_WGS84 <- sp::spTransform(coordPlots_L93,  sp::CRS("+init=epsg:4326")) # WGS84
  # 
  # data_stands_harmonized$longitude <- sp::coordinates(coordPlots_WGS84)[,"XL"]
  # data_stands_harmonized$latitude <- sp::coordinates(coordPlots_WGS84)[,"YL"]
  
  
  # Change date : from year to date (date-01-01)
  data_stands_harmonized <- data_stands_harmonized %>% 
    mutate(surveydate1 = as.Date(paste0(surveydate1, "-01-01"), format = "%Y-%m-%d"),
           surveydate2 = as.Date(paste0(surveydate2, "-01-01"), format = "%Y-%m-%d"))
  
  
  
  # Filter the dataset
  data_stands_harmonized <- data_stands_harmonized %>% 
    select(plotcode, country,
           longitude, latitude, altitude,
           surveydate1, surveydate2)
  
  
  
  ##################################
  ######## CLEAN DATASETS ##########
  ##################################
  
  # Remove plots with NA species
  plots_na_sp <- unique(data_trees_harmonized %>% filter(is.na(species)) %>% pull(plotcode)) #1 plot
  
  data_trees_harmonized <- data_trees_harmonized %>% filter(!(plotcode %in% plots_na_sp))
  data_stands_harmonized <- data_stands_harmonized %>% filter(!(plotcode %in% plots_na_sp))
  
  # Remove plots with dead trees and weight1 NA or 0 (5 plots)
  data_trees_harmonized <- data_trees_harmonized %>% filter(!(treestatus %in% c(3, 4, 5, 6) & (is.na(weight1) | weight1 == 0)))
  data_stands_harmonized <- data_stands_harmonized %>% filter(plotcode %in% unique(data_trees_harmonized$plotcode))
  
  
  ###### RETURN DATASETS AS A LIST #####
  return(list(trees = data_trees_harmonized, plots = data_stands_harmonized))
}
