Harmonize_Datasets_Slovenia <- function(filtered_data_slovenia)
{
  
  # Unlist filtered datasets
  data_trees  <- filtered_data_slovenia$trees
  data_plots  <- filtered_data_slovenia$plots
  
  
  ##################################
  ###### CREATE TREES DATASET ######
  ##################################
  
  # Filter the dataset
  data_trees_harmonized <- data_trees %>% 
    select(plotkey, treestatus_NFI = treecode,
           dbh1, dbh2, Latinname)
  
  # country and countrycode variable (8 for Slovenia)
  data_trees_harmonized <- data_trees_harmonized %>% 
    mutate(country = "SI",
           countrycode = 8)
  
  
  # plotcode variable (add country code before plotkey) and set it into a string
  data_trees_harmonized <- data_trees_harmonized %>% 
    mutate(plotcode = paste0(countrycode, "_", plotkey))
  
  
  # treecode variable (get a unique identifier for tree as countrycode_plotcode_treeIDwithinPlot)
  data_trees_harmonized <- data_trees_harmonized %>%
    group_by(plotcode) %>% 
    mutate(treecode = paste0(plotcode, "_", row_number(plotcode)))
  
  
  # treestatus variable (cf docu for correspondance table between Slovenian NFI and FunDivEurope)
  
  # Table of modalities before harmonizing treecode modality
  print("Treestatus before harmonization")
  print(table(data_trees_harmonized$treestatus_NFI, useNA = "always"))

  # Remove modalities 8 (tree in larger plot smaller than 30cm at survey1 but greater at survey2)
  data_trees_harmonized <- data_trees_harmonized %>% filter(treestatus_NFI != 8)
  
  # Group modalities of the trees status from NFI into modalities of FunDivEUROPE treestatus
  data_trees_harmonized <- data_trees_harmonized %>%
    mutate(treestatus = case_when(
      treestatus_NFI == 3 ~ 1, # ingrowth (tree not present in the first survey)
      treestatus_NFI == 0 ~ 2, # survivor
      treestatus_NFI %in% c(1, 9) ~ 3, # dead (harvested)
      treestatus_NFI == 2 ~ 4, # dead (stem present)
      treestatus_NFI %in% c(4, 6) ~ 6 # For light calculation (here, corrected tree measurement through regression)
    ))
  
  data_trees_harmonized$treestatus <- as.integer(data_trees_harmonized$treestatus)
  
  
  # Table of modalities after harmonization
  print("Treestatus after hamonization")
  print(table(data_trees_harmonized$treestatus, useNA = "always"))
  
  # weight column : radius of the plot on which the tree has been measured 
  
  # Bind r1 and r2 columns of data_plots to data_trees
  data_trees_harmonized <- data_trees_harmonized %>% 
    left_join(data_plots %>% select(plotkey, r1, r2), by = "plotkey")
  
  # Tree's weight is r1 for DBH > 30cm (larger plot) and r2 for 10cm < DBH < 29.99cm (smaller plot)
  data_trees_harmonized <- data_trees_harmonized %>% 
    mutate(
      radius1 = case_when(
        dbh1 < 30 ~ r2, # r2 = smaller plots : for smaller trees (10cm<dbh<30cm)
        dbh1 >= 30 ~ r1), # r1 = larger plots : for larger trees (>= 30cm dbh)
      radius2 = case_when(
        dbh2 < 30 ~ r2, # r2 = smaller plots : for smaller trees (10cm<dbh<30cm)
        dbh2 >= 30 ~ r1),
      
      weight1 = 10000 / (pi * radius1^2),
      weight2 = 10000 / (pi * radius2^2)
      )
  
  # Convert dbh1 and dbh2 from cm to mm
  data_trees_harmonized <- data_trees_harmonized %>% 
    mutate(dbh1 = dbh1*10, 
           dbh2 = dbh2*10)
  data_trees_harmonized$dbh1 <- as.integer(data_trees_harmonized$dbh1)
  data_trees_harmonized$dbh2 <- as.integer(data_trees_harmonized$dbh2)
  
  
  # Convert Latinname variable as character
  data_trees_harmonized$Latinname <- as.character(data_trees_harmonized$Latinname)
  
  
  
  ##################################
  ###### CREATE PLOTS DATASET ######
  ##################################
  
  
  data_plots_harmonized <- data_plots %>% 
    select(plotkey,
           yearsbetweensurveys = year_dif,
           surveydate1 = year1, surveydate2 = year2,
           Xdispl, Ydispl, elv)
  
  # country and countrycode variable (8 for Slovenia)
  data_plots_harmonized <- data_plots_harmonized %>% 
    mutate(country = "SI",
           countrycode = 8)
  
  
  # plotcode variable (add country code before plotkey) and set it into a string
  data_plots_harmonized <- data_plots_harmonized %>% 
    mutate(plotcode = paste0(countrycode, "_", plotkey))
  
  
  # Change date variable as character
  data_plots_harmonized$surveydate1 <- as.Date(data_plots_harmonized$surveydate1, format = "%d-%m-%Y")
  data_plots_harmonized$surveydate2 <- as.Date(data_plots_harmonized$surveydate2, format = "%d-%m-%Y")
  
  
  
  # Convert coord from D48/GK national projection system to WGS84 projection system
  coordPlots_D48 <- terra::vect(data_plots_harmonized %>% 
                                  select(Xdispl, Ydispl), 
                                geom = c("Xdispl", "Ydispl"), crs = "EPSG:3787")
  coordPlots_WGS84 <- as.data.frame(terra::geom(terra::project(coordPlots_D48, "EPSG:4326")))
  
  data_plots_harmonized$longitude <- coordPlots_WGS84$x
  data_plots_harmonized$latitude <- coordPlots_WGS84$y
  
  # OLD VERSION WITH SP
  # coordPlots_D48GK <- data_plots_harmonized %>% select(Xdispl, Ydispl)
  # coordinates(coordPlots_D48GK) <- c("Xdispl", "Ydispl")
  # 
  # proj4string(coordPlots_D48GK) <- CRS("+init=epsg:3787") #https://epsg.io/3787 : corresponding EPSG for D48/GK
  # coordPlots_WGS84 <- spTransform(coordPlots_D48GK,  CRS("+init=epsg:4326")) # WGS84
  # 
  # data_plots_harmonized$longitude <- coordinates(coordPlots_WGS84)[,"Xdispl"]
  # data_plots_harmonized$latitude <- coordinates(coordPlots_WGS84)[,"Ydispl"]
  # 
  
  # Remove plotcodes in data_plots not in data_trees
  data_plots_harmonized <- data_plots_harmonized %>% filter(plotcode %in% unique(data_trees_harmonized$plotcode))
  
  
  
  ##################################
  ###### SELECT VARIABLES ##########
  ##################################
  
  
  # Select columns of data trees according to FunDivEUROPE conventions
  data_trees_harmonized <- data_trees_harmonized %>% 
    select(treecode, plotcode,
           species = Latinname, treestatus, 
           dbh1, dbh2,
           weight1, weight2
           )
  
  # Select columns of data plots according to FunDivEUROPE conventions
  data_plots_harmonized <- data_plots_harmonized %>% 
    select(plotcode, country,
           longitude, latitude, altitude = elv,
           surveydate1, surveydate2)
  
  
  ##################################
  ######## CLEAN DATASETS ##########
  ##################################
  
  # Remove trees with dbh1 = 0 and treestatus = 3 or 4
  data_trees_harmonized <- data_trees_harmonized %>% 
    dplyr::filter(!(dbh1 == 0 & treestatus %in% c(3,4)))
  
  data_plots_harmonized <- data_plots_harmonized %>% 
    dplyr::filter(plotcode %in% unique(data_trees_harmonized$plotcode))
  
  
  
  ##### RETURN DATASETS AS A LIST #####
  return(list(trees = data_trees_harmonized, plots = data_plots_harmonized))
}