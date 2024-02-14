Filter_and_Harmonize_Datasets_Slovakia <- function(raw_data) {
  
  # Select variables
  data <- raw_data %>% 
    dplyr::select(plotcode = IDPlots, treecode = IDtree,
                  longitude = WGS_EE, latitude = WGS_NN,
                  surveydate1 = Date1, surveydate2 = Date2,
                  dbh1 = DBH1mm, dbh2 = DBH2mm, weight = FactorArea,
                  treetatus1 = Status1, treetatus2 = Status2,
                  harvest_reason = `Harvest between 1 and 2`,
                  species = Name)
  
  # # Observe status
  # table(data$treetatus1, data$treetatus2, useNA = "always")
  
  # Remove trees dead in the first survey
  data <- data %>% 
    dplyr::filter(treetatus1 %in% c(NA, "Live tree"))
  
  # # Observe status
  # table(data$treetatus1, data$treetatus2, useNA = "always")
  
  # Remove data absent in first survey and dead or harvested in the second one
  data_na1_harvested2 <- data %>% 
    dplyr::filter(is.na(treetatus1) & treetatus2 == "Harvested tree")
  # table(data_na1_harvested2$harvest_reason, useNA = "always")
  
  data <- data %>% 
    dplyr::filter(!(is.na(treetatus1) & treetatus2 != "Ingrowth tree"))
  
  # # Observe status
  # table(data$treetatus1, data$treetatus2, useNA = "always")
  # table(data$treetatus2, data$harvest_reason, useNA = "always")
  
  # Set treestatus
  data <- data %>% 
    dplyr::mutate(treestatus = case_when(
      
      treetatus2 == "Ingrowth tree" ~ 1, # Ingrowth data
      treetatus2 == "Live tree" ~ 2, # Living data
      treetatus2 == "Harvested tree" & harvest_reason %in% c("Non clearly felling cause", "Planned felling", NA) ~ 3, # Harvested data
      treetatus2 == "Dead tree" | (treetatus2 == "Harvested tree" & harvest_reason == "Salvage felling") ~ 4
      
    ))
  
  # # Observe datatatus
  # table(data$treestatus) # OK
  # 
  # 
  # # CHECK : species column
  # sort(table(data$species), decreasing = T) # OK
  # 
  # 
  # # CHECK : DBH = 0
  # nrow(data %>% dplyr::filter(dbh1 == 0)) # Nothing
  # nrow(data %>% dplyr::filter(dbh2 == 0)) # Nothing
  # 
  # 
  # # CHECK : DBH is NA
  # table(data %>% dplyr::filter(is.na(dbh1)) %>% dplyr::pull(treestatus)) # Only ingrowth species : OK
  # table(data %>% dplyr::filter(is.na(dbh2)) %>% dplyr::pull(treestatus)) # Nothing
  
  # Set NA dbh and corresponding weight to 0
  data <- data %>% 
    dplyr::mutate(
      weight1 = case_when(treestatus == 1 ~ 0,
                          TRUE ~ weight),
      dbh1 = case_when(treestatus == 1 ~ 0,
                       TRUE ~ dbh1),
      weight2 = case_when(treestatus %in% c(3, 4, 5) ~ 0,
                          TRUE ~ weight),
      dbh2 = case_when(treestatus %in% c(3, 4, 5) ~ 0,
                       TRUE ~ dbh2)
    )
  
  # Weights are attributed using the diameter of the revisit
  # Thus, but some trees are above 12cm and have a weight of trees below 12cm because below when the first visit
  # Some trees are living in the fort visit but dead at the second, thus dbh is set at 70mm and thus weight is biased

  # Create treecode and country_code
  data <- data %>% 
    dplyr::mutate(plotcode = paste0("9_", plotcode),
                  treecode = paste0(plotcode, "_", treecode),
                  country = "SK",
                  altitude = NA)
  
  # Separate into plots and data dataset
  trees <- data %>% 
    dplyr::select(treecode, plotcode, species,
                  treestatus, dbh1, dbh2,
                  weight1, weight2)
  
  plots <- data %>% 
    dplyr::select(plotcode,
                  country, longitude, latitude, altitude,
                  surveydate1, surveydate2) %>% 
    dplyr::distinct()
  
  # # Observe missing date data
  # sum(is.na(plots$surveydate1)) # 31 plots
  # sum(is.na(plots$surveydate2)) # 0 plot
  
  trees_no_surveydate1 <- trees %>% 
    dplyr::filter(plotcode %in% plots$plotcode[is.na(plots$surveydate1)])
  
  # nrow(trees_no_surveydate1) #351 trees
  # nrow(trees_no_surveydate1 %>% tidyr::drop_na(dbh1)) # 0 plot
  # # All plots with no surveydate 1 are plots without dbh1 (no survey of this tree)
  # # That's not ingrowth trees but newly surveyed trees ==> remove those plots
  
  # Remove plots and trees with no surveydate1 (only one visit)
  trees <- trees %>% dplyr::filter(!plotcode %in% unique(trees_no_surveydate1$plotcode))
  plots <- plots %>% dplyr::filter(!plotcode %in% unique(trees_no_surveydate1$plotcode))
  
  # sum(is.na(plots$surveydate1)) # 0 plot
  
  # Change date variable as character
  plots$surveydate1 <- as.Date(plots$surveydate1, format = "%Y-%m-%d")
  plots$surveydate2 <- as.Date(plots$surveydate2, format = "%Y-%m-%d")
  
  
  # Return plots and trees datasets
  return(list(trees = trees, plots = plots))
}