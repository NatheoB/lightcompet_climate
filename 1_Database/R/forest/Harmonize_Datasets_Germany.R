Harmonize_Datasets_Germany <- function(data_germany) {
  
  data_plots <- data_germany$plots
  data_trees <- data_germany$trees
  
  # Add country code before plotcode  + change country name from Germany to DE
  data_plots <- data_plots %>% 
    mutate(plotcode = paste0("3_", gsub("\\.", "_", plotcode)),
           country = "DE")
  
  # Set altitude variable as numerical avriable and yearsbetweensurveys as integer
  data_plots$altitude <- as.numeric(data_plots$altitude)
  data_plots$yearsbetweensurveys <- as.integer(data_plots$yearsbetweensurveys)
  
  
  ### If one surveydate is NA, we can retrieve it with yearsbetweensurveys (btw, always 5)
  
  # Get date columns  
  lt_surveydate1 <- as.POSIXlt(data_plots$surveydate1)
  lt_surveydate2 <- as.POSIXlt(data_plots$surveydate2)
  yearsbetweensurveys <- data_plots$yearsbetweensurveys
  
  # Substract yearsbetweensurvey to surveydate2 when NA surveydate1
  lt_surveydate1[which(is.na(lt_surveydate1))] <- lt_surveydate2[which(is.na(lt_surveydate1))] - yearsbetweensurveys[which(is.na(lt_surveydate1))]
  
  # Add yearsbetweensurvey to surveydate1 when NA surveydate2
  lt_surveydate2[which(is.na(lt_surveydate2))] <- lt_surveydate1[which(is.na(lt_surveydate2))] + yearsbetweensurveys[which(is.na(lt_surveydate2))]
  
  # Update columns
  data_plots$surveydate1 <- as.Date(lt_surveydate1)
  data_plots$surveydate2 <- as.Date(lt_surveydate2)
  
  
  ### Select only interest variables
  data_plots <- data_plots %>% 
    select(plotcode, country,
           longitude, latitude, altitude,
           surveydate1, surveydate2)
  
  
  ##### TREES DATASET #####
  
  # Add country code before plotcode and treecode + change country name from Germany to DE
  data_trees <- data_trees %>% 
    mutate(plotcode = paste0("3_", gsub("\\.", "_", plotcode)),
           treecode = paste0("3_", gsub("\\.", "_", treecode)),
           country = "DE")
  
  # Set type of speciesid variable as character
  data_trees$species <- as.character(data_trees$species)
  
  # Treestatus
  data_trees$treestatus <- as.integer(data_trees$treestatus)
  
  # Correct 99 modality of treestatus for germany (99 are survivor trees)
  data_trees$treestatus[data_trees$treestatus == 99] = 2
  
  # DBH variable
  data_trees$dbh1 <- as.integer(data_trees$dbh1)
  data_trees$dbh2 <- as.integer(data_trees$dbh2)
  
  # Get only interest variables
  data_trees <- data_trees %>% 
    select(treecode, plotcode,
           species, treestatus,
           dbh1, dbh2, weight1, weight2)
  
  
  ### Remove the plot without the altitude
  data_plots <- data_plots %>% filter(!is.na(altitude))
  data_trees <- data_trees %>% filter(plotcode %in% unique(data_plots$plotcode))
  
  
  ##### RETURN DATASETS AS A LIST ##### 
  return(list(trees = data_trees, plots = data_plots))
}