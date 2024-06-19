Filter_and_Harmonize_Datasets_CzechRep <- function(raw_trees, 
                                                   raw_plots1, raw_plots2, 
                                                   raw_species) {
  
  # Select plots in survey 1 and 2
  plots <- dplyr::full_join(
    raw_plots1 %>% 
      dplyr::select(plotcode = RowLabels, 
                    latitude = Latitude, longitude = Longitude,
                    altitude = Elevation,
                    surveydate1 = Year_month,
                    area_ha = Plot_area_ha),
    raw_plots2 %>% 
      dplyr::select(plotcode = RowLabels, surveydate2 = Year_month),
    by = "plotcode")
  
  # 
  # # CHECK : plots without come back and plots without first survey
  # nrow(plots %>% dplyr::filter(is.na(surveydate1))) # 11 plots
  # nrow(plots %>% dplyr::filter(is.na(surveydate2))) # 7 plots
  # nrow(plots %>% dplyr::filter(is.na(surveydate2) & is.na(surveydate1))) # 0 plots
  # 
  # # CHECK : Remove plots with NA date
  plots <- plots %>% tidyr::drop_na(surveydate1, surveydate2) # 597 - 18 = 579 plots
  
  # Select variables in data_trees
  trees <- raw_trees %>% 
    dplyr::select(plotcode = IDPLOTS, treecode = ID,
                  new_missing_tree = NEWORMISSINGTREE,
                  species_id = IDSPECIES,
                  area_subplot_ha_1 = AREASUBPLOT_HA_CT1, area_subplot_ha_2 = AREASUBPLOT_HA_CT2,
                  death_cause_1 = DEADTREE_CT1, death_cause_2 = DEADTREE_CT2,
                  dbh1 = DBH_MM_CT1, dbh2 = DBH_MM_CT2,
                  expansion_factor_1 = EXPANSIONFACTOR_CT1, expansion_factor_2 = EXPANSIONFACTOR_CT2)
  
  
  # Remove trees without plots info
  trees <- trees %>% 
    dplyr::filter(plotcode %in% plots$plotcode) #17974 trees --> 17140 trees
  
  
  # # Observe death_cause
  # table(trees$death_cause_1, useNA = "always")
  
  # Keep only living trees or NA (ingrowth) in the first survey
  trees <- trees %>% 
    dplyr::filter(death_cause_1 %in% c("living tree", NA))
  
  # # Observe status1 and status2
  # table(trees$death_cause_1, trees$death_cause_2, useNA = "always")
  
  # Remove NA trees at survey1 and dead trees at survey2
  trees <- trees %>% 
    dplyr::filter(!(is.na(death_cause_1) & death_cause_2 %in% c("dead tree from the past", "fresh dead tree")))
  
  # # Observe status1 and status2
  # table(trees$death_cause_1, trees$death_cause_2, useNA = "always")
  
  # Remove plots with added or omitted trees (11 trees, 4 plots)
  # table(trees$new_missing_tree, trees$death_cause_2, useNA = "always")
  
  plots_added_omitted_trees <- trees %>% 
    dplyr::mutate(added_tree = grepl("added", new_missing_tree) | new_missing_tree == "omitted") %>% 
    dplyr::group_by(plotcode) %>% 
    dplyr::mutate(remove_plot = sum(added_tree) > 0) %>% 
    dplyr::filter(remove_plot) %>% 
    dplyr::select(plotcode) %>% 
    dplyr::distinct() %>% 
    dplyr::pull(plotcode)
  trees <- trees %>% dplyr::filter(!plotcode %in% plots_added_omitted_trees)
  
  # table(trees$new_missing_tree, trees$death_cause_2, useNA = "always")
  
  # # Observe expansion factor and area of subplot
  # table(trees$expansion_factor_1, trees$expansion_factor_2, useNA = "always") # 
  # table(trees$expansion_factor_1, trees$area_subplot_ha_1, useNA = "always") # All NAs area are all NAs of expansion factor
  # 
  # table(trees$expansion_factor_1, trees$new_missing_tree, useNA = "always") # NAs are ingrowth trees (OK) and no change (not ok) ==> 7 trees
  # table(trees$expansion_factor_2, trees$new_missing_tree, useNA = "always") # NAs are missing trees (OK)
  
  # Remove plots without definition of area and expansion factor in first survey that are no change trees #7 plots for 7 trees (remove 190 trees)
  plots_without_area <- trees %>% 
    dplyr::mutate(nochange_na = is.na(expansion_factor_1) & new_missing_tree == "no change") %>% 
    dplyr::group_by(plotcode) %>% 
    dplyr::mutate(remove_plot = sum(nochange_na) > 0) %>% 
    dplyr::filter(remove_plot) %>% 
    dplyr::select(plotcode) %>% 
    dplyr::distinct() %>% 
    dplyr::pull(plotcode)
  trees <- trees %>% dplyr::filter(!plotcode %in% plots_without_area)
  
  # table(trees$expansion_factor_1, trees$new_missing_tree, useNA = "always") # NAs are ingrowth trees (OK) and no change (not ok) ==> 7 trees
  # table(trees$expansion_factor_2, trees$new_missing_tree, useNA = "always") # NAs are missing trees (OK)
  
  # Create weight column
  trees <- trees %>% 
    dplyr::mutate(weight1 = expansion_factor_1/area_subplot_ha_1,
                  weight2 = expansion_factor_2/area_subplot_ha_2) %>% 
    dplyr::mutate_at(c('weight1','weight2', 'dbh1', 'dbh2'), ~replace_na(.,0))
  
  # table(is.na(trees$weight1), trees$new_missing_tree, useNA = "always") #OK (NA weights are ingrowth trees)
  # table(is.na(trees$weight2), trees$new_missing_tree, useNA = "always") #OK (NA weights are missing trees)
  
  # Create treestatus column
  # table(trees$new_missing_tree, trees$death_cause_1, useNA = "always")
  # table(trees$new_missing_tree, trees$death_cause_2, useNA = "always")
  
  trees <- trees %>% 
    dplyr::mutate(treestatus = case_when(
      new_missing_tree == "ingrowth" ~ 1,
      new_missing_tree == "missing" ~ 5, # dead (stem absent)2
      new_missing_tree == "no change" ~ ifelse(death_cause_2 == "living tree", 2, 4) # Either survivor 2 or dead trees 4
    ))
  
  # table(trees$treestatus, useNA = "always")
  
  # BIND and Remove trees without species
  trees <- dplyr::left_join(trees, raw_species, by = c("species_id" = "ID"))
  trees <- trees %>% 
    dplyr::mutate(VALUE_LAT = case_when(
      VALUE_EN == "other conifers" ~ "conifer",
      VALUE_EN == "other hard broadleaves" ~ "broadleaf",
      VALUE_EN == "other not improved poplars" ~ "Populus",
      VALUE_EN == "other improved poplars" ~ "Populus",
      VALUE_EN == "other soft broadleaves" ~ "broadleaf",
      TRUE ~ VALUE_LAT
    ))
  trees$VALUE_LAT[trees$VALUE_LAT == "Fagus silvatica"] <- "Fagus sylvatica"
  # sort(table(trees$VALUE_LAT, useNA = "always"))
  
  # Change date variable as character
  plots$surveydate1 <- as.Date(plots$surveydate1, format = "%Y-%m-%d")
  plots$surveydate2 <- as.Date(plots$surveydate2, format = "%Y-%m-%d")
  
  # Arrange plots and trees datasets
  trees <- trees %>% 
    dplyr::mutate(plotcode = paste0("10_", plotcode),
                  treecode = paste(plotcode, treecode, sep="_")) %>% 
    dplyr::select(treecode, plotcode, 
                  treestatus, dbh1, dbh2,
                  weight1, weight2,
                  species = VALUE_LAT)
  
  plots <- plots %>% 
    dplyr::mutate(cluster = NA,
                  country = "CZ",
                  plotcode = paste0("10_", plotcode)) %>% 
    dplyr::select(plotcode, cluster,
                  country, longitude, latitude, altitude,
                  surveydate1, surveydate2) %>% 
    dplyr::filter(plotcode %in% unique(trees$plotcode))
  
  # There are some trees with the same treecode (get those trees and add a bis letter)
  duplicated_ids <- trees$treecode[-match(unique(trees$treecode), trees$treecode)]
  trees_duplicated <- trees %>% 
    dplyr::filter(treecode %in% duplicated_ids) %>% 
    dplyr::group_by(treecode) %>% 
    dplyr::mutate(treecode = paste0(treecode,
                                    letters[row_number()]))
  
  trees <- trees %>% 
    dplyr::filter(!treecode %in% duplicated_ids) %>% 
    dplyr::add_row(trees_duplicated)
  
  # nrow(trees)
  # length(unique(trees$treecode))
  
  # Return plots and trees datasets
  return(list(trees = trees, plots = plots))
}