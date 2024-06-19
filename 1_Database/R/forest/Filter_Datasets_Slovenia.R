Filter_Datasets_Slovenia <- function(data_slovenia_list)
{
  
  raw_data_trees_slovenia <- data_slovenia_list$trees
  raw_data_plots_slovenia <- data_slovenia_list$plots
  
  ### FILTER PLOTS ACCORDING TO CODE VARIABLE (remove newly established plots at survey 2)
  table(raw_data_plots_slovenia$code1, useNA = "always")
  table(raw_data_plots_slovenia$code2, useNA = "always")
  
  data_plots_filtered <- raw_data_plots_slovenia %>% filter(code2 != "1")
  data_trees_filtered <- raw_data_trees_slovenia %>% filter(plotkey %in% unique(data_plots_filtered$plotkey))

  
  ### REMOVE PLOTS WHERE NO VALUE FOR R1 AND R2 (size of plot)
  data_plots_filtered <- data_plots_filtered %>% filter(r1 != 0 & !is.na(r1) & r2 != 0 & !is.na(r2))
  data_trees_filtered <- data_trees_filtered %>% filter(plotkey %in% unique(data_plots_filtered$plotkey))
  
  
  
  ### FILER ACCORDING TO TREECODE VARIABLE
  print(table(data_trees_filtered$treecode, useNA = "always"))
  
    # TREECODE IS NA AND DBH1 != 0 ==> set to modlity 0 : survivor trees (no change)
    # TREECODE IS NA AND DBH1 = 0 ==> set to modality 8 : trees that overgrown 30 cm dbh and are located on the larger plot
  print(nrow(data_trees_filtered %>% filter(is.na(treecode) & dbh1 == 0))) #63,778
  print(nrow(data_trees_filtered %>% filter(is.na(treecode) & dbh1 == 0 & dbh2 >= 30))) #63,778 ==> only trees with dbh2 >= 30cm
  
  data_trees_filtered <- data_trees_filtered %>% mutate(
    treecode = case_when(
      is.na(treecode) ~ ifelse(dbh1 == 0, 8, 0),
      TRUE ~ as.numeric(as.character(treecode))
    )
  )
  
  print(table(data_trees_filtered$treecode, useNA = "always"))
  
  
  ### RETURN DATASETS AS A LIST ###
  return(list(trees = data_trees_filtered, plots = data_plots_filtered))
}

