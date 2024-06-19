Filter_Datasets_Poland <- function(data_poland_list)
{
  data_trees <- data_poland_list$trees
  data_plots <- data_poland_list$plots
  
  ### REMOVE CYCLE 1
  data_trees <- data_trees %>% filter(nfi_cycle != 1)
  
  
  ### ADD LACKING TREE IDS WIN EACH CYCLE : TRACE WITH CHANGE_CODE = 99 (ADDING ROW)
  
  # Get ids of trees in each cycle
  trees_nfi2 <- unique(data_trees %>% filter(nfi_cycle == 2) %>% pull(id_tree))
  trees_nfi3 <- unique(data_trees %>% filter(nfi_cycle == 3) %>% pull(id_tree))
  
  # Get ids of trees in one cycle but not in the other
  trees_in2_not3 <- setdiff(trees_nfi2, trees_nfi3)
  length(trees_in2_not3) #24438
  trees_in3_not2 <- setdiff(trees_nfi3, trees_nfi2)
  length(trees_in3_not2) #230790
  
  # Add lacking trees in the corresponding nfi
  add_trees_in2_not3 <- data_trees %>% filter(nfi_cycle == 2 & id_tree %in% trees_in2_not3) %>% 
    mutate(nfi_cycle = 3, nfi_years = "2015-2019",
           species = NA, age = age+5, dbh = NA, height = NA, volume = NA, 
           change_code = 99, change_description = "added row")
  
  add_trees_in3_not2 <- data_trees %>% filter(nfi_cycle == 3 & id_tree %in% trees_in3_not2) %>% 
    mutate(nfi_cycle = 2, nfi_years = "2010-2014",
           species = NA, age = age-5, dbh = NA, height = NA, volume = NA, 
           change_code = 99, change_description = "added row")
  
  # Bind added rows to original dataset
  data_trees <- rbind(data_trees, add_trees_in2_not3, add_trees_in3_not2)
  
  # Check if missing tree
  table(data_trees %>% group_by(id_tree) %>% summarize(N = n()) %>% pull(N))
  
  
  ### SELECT INTEREST VARIABLES
  data_trees <- data_trees %>% 
    select(id_plot, id_tree, nfi_cycle, species, dbh, change_code)
  
  
  ### WIDER THE DATASET (ONE ROW FOR EACH TREE)
  data_trees <- data_trees %>% 
    pivot_wider(names_from = nfi_cycle,
                names_sep = "",
                values_from = c(species, dbh, change_code))
  
  
  
  ### CHECK CROSS-MODALITIES TABLE FOR CHANGE_CODE AT CYCLE 2 AND 3
  table(data_trees[,c("change_code2", "change_code3")], useNA = "always")
  
  
  
  ### CREATE TREESTATUS VARIABLES BASED ON CHANGE_CODE AT CYCLE 2 AND 3
  data_trees <- data_trees %>% 
    mutate(treestatus = case_when(
      is.na(change_code3) ~ "6", # No dbh for NA change code in cycle3 : only use them for light calculation
      change_code3 == 0 ~ "2", # no change : Survivor
      change_code3 == 1 ~ "1", # new tree (from growth) : Ingrowth
      change_code3 == 2 ~ "3", # tree removed (cut down): Dead (harvested)
      change_code3 == 3 ~ case_when( # dead tree
        is.na(change_code2) ~ "4", # Dead (stem present)
        change_code2 %in% c("0","1","5","7","9") ~ "4", # Dead (stem present)
        change_code2 == 99 ~ "RM_P" # remove plot if no tree in cycle 2
      ),
      change_code3 == 4 ~ "RM_T", # tree incorrectly included in the plot in the previous cycle : remove tree
      change_code3 == 5 ~ case_when(# Tree missed in the previous cycle :
        is.na(change_code2) ~ "6", # if NA modality at cycle 2 : use the tree only for light calculation
        change_code2 == 99 ~ "RM_P" # if no tree at cycle 2 : remove the plot
      ),
      change_code3 == 6 ~ "RM_T", # tree removed as a result of a change in the size of the plot : remove this tree and plot area : cycle3 area
      change_code3 == 7 ~ "RM_T", # new tree as a result of a change in the size of the plot : remove this tree and area is the cycle2 survey area
      change_code3 == 8 ~ "RM_P", # tree removed - plot was not found in the field : remove the corresponding plot
      change_code3 == 9 ~ "RM_P", # tree measured for the first time - newly established plot : no need of this plot
      change_code3 == 10 ~ "RM_T", # tree removed in the previous cycle : no need of this tree
      change_code3 == 99 ~ case_when( # tree not in the cycle3 survey dataset
        change_code2 == 1 ~ "6", # new tree (from growth) : in the cycle2 but not in the cycle3 : just use for light calculation,
        change_code2 == 8 ~ "RM_P", # tree removed - plot was not found in the field : remove the corresponding plot
        # 2 = tree removed (cut down) : dead (harvested) during cycle 1
        # 3 = dead tree : dead (stem present) during cycle 1
        # 4 = tree incorrectly included in the plot in the previous cycle during cycle 1
        # or 6 = tree removed as a result of a change in the size of the plot during cycle 1
        change_code2 %in% c(2,3,4,6) ~ "RM_T" # 
      )
    ))
  
  # Check if no NA in treestatus
  sum(is.na(data_trees$treestatus))
  
  
  
  ### REMOVE PLOTS OR TREES 
  
  # Remove corresponding plots : treestatus == "RM_P"
  remove_plots <- unique(data_trees %>% filter(treestatus == "RM_P") %>% pull(id_plot))
  length(remove_plots) # Remove 328 plots
  length(unique(data_trees$id_plot)) #19,019 plots before
  data_trees <- data_trees %>% filter(!(id_plot %in% remove_plots))
  length(unique(data_trees$id_plot)) #18,691 plots now
  
  
  # Remove "RM_T" trees
  nrow(data_trees %>% filter(treestatus == "RM_T")) #Remove 261,248 trees
  nrow(data_trees) #701,634 trees before
  data_trees <- data_trees %>% filter(treestatus != "RM_T")
  nrow(data_trees) #440,386 trees now
  
  
  ### SET TREESTATUS AS INTEGER VARIABLE
  data_trees$treestatus <- as.integer(data_trees$treestatus)
  
  
  ### SEARCH FOR INAPROPRIATED NA DBH
  
  ## Cycle 3 NA DBH :
  
  table(data_trees %>% filter(is.na(dbh3)) %>% pull(treestatus), useNA = "always")
  # 1     2     3     4     6  <NA> 
  # 12    30 51888 12197  1413     0
  
  # No problem for treestatus =  3, 4 (dead) or 6 (used for light calculation because no dbh at cycle3)
  
  # Set trees with na dbh at cycle3 and treestatus within 1 or 2 at treestatus = 6 (used for light calculation only)
  data_trees <- data_trees %>% 
    mutate(treestatus = replace(treestatus, is.na(dbh3) & treestatus %in% c(1, 2), 6))
  
  # Check changes
  table(data_trees %>% filter(is.na(dbh3)) %>% pull(treestatus), useNA = "always")
  # 3     4     6  <NA> 
  # 51888 12197  1455     0  
  
  
  
  ## Cycle 2 NA DBH:
  
  table(data_trees %>% filter(is.na(dbh2)) %>% pull(treestatus), useNA = "always")
  # 1     3     4     6  <NA> 
  # 39409   248   205    16     0  
  
  # No problem for treestatus = 1 (ingrowth) but problem for 3 and 4 (dead) and 6 (for light calculation)
  table(data_trees %>% filter(is.na(dbh2) & treestatus %in% c(3, 4, 6)) %>% select(change_code2, change_code3), useNA = "always")
  
  
  # How many plots it concerns?
  temp_trees <- data_trees %>% filter(is.na(dbh2) & treestatus %in% c(3, 4, 6))
  length(unique(temp_trees$id_plot)) #396 plots
  
  # How many with NA dbh3 ?
  length(is.na(temp_trees$dbh3)) #469 trees
  
  # Remove the concerning trees
  data_trees <- data_trees %>% filter(!(is.na(dbh2) & treestatus %in% c(3, 4, 6)))
  
  # Check changes
  table(data_trees %>% filter(is.na(dbh2)) %>% pull(treestatus), useNA = "always")
  
  
  
  ### CHECK IF PLOTS IN DATA TREES ARE ALL IN DATA PLOTS
  
  plots_in_data_trees <- unique(data_trees$id_plot)
  plots_in_data_plots <- unique(data_plots$id_plot)
  
  setdiff(plots_in_data_trees, plots_in_data_plots) # All plots in data_trees are in data_plots
  
  
  ### FILTER DATA_PLOTS
  
  # Get only plots in data_trees for nfi_cycle 2 and 3
  data_plots <- data_plots %>% filter(id_plot %in% plots_in_data_trees & nfi_cycle != 1)
  
  # Change plot area according to the smaller area between nfi_cycle 2 and 3
  data_plots <- data_plots %>% group_by(id_plot) %>% mutate(smaller_area = min(area))
  
  # Get one row per plot
  data_plots <- data_plots %>% 
    select(id_plot, nfi_cycle, year, latitude, longitude, elevation = elavation, smaller_area) %>% 
    pivot_wider(names_from = nfi_cycle,
                names_prefix = "year",
                values_from = year)
  
  # Check if one row per plot
  print(table(table(data_plots$id_plot)))
  which(table(data_plots$id_plot)==2) # id_plot = 16150105 : wrong wider : still two rows
  row_to_add <- data_plots %>% 
    filter(id_plot == "16150105")
  row_to_add[1, "year3"] <- row_to_add[2, "year3"]
  data_plots <- data_plots %>% filter(id_plot != "16150105")
  data_plots <- rbind(data_plots, row_to_add[1,])
  which(table(data_plots$id_plot)==2) # OK : one row per id_plot
  
  ### CHECK SPECIES
  
  # Which treestatus have species NA at cycle 2 ?
  table(data_trees %>% filter(is.na(species2)) %>% pull(treestatus), useNA = "always") # Only 1 so only ingrowth (NIIIICE !)
  
  # Which treestatus have species NA at cycle 3 ?
  table(data_trees %>% filter(is.na(species3)) %>% pull(treestatus), useNA = "always") # One tree used for light calculation but its ok because no dbh at cycle 3
  
  # Set the correct species
  data_trees <- data_trees %>% mutate(latinname = case_when(
    treestatus == 1 ~ species3,
    TRUE ~ species2
  ))
  
  # Check if no NA species
  nrow(data_trees %>% filter(is.na(latinname))) # 0 tree
  
  
  ### RETURN DATASETS AS A LIST ###
  return(list(trees = data_trees, plots = data_plots))
}
