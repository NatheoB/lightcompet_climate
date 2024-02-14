Filter_Datasets_France <- function(data_france_list) {
  
  raw_data_trees_france <- data_france_list$trees
  raw_data_plots_france <- data_france_list$plots
  
  ### FILTER BY NUMBER OF VISITS ##
  
  # Add Nvisits (number of visits in the same plot) and lastCampagne (date of the last campaign)
  data_stands_filtered <- raw_data_plots_france %>% 
    group_by(IDP) %>% 
    mutate(
      Nvisits = n(),
      lastCampagne = max(CAMPAGNE)
    )
  
  print(table(data_stands_filtered$Nvisits, useNA = "always"))  
  
  # Get only stands visited twice (for increment over bark)
  data_stands_filtered <- data_stands_filtered %>% 
    filter(Nvisits == 2)
  
  # Add firstVisit variable (TRUE if it is the first one, FALSE otherwise)
  data_stands_filtered <- data_stands_filtered %>% 
    mutate(firstVisit = !(CAMPAGNE == lastCampagne)) # First visit is not the lastCampagne 
  
  # Table of CAMPAGNE
  print(table(data_stands_filtered$CAMPAGNE, useNA = "always"))
  
  # Check if there still are two visits per plots
  print(table(table(data_stands_filtered$IDP))) # No plots with 1 row, 67439 plots with 2 rows
  
  
  
  ### FILTER TO REMOVE FIRST CAMPAIGN BECAUSE NO REMEASURENT OF C13 ###
  
  # First campaign is 2005 (first visit) and 2010 (second visit)
  data_stands_filtered <- data_stands_filtered %>% 
    mutate(firstCampaign = (CAMPAGNE == 2005 & firstVisit) | (CAMPAGNE == 2010 & !firstVisit))
  
  
  # How many of the first campaign
  print(table(data_stands_filtered$firstCampaign)) #11648 rows = 5824 (CAMPAGNE == 2005) * 2 (second row for second visit)
  
  
  # Remove the corresponding rows
  data_stands_filtered <- data_stands_filtered %>% 
    filter(!firstCampaign)
  
  
  
  ### FILTER BY VEGETATION TYPE ###
  
  table(data_stands_filtered$CSA, useNA = "always")
  
  # Check whether the stand is considered as a closed forest or not (peupleraie is not considered as a forest)
  data_stands_filtered <- data_stands_filtered %>% 
    mutate(isForest = CSA==1)
  
  # Check whether stand is a forest at both visits
  data_stands_filtered <- data_stands_filtered %>% 
    group_by(IDP) %>% 
    mutate(isForestBothVisits = (sum(isForest)==2)) # TRUE + TRUE at both visits
  
  # Get only forests (and not peupleraie)
  # BE CAREFUL : stands can be other than forest at first visit but can become a forest after
  # Just keep stands that are forests at both visits
  data_stands_filtered <- data_stands_filtered %>%
    filter(isForestBothVisits)
  
  # Check if there still are two visits per plots
  table(table(data_stands_filtered$IDP)) # No plots with 1 row, 56994 rows
  
  
  
  ### FILTER BY INTERCEPTION WITH FOREST EDGE ###
  
  table(data_stands_filtered$PLISI, useNA = "always")
  
  # For campaign after 2007
  table((data_stands_filtered %>% filter(CAMPAGNE > 2007))$PLISI, useNA = "always")
  
  # For campaign before 2007
  table((data_stands_filtered %>% filter(CAMPAGNE <= 2007))$PLISI, useNA = "always")
  
  # Which stands are PLISI == NA
  unique((data_stands_filtered %>% filter(is.na(PLISI)))$CSLISI) # CSLISI always = "" ==> no description on the type of edge ==> there is no interception with an edge
  
  # PLISI range in 1-2-3 in 2007 and before and from 0-1-2 in 2008 and after
  data_stands_filtered <- data_stands_filtered %>% 
    mutate(PLISI = case_when(
      CAMPAGNE > 2007  ~ ifelse(is.na(PLISI), 1, PLISI + 1), # Set modalities of before-2007 campaign to fit those of post-2007 
      CAMPAGNE <= 2007 ~ ifelse(is.na(PLISI), 1, PLISI + 0) # Keep the pre-2007 modalities
    ))
  
  # And now, what about the edge modalities
  table(data_stands_filtered$PLISI, useNA = "always") #1:no edge, 2:at least one interception, 3:road or river less than 20m
  
  # Check wether the stand is at forest edge or not 
  data_stands_filtered <- data_stands_filtered %>% 
    mutate(isNotAtEdge = (PLISI==1)) # TRUE is stand is not at edge, FALSE if it intercepts an edge
  
  # Check wether the stand still not at a forest edge or not between both visits
  data_stands_filtered <- data_stands_filtered %>% 
    group_by(IDP) %>% 
    mutate(isNotAtEdgeBothVisits = (sum(isNotAtEdge)==2)) # TRUE + TRUE at both visits
  
  # BE CAREFUL : stands can be at an edge at first visit but could not intercept one at the second visit
  # Just keep stands that are not at edge at both visits
  data_stands_filtered <- data_stands_filtered %>% 
    filter(isNotAtEdgeBothVisits)
  
  # Check if there still are two visits per plots
  table(table(data_stands_filtered$IDP)) # No plots with 1 row, 52838 with 2 rows (removed 65053 - 52838 = 12215 plots)
  
  
  
  ### FILTER THE TREE DATASET ACCORDING TO KEPT PLOTS ###
  
  # Which pltos are kept after filtering
  kept_plots <- unique(data_stands_filtered$IDP)
  
  
  # Filter tree dataset to keep only filtered plots
  data_trees_filtered <- raw_data_trees_france %>% 
    filter(IDP %in% kept_plots)
  
  
  # Which plots/campaign are present in the trees dataset ?
  plots_campaigns_tree_dataset <- unique(data_trees_filtered[,c("IDP","CAMPAGNE")])
  
  # Number of plots for each number of visits
  table(table(plots_campaigns_tree_dataset$IDP))
  
  
  # Get IDP of plots that are visited twice
  plots_visited_twice <- data_trees_filtered %>% 
    group_by(IDP) %>% 
    summarize(Nvisits = length(unique((CAMPAGNE)))) %>% 
    filter(Nvisits == 2) %>% 
    pull(IDP)
  
  length(plots_visited_twice)
  
  
  # Filter for keeping only plots visited twice in tree and stand datasets
  data_trees_filtered <- data_trees_filtered %>% 
    filter(IDP %in% plots_visited_twice)
  
  data_stands_filtered <- data_stands_filtered %>% 
    filter(IDP %in% plots_visited_twice)
  
  
  
  ### FILTER THE TREE DATASET FOR KEEPING ONLY STANDS WHERE TREES HAS BEES REMEASURED AT THE SECOND VISIT ###
  # There are some plots that are visited twice but there is no measurement of C13 at the second visit
  
  # Add first visit variable from data_stands
  data_trees_filtered <- data_trees_filtered %>% 
    left_join(data_stands_filtered %>% select(IDP, CAMPAGNE, firstVisit),
              by = c("IDP", "CAMPAGNE"))
  
  
  # Search for stands where there is no remeasurement of living trees at the second visit
  # i.e. stands where VEGET5 = 0 and C13 = NA or C13 == 0 for all trees
  data_trees_filtered <- data_trees_filtered %>% 
    mutate(
      isNotRemeasured = ifelse(!firstVisit & VEGET5 == 0, is.na(C13), NA)
      # If first visit : NA
      # If second visit :
      #   - VEGET != 0 (tree not alive) : NA
      #   - VEGET = 0 :
      #       * TRUE if not remeasured
      #       * FALSE is remeasured
    )
  
  
  # Get stands with not measured trees that should be measured
  stands_remeasured <- data_trees_filtered %>% 
    group_by(IDP) %>% 
    summarize(SumNotRemeasured = sum(isNotRemeasured, na.rm = TRUE)) %>% 
    mutate(standsRemeasured = (SumNotRemeasured==0))
  
  
  # How many stands has not been remeasured
  table(stands_remeasured$standsRemeasured) # 2937 not remeasured and 26165 remeasured
  
  
  # Get IDP of remeasured stands
  stands_remeasured <- stands_remeasured %>% filter(standsRemeasured) %>% pull(IDP)
  
  
  # Filter datasets with only remeasured stands
  data_trees_filtered <- data_trees_filtered %>% 
    filter(IDP %in% stands_remeasured)
  
  data_stands_filtered <- data_stands_filtered %>% 
    filter(IDP %in% stands_remeasured)
  
  
  
  
  ### FILTER THE TREE DATASET FOR KEEPING ONLY LIVING TREES AT FIRST VISIT ###
  
  # How many trees with VEGET != 0
  table(data_trees_filtered$VEGET, useNA = "always") # 17408 for 5, 1576 for A, 4563 for C, 6 for Z
  
  # Searching for IDP, A with VEGET != 0
  data_trees_filtered <- data_trees_filtered %>% 
    mutate(isVeget0 = ifelse(firstVisit, VEGET==0, NA)) %>% 
    group_by(IDP, A) %>% 
    mutate(firstVisitIsVeget0 = sum(isVeget0, na.rm = TRUE)==1) # TRUE + NA
  
  # What is the VEGET5 for those trees ?
  table(data_trees_filtered %>% filter(VEGET != 0) %>% pull(VEGET5), useNA = "always")
  
  # Remove trees that are not VEGET = 0 at first visit (keep only living trees at first visit)
  data_trees_filtered <- data_trees_filtered %>% 
    filter(firstVisitIsVeget0)
  
  # Filter data_stands according to stands in data_trees (in case of we removed all trees within a stand)
  new_stands <- unique(data_trees_filtered$IDP)
  data_stands_filtered <- data_stands_filtered %>% 
    filter(IDP %in% new_stands) # Before : 53694 rows, After : 53606
  
  
  
  ### REMOVE PLOTS WITHOUT ALTITUDE VARIABLE ###
  
  # How many plots ?
  stands_no_alt <- unique(data_stands_filtered %>% filter(is.na(altitude)) %>% pull(IDP))
  print(length(stands_no_alt)) # 0 plot : OK
  
  data_trees_filtered <- data_trees_filtered %>% filter(!(IDP %in% stands_no_alt))
  data_stands_filtered <- data_stands_filtered %>% filter(!(IDP %in% stands_no_alt))
  
  
  
  
  ##### RETURN DATASETS AS A LIST #####
  return(list(trees = data_trees_filtered, plots = data_stands_filtered))
}