# Compute_BAL_tree <- function(row_id, data_trees_plot) {
# 
#   # Get G_ha of given tree
#   dbh1_tree <- data_trees_plot$dbh1[row_id]
# 
#   # Computer BAL (sum of G_ha of trees with higher dbh1 than the given tree)
#   data_trees_plot %>% 
#     dplyr::mutate(bigger = dbh1 > dbh1_tree) %>% 
#     dplyr::mutate(compet_ba = ifelse(bigger, G1_ha, 0)) %>% 
#     dplyr::summarize(bal_m2 = sum(compet_ba)) %>% 
#     dplyr::pull(bal_m2)
# }
# 
# 
# Compute_BAL_plot <- function(row_id, data_trees, plotcodes) {
# 
#   print(row_id)
#   
#   # Get all trees within the plot
#   data_trees_plot <- data_trees %>% filter(plotcode == plotcodes[row_id])
#   
#   # Compute BAL of each tree within the plot
#   sapply(1:nrow(data_trees_plot), Compute_BAL_tree, data_trees_plot)
# }


# # Original dataset
# n_plots <- 1000
# n_trees <- n_plots*200
# data_trees <- data.frame(plotcode = sample(1:n_plots, n_trees, replace = T),
#                          dbh_cm = pmax(1, rnorm(n_trees, 15, 10))) %>% 
#   dplyr::mutate(ba = pi*(dbh_cm/100)^2/4) %>% 
#   dplyr::group_by(plotcode) %>% 
#   dplyr::mutate(treecode = row_number())


# Methode Julien Barrere (don't work if trees have the same dbh)
# compute_bal_JB <- function(data_trees) {
#   
#   data_trees %>%
#     left_join((data_trees %>%
#                  group_by(plotcode) %>%
#                  mutate(rank.ba = paste0("rank", rank(ba))) %>%
#                  dplyr::select(-treecode) %>%
#                  spread(key = "rank.ba", value = "ba")), by = "plotcode") %>%
#     gather(key = "rank.ba", value = "ba.comp", colnames(.)[grep("rank", colnames(.))]) %>%
#     filter(ba.comp >= ba) %>%
#     group_by(plotcode, treecode, ba) %>%
#     summarize(ba.larger = sum(ba.comp, na.rm = TRUE)) %>%
#     mutate(ba.larger = ba.larger - ba)
#   
# }

# Methode par Laura Touzot
Compute_BAL_LT <- function(data_trees) {
  
  data_trees <- data_trees %>% 
    group_by(plotcode) %>% 
    arrange(desc(G1_ha), .by_group = TRUE)
  
  data_trees$csum <- ave(data_trees$G1_ha, 
                         data_trees$plotcode, 
                         FUN = cumsum)
  data_trees$bal <- data_trees$csum - data_trees$G1_ha
  
  data_trees %>% 
    dplyr::select(plotcode, treecode, bal)
}


Compute_BACompetition_Variables <- function(data_trees) {
  
  # Compute basal area per hectare of each tree
  data_trees <- data_trees %>% 
    mutate(G1_ha = (pi * (dbh1/1000)^2 / 4)*weight1)  #in m2/ha
  
  
  print("Computing total basal area...")
  # Compute BAtot (total basal area of the stand)
  data_trees <- data_trees %>% 
    dplyr::group_by(plotcode) %>% 
    dplyr::mutate(batot_m2 = sum(G1_ha)) %>% 
    dplyr::ungroup()
  
  
  print("Computing basal area of larger trees...")
  # Compute BAL (basal area of bigger than me) 
  data_bal <- Compute_BAL_LT(data_trees)
  data_trees <- dplyr::left_join(data_trees,
                                 data_bal,
                                 by = c("plotcode", "treecode"))
  
  # Return only competition variables
  data_trees %>% 
    dplyr::select(treecode, batot_m2, bal_m2 = bal)
}

