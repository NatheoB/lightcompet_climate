Merge_SoilGrids <- function(sg_vars_depths_values) {
  
    # Select dataframe from lists of the given tile
  sg_merged <- lapply(sg_vars_depths_values, function(X) X$data)
  
    # Bind dfs into a single one with all vars and depths
  sg_merged <- sg_merged %>% 
    purrr::reduce(full_join, by = "plotcode")
  
    # Return result as a list for each var and depth
  return(sg_merged)
}