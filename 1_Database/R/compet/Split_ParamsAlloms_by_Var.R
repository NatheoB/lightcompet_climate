Split_ParamsAlloms_by_Var <- function(params_alloms_df) {
  
  alloms_list <- list()
  
  # Create a dataframe for each variable
  for (var in c("height", "cdiameter", "cratio")) {
    
    alloms_list[[var]] <- params_alloms_df %>% 
      dplyr::select(species, contains(var)) %>% 
      tidyr::drop_na(paste0(var, ".model"))
    
  }
  
  alloms_list
}