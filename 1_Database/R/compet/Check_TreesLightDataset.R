Check_TreesLightDataset <- function(data_NFIslight) {
  
  # Check if each tree has a unique samsalight id within a unique plotcode
  ngroup_id <- data_NFIslight$trees %>% 
    dplyr::group_by(plotcode, lilo_id) %>% 
    dplyr::mutate(ngroup_id = n()) %>% 
    dplyr::ungroup() %>% 
    dplyr::pull(ngroup_id)
  
  if (sum(ngroup_id != 1) > 0) stop("Not unique id")
  
  
  # Check NAs
  cols <- c("dbh1", "species", "lad", "order", "height", "cbh", "cr")
  for (col in cols) {
    
    na_rows <- is.na(data_NFIslight$trees[[col]])
    if (sum(na_rows) > 0) stop(paste("Variable", col, "has NA values"))
    
  }
  
  
  # Check 0 values
  cols <- c("height", "cbh", "cr")
  
  for (col in cols) {
    
    null_rows <- data_NFIslight$trees[[col]] == 0
    if (sum(null_rows) > 0) stop(paste("Variable", col, "has 0 values"))
    
  }
  
  return(TRUE)
}