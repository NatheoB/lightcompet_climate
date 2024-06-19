Merge_Radiation_Datasets <- function(df_list) {
  
    # Find null element in the list (must use a for loop because can't allocate vector of size nanana with sapply)
  null_ids <- c()
  for (i in 1:length(df_list)) {
    if (length(df_list[[i]]) == 0) {
      null_ids <- c(null_ids, i)
    }
  }
  
    # Remove NULL elements
  print(paste("Number removed NULL plots", length(null_ids)))
  df_list <- df_list[-null_ids]
  
    # Check number of columns (if different from 385, i.e. data until 2020)
  no385cols_ids <- c()
  for (i in 1:length(df_list)) {
    if (ncol(df_list[[i]]) != 385) {
      no385cols_ids <- c(no385cols_ids, i)
    }
  }
  
    # Remove plots without the same number of columns (i.e. plots without data after 2016)
  print(paste("Number removed no data after 2016 plots", length(no385cols_ids)))
  df_list <- df_list[-no385cols_ids]
  
    # Bind all dataframes by rows
  data_radiation <- do.call("rbind", df_list)
  
  return(data_radiation)
}