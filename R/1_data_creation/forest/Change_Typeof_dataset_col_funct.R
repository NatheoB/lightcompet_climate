Change_Typeof_dataset_col_funct <- function(data_list, dataset_name, colname, funct_typeconvert) {
    # Get function from string
  f <- get(funct_typeconvert)
  
    # Apply function to given col of given dataset
  data_list[[dataset_name]][,colname] <- f(data_list[[dataset_name]][,colname])
  
  return(data_list)
}