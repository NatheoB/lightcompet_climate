Merge_NFIs_Datasets <- function(list_data_plots, list_data_trees) {
  
    # Merge datasets by rows
  data_plots <- dplyr::bind_rows(list_data_plots)
  data_trees <- dplyr::bind_rows(list_data_trees)
  
    # Return datasets as a list
  return(list(plots = data_plots, trees = data_trees))
}
