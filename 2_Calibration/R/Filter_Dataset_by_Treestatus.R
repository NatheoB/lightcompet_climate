#' @description Filter dataset by keeping only given treestatus
#' 
#' @param dataset data.frame - Calibration dataset with a "treestatus" column
#' @param treestatus_to_keep integer vector - Tree status one want to keep
#' 
#' @return data.frame - Filtered dataset
#' 
Filter_Dataset_by_Treestatus <- function(dataset, treestatus_to_keep) {
  
  dataset %>% 
    dplyr::filter(treestatus %in% treestatus_to_keep)
  
}