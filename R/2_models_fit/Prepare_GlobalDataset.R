#' @description Prepare global dataset before growth and mortality dataset creation 
#' (select variables, compute derived variables and clean dataset)
#' 
#' @param dataset_global data.frame - Global Samsara calibration dataset (merged raw datasets)
#' 
#' @return data.frame - Prepared global Samsara calibration dataset
#' 
Prepare_GlobalDataset <- function(dataset_global) {
  
  # Select variables ----
  dataset_selected <- Select_Variables_GlobalDataset(dataset_global)

  # Compute derived variables ----
  dataset_selected_derived <- Compute_DerivedVariables_GlobalDataset(dataset_selected)

  # Clean dataset ----
  dataset_selected_derived_cleaned <- Clean_GlobalDataset(dataset_selected_derived)
  
  # Reduce climate variables with PCA ----
  dataset_selected_derived_cleaned_pca <- Reduce_ClimateVars(dataset_selected_derived_cleaned)
  
  
  dataset_selected_derived_cleaned_pca
}