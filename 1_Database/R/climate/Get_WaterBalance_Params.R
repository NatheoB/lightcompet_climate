Get_WaterBalance_Params <- function(names_params) {
  
  # Create a dataframe with True/False combinations of all parameters
  params <- do.call("expand.grid", lapply(names_params, function(X){c(TRUE, FALSE)}))
  names(params) <- names_params
  
  # Add id column = string with parameters used
  do.call("expand.grid", lapply(names_params, function(X){c(X, NA)})) %>% 
    tidyr::unite("id", everything()) %>% 
    dplyr::mutate(id = replace(id, id == "NA_NA_NA", "control")) %>% 
    dplyr::bind_cols(params)
  
}
  