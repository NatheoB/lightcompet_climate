#' Scale a dataset given variables to scale
#' 
#' @param data Dataset to scale
#' @param vars_to_scale Variables to scale as a character vector
#'
#' @return a list with scaled data and attributes dataframe
#'
Scale_Dataset <- function(data, vars_to_scale) {
  
  
  ### Scale given covariables
  data <- mutate_at(data, vars_to_scale, scale)
  
  # Add scale and offset in another dataframe
  df_attr <- list()
  for (var in vars_to_scale) {
    
    # Store attriutes
    df_attr[[var]] <- c("scale" = attr(data[[var]], "scaled:scale"),
                        "center" = attr(data[[var]], "scaled:center"))
    
    # Remove attributes from dataset
    data[[var]] <- data[[var]][,1]
    
  }
  df_attr <- dplyr::bind_rows(df_attr, .id = "var")
  
  return(list("data" = data, "attr" = df_attr))
}