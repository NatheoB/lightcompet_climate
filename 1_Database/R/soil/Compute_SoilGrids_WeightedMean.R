Compute_SoilGrids_WeightedMean <- function(data_sg, vars) {
    
    # For each variable
  data_means <- sapply(vars, function(X){
    
      # Get columns for mean and uncertainty of the given variable
    data_sg.var <- data_sg[grepl(X, colnames(data_sg))]

      # Get means and inverse of uncertainties (the more the uncertainty is, the less weighted the mean will be within the weighted mean) for the given variable
    data_sg.var.mean <- data_sg.var[,grepl("mean", colnames(data_sg.var))]
    data_sg.var.uncertainties <- 1/data_sg.var[,grepl("uncertainty", colnames(data_sg.var))]

      # Compute mean weighted by uncertainty layer
    rowSums(data_sg.var.mean*data_sg.var.uncertainties) / rowSums(data_sg.var.uncertainties)

  }, simplify = FALSE, USE.NAMES = TRUE)
  
  
    # Return a dataframe with all weighted means for all variables
  data.frame(plotcode = data_sg$plotcode, do.call("cbind", data_means))
}