Reduce_ClimateVars <- function(dataset) {
  
  # Get climate dor each plot
  data.pca <- dataset %>% 
    dplyr::select(plotcode, sgdd, aet2pet) %>% 
    dplyr::distinct(plotcode, .keep_all = T)
  
  # Run PCA
  res.pca <- PCA(data.pca[,c("sgdd", "aet2pet")], scale.unit = T, ncp = 1, graph = FALSE)
  
  # Get individuals and value of the first dimension (eigen value = 87%)
  ind <- get_pca_ind(res.pca)
  data.pca$climate <- ind$coord[,1]
  
  # Bind new values to base dataset
  dataset <- dataset %>% 
    dplyr::left_join(data.pca %>% 
                       dplyr::select(plotcode, climate),
                     by = "plotcode")
  
  return(dataset)
}