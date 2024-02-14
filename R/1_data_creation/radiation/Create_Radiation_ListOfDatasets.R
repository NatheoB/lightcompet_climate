Check_PVGISstr_plot <- function(PVGISstr_plot) {
  return(!is.null(PVGISstr_plot) & grepl("\tJan\t", PVGISstr_plot))
}


Transform_PVGIS_str2df_plot <- function(PVGISstr_plot) {
    # Transform the string into a df
  PVGIS_df_plot <- read.table(text=PVGISstr_plot,
                              col.names = c("year", "month", "Hrad", "ratio"))
  
    # Set the month and year modalities as variables
  PVGIS_df_plot <- PVGIS_df_plot %>% 
    pivot_wider(names_from = c(year, month),
                names_sep = "_",
                values_from = c(Hrad, ratio))
  
  return(PVGIS_df_plot)
}


Create_Radiation_ListOfDatasets <- function(data_PVGIS) {
  
  # Create a list of dataframes
  df_list <- lapply(data_PVGIS, function(X) {
    
      # If data has not been fetched successfully
    if (!Check_PVGISstr_plot(X[["data"]])) {
      print(paste("cannot load data for", X[["plotcode"]]))
      return(NULL)
    }
    
      # Transform PVGIS string data into dataframe
    pvgis_tmp <- tryCatch(expr = {
      Transform_PVGIS_str2df_plot(X[["data"]])
    }, error = function(e) {
      return(NULL)
    })
    
      # If data has not been transformed successfully
    if (is.null(pvgis_tmp)) {
      print(paste("cannot transform data for", X[["plotcode"]]))
      return(NULL)
    }
    
      # Bind plotcode and radiation data 
    cbind(plotcode = X[["plotcode"]], pvgis_tmp)
  })

  return(df_list)
}