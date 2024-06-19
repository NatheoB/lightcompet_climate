Run_SamsaraLight_for_Cells_plot <- function(weather_folderpath, inv_folderpath) {
  
  ### Initialize script for SamsaraLightLoader
  tree_dir <- createJavaObject("java.io.File", file.path(getwd(), inv_folderpath))
  weather_dir <- createJavaObject("java.io.File", file.path(getwd(), weather_folderpath))
  output_dir <- createJavaObject("java.io.File", file.path(getwd()))
  export_dir <- createJavaObject("java.io.File", isNullObject = TRUE)
  
  export <- createJavaObject("samsaralightloader.myscripts.LiloExportCellLight", 
                             tree_dir, weather_dir, output_dir, export_dir)
  
  
  ### Run the simulation
  export$doAnalysis() #takes time : it runs the simulation.
  
  
  ### Export results
  res_cells <- export$getResults()
  n_items_cells <- res_cells$size()
  indexes_cells <- as.integer(0:(n_items_cells-1))
  items_cells <- res_cells$get(indexes_cells)
  capsis.output_cells <- tibble(site=items_cells$site[indexes_cells+1],
                                # id = items_cells$id[indexes_cells+1],
                                e = items_cells$e[indexes_cells+1],
                                erel = items_cells$erel[indexes_cells+1]) %>% 
    dplyr::group_by(site) %>% 
    dplyr::summarise_if(is.numeric, list("mean" = mean, "sd" = sd, "min" = min, "max" = max))
  
  return(capsis.output_cells)
}


Run_SamsaraLight_for_Cells <- function(weathers_fp, inventories_fp,
                                       capsis_dir) {
  
  ### Install J4R and RCapsis
  # install.packages("docu/J4R_1.1.1-228.tar.gz", repos = NULL, type = "source")
  # install.packages("docu/RCapsis_0.0.1.tar.gz", repos = NULL, type = "source")
  
  ### Connect to Capsis
  path_capsis <- capsis_dir
  
  setCapsisPath(path=path_capsis)
  connectToCapsis()
  
  ### Set inventory fp and weather fp in the same order
  
  # Bind weather and inventories filepath
  inputs_folderpath <- weathers_fp %>% 
    dplyr::rename(weather = fp) %>% 
    dplyr::full_join(inventories_fp %>% 
                       dplyr::rename(inv = fp),
                     by = "plotcode")
  
  # Check if all plots have an inventory and weather files
  if (sum(is.na(inputs_folderpath$weather)) > 0 | sum(is.na(inputs_folderpath$inv)) > 0) {
    stop("All plots do not have an inventory AND weather files")
  }
  
  # Get directory from filepath
  inputs_folderpath <- inputs_folderpath %>% 
    dplyr::mutate(weather = dirname(weather),
                  inv = dirname(inv))
  
  ### Run simulations for each plot
  nplots <- nrow(inputs_folderpath)
  samsalight_outputs <- vector(mode = "list", length = nplots)
  
  for (i in seq_along(samsalight_outputs)) {
    
    print(paste(i, nplots, sep = "/"))
    
    samsalight_outputs[[i]] <- Run_SamsaraLight_for_Cells_plot(
      inputs_folderpath$weather[i], inputs_folderpath$inv[i]
    )
    
  }
  
  ### Shutdown java client
  shutdownClient()
  
  ### Return dataframe with all simu outputs
  samsalight_outputs %>% dplyr::bind_rows()
}