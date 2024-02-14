Run_SamsaraLight_for_Trees_plot <- function(weather_folderpath, inv_folderpath, 
                                            export_folderpath) {
  
  ### Initialize script for SamsaraLightLoader
  tree_dir <- createJavaObject("java.io.File", file.path(getwd(), inv_folderpath))
  weather_dir <- createJavaObject("java.io.File", file.path(getwd(), weather_folderpath))
  output_dir <- createJavaObject("java.io.File", file.path(getwd()))
  export_dir <- createJavaObject("java.io.File", file.path(getwd(), export_folderpath))
  
  export <- createJavaObject("samsaralightloader.myscripts.LiloExportTreeLight", 
                             tree_dir, weather_dir, output_dir, export_dir)

  
  ### Run the simulation
  export$doAnalysis() #takes time : it runs the simulation.

  
  ### Export results
  res_trees <- export$getResults()
  n_items_trees <- res_trees$size()
  indexes_trees <- as.integer(0:(n_items_trees-1))
  items_trees <- res_trees$get(indexes_trees)
  capsis.output_trees<-tibble(site=items_trees$site[indexes_trees],
                              id = items_trees$id[indexes_trees],
                              e = items_trees$e[indexes_trees],
                              epot = items_trees$epot[indexes_trees])
  
  return(capsis.output_trees)
}


Run_SamsaraLight_for_Trees <- function(weathers_fp, inventories_fp,
                                       export_folderpath, capsis_dir) {
  
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
    
    samsalight_outputs[[i]] <- Run_SamsaraLight_for_Trees_plot(
      inputs_folderpath$weather[i], inputs_folderpath$inv[i],
      export_folderpath
    )
    
  }
  
  ### Shutdown java client
  shutdownClient()
  
  ### Return dataframe with all simu outputs
  samsalight_outputs %>% dplyr::bind_rows()
}