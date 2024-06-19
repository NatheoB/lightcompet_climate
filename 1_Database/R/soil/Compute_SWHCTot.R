Compute_SWHCTot <- function(data_soil, depths) {
  
  data_merged <- dplyr::left_join(data_swhcProp, data_rootingdepths, by = "plotcode")
  
  # For each soil layer
  for (depth_str in depths) {
    
    # Get min and max depth as integer vect from string (e.g. "0-5cm" --> [0,5])
    depths_int <- strsplit(depth_str, "-")[[1]]
    depths_int[[2]] <- stringr::str_sub(depths_int[[2]], end = -3)
    depths_int <- as.integer(depths_int)
    
    # Compute sg layer size
    layer_size_cm <- depths_int[2] - depths_int[1]
    
    # Compute proportion of the considered soilgrids layer covered by root 
    prop_with_roots <- sapply( sapply( (data_rootingdepths$rooting_depth - depths_int[1]) / (depths_int[2] - depths_int[1]), max, 0), min, 1)
    
    # Compute total soil water content at filed capacity and at wilting point in mm
    data_merged[[paste0("swcFCMm_", depth_str)]] <- 
      data_merged[[paste0("swcFCProp_", depth_str)]] * layer_size_cm * prop_with_roots * 10
    
    data_merged[[paste0("swcWPMm_", depth_str)]] <- 
      data_merged[[paste0("swcWPProp_", depth_str)]] * layer_size_cm * prop_with_roots * 10
    
    
    # Compute total swhc in mm
    data_merged[[paste0("swhcMm_", depth_str)]] <- 
      data_merged[[paste0("swhcProp_", depth_str)]] * layer_size_cm * prop_with_roots * 10
     
  }
  
  # Add total swhc and swc over all soil layers
  swhc_tot <- rowSums(data_merged %>% dplyr::select(contains("swhcMm_")))
  swcFC_tot <- rowSums(data_merged %>% dplyr::select(contains("swcFCMm_")))
  swcWP_tot <- rowSums(data_merged %>% dplyr::select(contains("swcWPMm_")))
    
  bind_cols(data_merged, swhcTot = swhc_tot, swcFCTot = swcFC_tot, swcWPTot = swcWP_tot)
}
