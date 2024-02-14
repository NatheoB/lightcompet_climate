Select_SoilVariables <- function(data_soil_mean, data_swhc_locDepDepth, data_swhc_fixedDepth) {
  
  # Get fixed rooting depth
  fixed_rooting_depth <- unique(data_swhc_fixedDepth %>% pull(rooting_depth))
  
  # Prepare swhc data (rename fixed depth, get rooting depth, SWHCProp per layer and total SWHC) and add data_soil mean
  data_swhc_fixedDepth %>% 
    dplyr::select(plotcode, swhcTot, swcFCTot, swcWPTot) %>% 
    dplyr::rename_at(vars(!plotcode), function(X){paste0(X, "_", fixed_rooting_depth, "cm")}) %>% 
    dplyr::left_join(data_swhc_locDepDepth %>%
                       dplyr::select(plotcode,
                                     rooting_depth, 
                                     contains("swhcProp_"), swhcTot, swcFCTot, swcWPTot),
                     by = "plotcode") %>% 
    dplyr::left_join(data_soil_mean, by = "plotcode")
  
}