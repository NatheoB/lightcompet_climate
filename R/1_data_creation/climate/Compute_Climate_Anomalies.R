Compute_Climate_Anomalies <- function(data_incrperiod,
                                      data_chelsaperiod) {
  
  # Get variables both computed in increment period and chelsa period
  vars_incrperiod <- gsub("_incrperiod", "", names(data_incrperiod))
  vars_incrperiod <- vars_incrperiod[vars_incrperiod!="plotcode"]
  
  vars_chelsaperiod <- gsub("_chelsaperiod", "", names(data_chelsaperiod))
  vars_chelsaperiod <- vars_chelsaperiod[vars_chelsaperiod!="plotcode"]
  
  vars <- intersect(vars_incrperiod, vars_chelsaperiod)

  # Bind chelsa and increment period values
  df_all <- dplyr::left_join(data_incrperiod, data_chelsaperiod, by = "plotcode")
  
  # Compute anomaly (difference between large timescale variable and increment period)
  for (var in vars) {
    print(var)
    
    df_all[paste0(var, "_anom")] <- 
      df_all[[paste0(var, "_incrperiod")]] - df_all[[paste0(var, "_chelsaperiod")]]
    
    df_all[paste0(var, "_anomratio")] <- 
      ifelse(
        df_all[[paste0(var, "_chelsaperiod")]] == 0,
        NA,
        df_all[[paste0(var, "_anom")]] / df_all[[paste0(var, "_chelsaperiod")]]
      )
    
  }
  
  df_all %>% 
    dplyr::select(plotcode, contains(c("_anom", "_anomratio")))
}