#' @description Select variables that are needed for Samsara growth and mortality calibration
#' 
#' @param dataset_global data.frame - Global Samsara calibration dataset (merged raw datasets) 
#'  
#' @return data.frame - Dataset with selected variables
#'  
Select_Variables_GlobalDataset <- function(dataset_global) {
  
  dataset_global %>% 
    dplyr::select(
      
      treecode, plotcode, country,
      longitude, latitude,
      
      surveydate1, surveydate2,
      
      treestatus,
      species,
      
      dbh_mm = dbh1, dbh2,
      lci = lci_mean, bal_m2, batot_m2,
    
      sgdd = sgdd_incrperiod,
      aet2pet = aet2pet.PrModif_expAET_locDepDepth_incrperiod
  )
 
}
