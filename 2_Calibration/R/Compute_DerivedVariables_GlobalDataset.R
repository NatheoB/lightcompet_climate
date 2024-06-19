#' @description Compute new variables for Samsara growth and mortality equations
#' 
#' @param dataset_global data.frame - Global Samsara calibration dataset (merged raw datasets) 
#'  
#' @return data.frame - Dataset with adding newly computed variables
#' 
Compute_DerivedVariables_GlobalDataset <- function(dataset_global) {
  
  dataset_global %>% 
    dplyr::mutate(
      
      # Years between surveys
      dyears = lubridate::year(surveydate2) - lubridate::year(surveydate1),
      
      # Is the tree dead ?
      alive = ifelse(treestatus == 2, 1, 0),
      dead = 1 - alive,
      
      # Growth (yearly diameter increment in mm)
      dD_mm = dbh2 - dbh_mm,
      dD_mm_year = dD_mm/dyears,
      dD_mm_year_log = ifelse(dD_mm > 0, log(dD_mm_year), NA),
      
      # Ontogeny
      dbh_mm_log = ifelse(dbh_mm > 0, log(dbh_mm), NA),
      
      # Climate (inverse and square of climatic variables)
      sgdd_inv = 1/sgdd,
      aet2pet_inv = 1/aet2pet,
      
      sgdd2 = sgdd^2,
      aet2pet2 = aet2pet^2
    )

}






