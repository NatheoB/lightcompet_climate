#' @description Create final dataset for Samsara growth calibration (filter species and clean variables)
#' 
#' @param dataset_global data.frame - Global Samsara calibration dataset (merged raw datasets)
#' 
#' @return data.frame - Final growth dataset
#'
Prepare_GrowthDataset <- function(dataset_global,
                                  nplots_min = 500,
                                  nliving_min = 1000,
                                  nliving_min_per_country = 100,
                                  sp_to_remove) {
  
  # Filter by treestatus ----
  data_growth <- Filter_Dataset_by_Treestatus(dataset_global, 2)
  
  # Remove country with not enough living individuals for each species ----
  data_growth <- Filter_Dataset_by_SpeciesXCountries(data_growth,
                                                     nliving_min_per_country = 100,
                                                     ndead_min_per_country = 0)
  
  # Filter by species ----
  sp_occurrences <- Get_SpeciesOccurrence(data_growth)
  
  species_calib <- Get_Species_by_Occ(sp_occurrences, 
                                      nplots_min = nplots_min,
                                      nliving_min = nliving_min,
                                      ndead_min = 0)
  species_calib <- species_calib[!species_calib %in% sp_to_remove]

  data_growth <- Filter_Dataset_by_Species(data_growth, species_calib)
  
  
  # Select and rename interesting variables ----
  data_growth %>% 
    dplyr::select(
      
      treecode,
      
      dD = dD_mm_year,
      dD_log = dD_mm_year_log,
      
      plotcode,
      longitude,
      latitude,
      country,
      
      species,
      
      dbh = dbh_mm,
      dbh_log = dbh_mm_log,
      
      lci,
      bat = batot_m2,
      bal = bal_m2,
      
      sgdd, sgdd_inv, sgdd2,
      aet2pet, aet2pet_inv, aet2pet2,
      climate
      
    )
}