
#' @description Create final dataset for Samsara mortality calibration (filter species and clean variables)
#' 
#' @param dataset_global data.frame - Global Samsara calibration dataset (merged raw datasets)
#' 
#' @return data.frame - Final mortality dataset
#'
Prepare_MortalityDataset <- function(dataset_global,
                                     nplots_min = 500,
                                     nliving_min = 1000,
                                     ndead_min = 100,
                                     nliving_min_per_country = 100,
                                     ndead_min_per_country = 20,
                                     sp_to_remove) {
  
  # Filter by treestatus ----
  data_mortality <- Filter_Dataset_by_Treestatus(dataset_global, c(2, 4))
  
  
  # Remove country with not enough living individuals for each species ----
  data_mortality <- Filter_Dataset_by_SpeciesXCountries(data_mortality,
                                                        nliving_min_per_country = 100,
                                                        ndead_min_per_country = ndead_min_per_country)
  
  # Filter by species ----
  sp_occurrences <- Get_SpeciesOccurrence(data_mortality)
  
  species_calib <- Get_Species_by_Occ(sp_occurrences, 
                                      nplots_min = nplots_min,
                                      nliving_min = nliving_min,
                                      ndead_min = ndead_min)
  species_calib <- species_calib[!species_calib %in% sp_to_remove]
  
  data_mortality <- Filter_Dataset_by_Species(data_mortality, species_calib)
  
  
  # Select and rename interesting variables ----
  data_mortality %>% 
    dplyr::select(
      
      treecode,
      
      dead,

      plotcode,
      longitude, 
      latitude,
      country,
      
      dyears,
      
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