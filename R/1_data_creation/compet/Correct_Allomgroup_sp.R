Correct_Allomgroup_sp <- function(speciesinfo_alloms) {

    speciesinfo_alloms %>% 
      dplyr::mutate(across(paste0(c("height", "cdiameter", "cratio"), "_corresp"),
                    ~ifelse(is.na(.), paste0(shadetol, order), .)))

}