Compute_Trees_Allometries <- function(data_NFIs_allomParams, data_compet_ba) {
  
  # Compute allometries : height, crown diameter and crown ratio
  data_NFIs_allomParams$trees <- data_NFIs_allomParams$trees %>% 
    
    # Add competition variables
    dplyr::left_join(data_compet_ba, by = "treecode") %>% 
    
    # Get competition variable for each species for crown diameter and crown ratio
    dplyr::mutate(
      cdiameter.comp_value = case_when(
        cdiameter.model == "nocomp" ~ 0,
        cdiameter.model == "bat" ~ batot_m2,
        cdiameter.model == "bal" ~ bal_m2),
      cratio.comp_value = case_when(
        cratio.model == "nocomp" ~ 0,
        cratio.model == "bat" ~ batot_m2,
        cratio.model == "bal" ~ bal_m2)) %>% 
    
    # Replace competition parameters by 0 if NA value (i.e. nocomp allom)
    dplyr::mutate(cdiameter.comp = tidyr::replace_na(cdiameter.comp, 0),
                  cratio.comp = tidyr::replace_na(cratio.comp, 0)) %>% 
    
    # Compute alloms
    dplyr::mutate(
      
      height = 
        1.3 + height.b1 * ( 1 - exp( - height.b2 * (dbh1 / 10) ) ) ^ height.b3,
      
      cdiameter = 
        (cdiameter.a1 + cdiameter.comp * cdiameter.comp_value) * (dbh1 / 10) ^ cdiameter.a2,
      
      cratio_linear = 
        cratio.a1 + cratio.comp * cratio.comp_value,
      cratio = exp(cratio_linear) / ( 1 + exp(cratio_linear) )
      
    ) %>% 

    # Correct if negative values ==> set to 0.01
    dplyr::mutate(
      height = pmax(0.01, height),
      cdiameter = pmax(0.01, cdiameter),
      cratio = pmax(0.01, cratio)
    ) %>%
    
    # Compute crown base heights cbh and crown radius cr
    dplyr::mutate(
      cbh = (1 - cratio) * height,
      cr = cdiameter / 2
    ) %>% 
    
    dplyr::select(-c(batot_m2, bal_m2,
                     contains(c("height.", "cratio", "cdiameter"))))
  
  data_NFIs_allomParams
}
