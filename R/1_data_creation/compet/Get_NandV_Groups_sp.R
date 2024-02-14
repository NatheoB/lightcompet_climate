Get_NandV_Groups_sp <- function(data_NandV, species_vect,
                                shadetol_limits) {
  
  data.frame(species = species_vect) %>% 
    dplyr::left_join(data_NandV %>% dplyr::select(Species, Gymnosperm, shadetol = shade_tolerance.mean),
                     by = c("species" = "Species")) %>% 
    dplyr::mutate(
      shadetol_group = case_when(
        
        shadetol <= shadetol_limits[1] ~ "l",
        shadetol > shadetol_limits[1] & shadetol < shadetol_limits[2] ~ "m",
        shadetol >= shadetol_limits[2] ~ "h"
        
      ),
      missing_shadetol = is.na(shadetol_group),
      order = ifelse(Gymnosperm == "Y", "g", "a"),
      remarks_shadetol = ""
    ) %>% 
  
    dplyr::select(species, order, shadetol = shadetol_group, missing_shadetol, remarks_shadetol)
}