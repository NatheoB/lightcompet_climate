Find_LADgroup_sp <- function(species_shadetol) {
  
  species_shadetol <- species_shadetol %>% 
    dplyr::mutate(lad_group = case_when(
      shadetol %in% c("l", "m") ~ shadetol,
      shadetol == "h" ~ paste0(shadetol, order)
    ))
  
}