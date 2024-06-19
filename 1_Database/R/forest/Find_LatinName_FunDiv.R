Find_LatinName_FunDiv <- function(data_species_fundiv) {
  
  # Paste genus and species name
  data_species_fundiv <- data_species_fundiv %>% mutate(
    latinname = case_when(
      rank == "genus" ~ paste(genus, "spp."),
      rank %in% c("species", "subspecies", "subsp") ~ paste(genus, species),
      TRUE ~ acceptedname
    )
  )
  
  return(data_species_fundiv)
}