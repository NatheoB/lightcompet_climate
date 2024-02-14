Correct_UnacceptedNames_Species <- function(species_uncompleted, unacepted_names_corresp) {
  
  match_id <- match(unacepted_names_corresp$rawname, species_uncompleted$rawname)
  species_uncompleted$latinname[match_id] <- unacepted_names_corresp$latinname
  
  species_uncompleted
}