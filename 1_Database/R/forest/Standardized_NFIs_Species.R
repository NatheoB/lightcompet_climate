### Using TNRS https://tnrs.biendata.org/instructions/

Standardized_NFIs_Species <- function(NFIs_species) {
  
  # Create dataframe adding column of species name without comma (TNRS don't like commas)
  NFIs_species <- data.frame(row = 1:length(NFIs_species), species_comma = NFIs_species) %>% 
    dplyr::mutate(species = sub(",", "", species_comma))
  
  # Get accepted species
  NFIs_species %>% 
    dplyr::select(row, species) %>% 
    TNRS::TNRS() %>% 
    dplyr::left_join(NFIs_species, by = c("Name_submitted" = "species")) %>% 
    dplyr::select(rawname = species_comma, latinname = Accepted_species)
}



### Using U.Taxonstand

# Standardized_NFIs_Species <- function(NFIs_species, database_name) {
# 
#   # https://www.sciencedirect.com/science/article/pii/S2468265922000944?via%3Dihub
# 
#   print("Loading database...")
# 
#   # Load database from github repository nameMatch/Database for a given plant database
#   urls <- paste0("https://github.com/nameMatch/Database/blob/main/Plants_",
#                  database_name,
#                  '_database_20220701/Plants_',
#                  database_name,
#                  "_database_part",
#                  1:3, ".xlsx?raw=true")
# 
#   files <- vector(mode = "list", length = 3)
#   for (i in 1:3) {
#     temp_file <- tempfile(fileext = ".xlsx")
#     req <- httr::GET(urls[i],
#                      # authenticate using GITHUB_PAT
#                      httr::authenticate(Sys.getenv("GITHUB_PAT"), ""),
#                      # write result to disk
#                      httr::write_disk(path = temp_file))
#     files[[i]] <- openxlsx::read.xlsx(temp_file)
#   }
# 
#   database <- rbind(files)
# 
# 
#   print("Matching species...")
#   # Match species names
#   U.Taxonstand::nameMatch(spList=NFIs_species, spSource=database, author=FALSE,
#                           max.distance=1, Append=FALSE)
# 
# }
