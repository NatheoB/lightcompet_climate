Get_Shadetol <- function(data_nandm_fp, data_poorter_fp,
                         species_list) {
  
  data_shadetol_nandm <- vroom(data_nandm_fp, show_col_types = F)
  data_shadetol_poorter <- vroom(data_poorter_fp, show_col_types = F)
  
  # Bind both datasets
  data_shadetol <- dplyr::bind_rows(
    data_shadetol_nandm %>% 
      dplyr::select(species = Species, shadetol = shade_tolerance.mean),
    data_shadetol_poorter %>% 
      dplyr::filter(species != "Pinus uncinata") # Not the same value as the dataset from Niinemets and Valladares
  ) %>% 
    dplyr::filter(species %in% species_list) %>% 
    dplyr::distinct()
  
  # Observe which species are lacking
  print(setdiff(species_list, data_shadetol$species))
  
  # Add lacking species
  data_shadetol <- data_shadetol %>% 
    add_row(
      species = c("Pinus canariensis", "Populus spp.", "Juniperus thurifera", "Tilia spp.", "Salix spp.", "Sorbus spp."),
      # order = c("gymno", "angio", "gymno", "angio", "angio", "angio"),
      shadetol = c(1, 
                   mean(data_shadetol_nandm$shade_tolerance.mean[grep("Populus", data_shadetol_nandm$Species)]),
                   mean(data_shadetol_nandm$shade_tolerance.mean[grep("Juniperus", data_shadetol_nandm$Species)]),
                   mean(data_shadetol_nandm$shade_tolerance.mean[grep("Tilia", data_shadetol_nandm$Species)]),
                   mean(data_shadetol_nandm$shade_tolerance.mean[grep("Salix", data_shadetol_nandm$Species)]),
                   mean(data_shadetol_nandm$shade_tolerance.mean[grep("Sorbus", data_shadetol_nandm$Species)]))
    ) %>% 
    dplyr::arrange(shadetol) %>% 
    dplyr::mutate(species = factor(species, levels = species)) %>% 
    dplyr::filter(!species %in% c("Pinus canariensis", "Pinus radiata"))
  
  return(data_shadetol)
}