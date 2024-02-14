Save_SamsaFiles <- function(samsa_files, output_folderpath) {
  
  print("Deleting existing folder...")
  
  # Create samsa file folder (if it exists, delete it)
  if (file.exists(output_folderpath)) {
    unlink(output_folderpath, recursive = T) # delete folder
  }
  dir.create(output_folderpath, recursive = T) # create folder
  
  
  print("Saving samsa files...")
  
  # Create samsa files filepath : in a single folder for the given plotcode
  samsa_files %>% 
    
    dplyr::mutate(
      folderpath = file.path(output_folderpath, plotcode),
      fp = file.path(folderpath, paste0(plotcode, ".txt"))) %>% 
    
    dplyr::rowwise() %>% 
    dplyr::mutate(
      create_folder = dir.create(folderpath, recursive = T),
      save = writeLines(out_str, fp)) %>% 
    
    dplyr::select(plotcode, fp)
}

