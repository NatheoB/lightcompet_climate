Extract_SRTM30_tile <- function(coords_srtm30_tile, tilename, folderpath_srtm) {
    # Import raster
  tilepath <- paste0(tilename, ".SRTMGL1.hgt.zip")
  if (!tilepath %in% list.files(folderpath_srtm)) {stop(paste("missing tile raster", tilepath))}
  rast_tile <- terra::rast(file.path(folderpath_srtm, tilepath))
  
    # Extract from coords
  res_tile <- terra::extract(rast_tile, coords_srtm30_tile[,c("longitude", "latitude")])
  
    # Check number of rows
  if(nrow(res_tile) != nrow(coords_srtm30_tile)) stop("missing plots")
  
    # Bind plotcode and extracted values
  res_tile <- cbind(plotcode = coords_srtm30_tile$plotcode, res_tile %>% select(-ID))
  
  return(res_tile)
}