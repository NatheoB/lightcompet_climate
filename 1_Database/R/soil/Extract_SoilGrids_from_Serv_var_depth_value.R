Extract_SoilGrids_from_Serv_var_depth_value <- function(coords, var, depth, value, rast_folderpath)
{
  ### LOAD RASTER
  rast_filepath <- file.path(rast_folderpath, var, paste0(var, "_", depth, "_", value, ".vrt"))
  rast <- terra::rast(rast_filepath)
  
    
  ### CONVERT COORDS
    # Set wgs84 proj to long/lat coords
  coords_igh <- terra::vect(coords, geom = c("longitude", "latitude"), crs = "epsg:4326")
  
    # Convert long/lat coords into igh (homolosine goode) projection
  coords_igh <- terra::project(coords_igh, "+proj=igh +datum=WGS84 +no_defs +towgs84=0,0,0")
  
    # Convert SpatVector into a coords dataframe
  coords_igh <- terra::crds(coords_igh, df = TRUE)
  
  
  ### EXTRACT SOILGRIDS VALUES AND NAME COLUMNS
  data_sg <- terra::extract(rast, coords_igh)[-1] # First column is ID : we don't mind
  names(data_sg) <- paste(var, depth, value, sep="_")
  
  data_sg <- bind_cols(plotcode = coords$plotcode, data_sg)
  
  
  ### RETURN VALUES AS DF FOR A GIVEN VAR, DEPTH AND VALUE (mean or uncertainty)
  return(list(var = var, depth = depth, value = value, data = data_sg))
}