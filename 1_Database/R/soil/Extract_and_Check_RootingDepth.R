# https://esdac.jrc.ec.europa.eu/content/european-soil-database-derived-data#tabs-0-description=0

Extract_and_Check_RootingDepth <- function(coords, rast_fp) {
  
  ### IMPORT RASTER
  rast <- terra::rast(rast_fp)
  
  ### EXTRACT ROOTING DEPTH
  print("First extraction")
  data_rooting_depth <- Extract_RootingDepth(coords, rast)
  
  ### CORRECT MISSING VALUES
  # Values with rooting_depth = 0 means that we were on a missing cell (e.g. water)
  # Due to blurred data : plot in water or rocky cell
  # Method : add or substract 1km at one or both axis until we obtain a no 0-value cell
  # If only 0-value cells around, set to NA
  
  add_vects <- data.frame(x_km = 0, y_km = 0)
  max_range_km <- 5
  
  for (range_km in 1:max_range_km) {
    print(paste("Range in km :", range_km))
    
    # Expand the cell we explore if extracted cell was inside a null cell
    add_vects_incrRange <- do.call("expand.grid", list(x_km = -range_km:range_km, 
                                                       y_km = -range_km:range_km))
    add_vects <- add_vects_incrRange[!(add_vects_incrRange$x_km %in% add_vects$x_km &
                                        add_vects_incrRange$y_km %in% add_vects$y_km),] 
    
    # Search a correct cell for missing cells
    
    for (i in 1:nrow(add_vects)) {
      print(add_vects[i,])
      
      ### GET COORDS OF MISSING VALUES
      coords_left <- data_rooting_depth %>% 
        dplyr::filter(rooting_depth == 0) %>% 
        dplyr::left_join(coords, by = "plotcode") %>% 
        dplyr::select(plotcode, longitude, latitude)
      
      print(nrow(coords_left))
      
      ### STOP IF NO MISSING VALUE
      if (nrow(coords_left) == 0) {
        return(data_rooting_depth)
      }
      
      ### ADD +/-1km TO ONE OR BOTH AXIS (BE CAREFUL CONVERSION IN DEG)
      # https://stackoverflow.com/questions/1253499/simple-calculations-for-working-with-lat-lon-and-km-distance
      coords_left$latitude <- coords_left$latitude + add_vects$y_km[i]/110.574
      coords_left$longitude <- coords_left$longitude + add_vects$x_km[i]/(111.320*cos(coords_left$latitude * pi / 180))

      ### EXTRACT AGAIN ROOTING DEPTH WITH NEW COORDS AND BIND IT TO 
      data_tmp <- Extract_RootingDepth(coords_left, rast)
      data_rooting_depth[match(data_tmp$plotcode, 
                               data_rooting_depth$plotcode), ] <- data_tmp
    }
    
  }
    
  return(data_rooting_depth)
  
}


Extract_RootingDepth <- function(coords, rast) {
  
  ### CONVERT COORDS
  # Get coords in wgs84 proj = long/lat coords
  coords_wgs84 <- terra::vect(coords, geom = c("longitude", "latitude"), crs = "epsg:4326")
  
  # Convert long/lat coords into ETRS89-LAEA projection
  coords_etrs89laea <- terra::project(coords_wgs84, "epsg:3035")
  
  # Convert SpatVector into a coords dataframe
  coords_etrs89laea <- terra::crds(coords_etrs89laea, df = TRUE)
  
  ### EXTRACT ROOTING DEPTH
  data_rooting_depth <- terra::extract(rast, coords_etrs89laea)[2] # First column is ID : we don't mind
  names(data_rooting_depth) <- c("rooting_depth")
  data_rooting_depth <- bind_cols(plotcode = coords$plotcode, data_rooting_depth)
  
  return(data_rooting_depth)
}