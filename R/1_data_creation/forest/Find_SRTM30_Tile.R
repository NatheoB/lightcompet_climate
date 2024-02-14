# Tiles downloaded at https://dwtkns.com/srtm30m/

Find_SRTM30_Tile <- function(coords) {
    # Find tile name (e.g. N45W012)
  srtm30_tiles <- sapply(1:nrow(coords), function(X) {
    paste0(ifelse(coords[X,"latitude"] >= 0 , "N", "S"), abs(floor(coords[X,"latitude"])),
           ifelse(coords[X,"longitude"] >= 0, "E", "W"), ifelse(abs(floor(coords[X,"longitude"])) < 100, "0", ""), ifelse(abs(floor(coords[X,"longitude"])) < 10, "0", ""), abs(floor(coords[X,"longitude"]))) 
  })
  
    # Add to coords df
  cbind(coords, srtm30_tile = srtm30_tiles)
}
