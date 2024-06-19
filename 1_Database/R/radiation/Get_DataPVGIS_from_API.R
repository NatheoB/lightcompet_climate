# https://www.dataquest.io/blog/r-api-tutorial/

# Get PVGIS data from 2005 to 2020 : 
# https://joint-research-centre.ec.europa.eu/pvgis-photovoltaic-geographical-information-system_en

# Given coordinates in WGS84 :
# coords (dataframe) : columns
#   * plotcode (string) : code id of the given 

Get_DataPVGIS_from_API <- function(coords, max_requests_per_sec = 30) {
    # Get number of coords
  ncoords <- nrow(coords)
  
    # Set progress bar
  pb <- txtProgressBar(min = 0, max = ncoords, style = 3)
  
    # Init time
  API_sum_time <- 0
  
    # Get data for all coordinates from non-interactive interface of PVGIS at a given maximum rate
  data_PVGIS_list <- list()
  for (i in 1:ncoords) {
      # Limit the number of get requests to the PVGIS server (sleep if dt < sleeping_time)
    tryCatch(expr = {sleeping_time - as.double(Sys.time()-ts)}, error = function(e){})
      
      # Get time at the beginning of the loop
    ts <- Sys.time()
    
      # Set progress bar
    setTxtProgressBar(pb, value = i)

      # Get the data for the given plot
    data.tmp <- tryCatch(expr = {
        # Make our request to the API 
      res <- httr::GET(paste0("https://re.jrc.ec.europa.eu/api/v5_2/MRcalc?", # MRcalc = Monthly radiation tool
                        "lat=", coords[i,]$latitude, # lat latitude 
                        "&lon=", coords[i,]$longitude, # lon = longitude
                        "&horirrad=1", # horirrad : Output horizontal plane irradiation
                        "&d2g=1", # d2g : Output monthly values of the ratio of diffuse to global radiation (horizontal plane)
                        "&outputformat=basic")) # outputformat as "basic" = output without text as csv
        # Transform request in binary into string
      rawToChar(res$content)
      
    }, error = function(e) {
      print(e)
      return(NULL)
    })


      # Add data to general list of datasets
    data_PVGIS_list[[i]] <- list("plotcode" = coords[i,]$plotcode,
                                 "data" = data.tmp)
    
      # Update sum time
    API_sum_time <- API_sum_time + as.double(Sys.time()-ts)
  }
  
    # Close progress bar
  close(pb)
  
    # Print mean time for getting one coordinates dataset
  print(paste("Mean API time :", API_sum_time/ncoords))
  
  return(data_PVGIS_list)
}







