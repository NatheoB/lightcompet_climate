dist_euclid <- function(x1, y1, x2, y2) {
  sqrt((x1-x2)^2 + (y1-y2)^2)
}


Find_Closest_plot <- function(long_ref, lat_ref, coords) {
    # Find the lower euclidian distance between long/lat of plot_ref and other plots
  dists <- coords %>% summarize(
    plotcode = plotcode,
    dist = dist_euclid(long_ref, lat_ref, longitude, latitude)
  )

  return(dists$plotcode[which.min(dists$dist)])
}


Add_PVGIS_MissingPlots <- function(pvgis_partial, coords) {
  
    # Get plotcode that are in coords but not in pvgis_all and get other plots
  missing_plots <- setdiff(coords$plotcode, pvgis_partial$plotcode)
  pvgis_plots <- coords %>% filter(!plotcode %in% missing_plots)
  
    # For each missing plot
  pvgis_missingplots <- lapply(missing_plots, function(X) {
      # Find the closest plot and return the same pvgis data
    closest_plot <- Find_Closest_plot(coords$longitude[which(coords$plotcode==X)],
                                      coords$latitude[which(coords$plotcode==X)],
                                      pvgis_plots)
      # Remove plotcode column (because plotcode of closest, we will add the plotcode of missing plot after)
    pvgis_partial[which(pvgis_partial$plotcode == closest_plot),-1] 
  })
  
    # Bind all missing plots by rows and add plotcode of missing plots
  pvgis_missingplots <- do.call("rbind", pvgis_missingplots)
  pvgis_missingplots <- data.frame(plotcode = missing_plots, pvgis_missingplots)
  
    # Add missing plots to pvgis dataset
  pvgis_all <- rbind(pvgis_partial, pvgis_missingplots)
  
  return(pvgis_all)
}
