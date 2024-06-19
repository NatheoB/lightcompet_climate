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


Correct_SoilGrids_MissingPlots <- function(data_sg, coords) {
  
  # Get plotcode with NA values (missing plots)
  unmissing_plots <- data_sg %>% 
    tidyr::drop_na() %>% 
    left_join(coords, by = "plotcode")
  
  missing_plots <- data_sg %>% 
    dplyr::filter(!(plotcode %in% unique(unmissing_plots$plotcode))) %>% 
    left_join(coords, by = "plotcode")

  # For each missing plot
  for (i in 1:nrow(missing_plots)) {
    print(i)
    # Find the closest plot and return data of closest plot
    closest_plot <- Find_Closest_plot(missing_plots[i,"longitude"],
                                      missing_plots[i,"latitude"],
                                      unmissing_plots)
    # Remove plotcode column (because plotcode of closest, we will add the plotcode of missing plot after)
    data_sg[which(data_sg$plotcode == missing_plots[i, "plotcode"]),-1] <- data_sg[which(data_sg$plotcode == closest_plot),-1] 
  }
  
  return(data_sg)
}
