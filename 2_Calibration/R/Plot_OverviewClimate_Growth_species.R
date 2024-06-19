Plot_OverviewClimate_Growth_species <- function(data_growth, 
                                                output_folderpath, 
                                                output_names_pattern) {
  
  # Get range of climatic space
  sgdd_range <- range(data_growth$sgdd)
  aet2pet_range <- range(data_growth$aet2pet)
  
  plot_list <- list()
  
  for (spe in unique(data_growth$species)) {
    
    print(spe)
    
    name_sp <- gsub(" ", "", spe)
    
    data_tmp <- data_growth %>% 
      dplyr::filter(species == spe)
    
    
    ### Boxplots of climate
    
    # SGDD
    boxplot_sgdd <- ggplot(data_tmp, aes(y = sgdd, x = country)) +
      geom_boxplot() +
      labs(subtitle = "Sum of Growing Degree-Days (°C)") +
      theme(plot.subtitle = element_text(hjust = 0.5)) +
      ylim(sgdd_range[1], sgdd_range[2])
      
    
    # aet2pet
    boxplot_aet2pet <- ggplot(data_tmp, aes(y = aet2pet, x = country)) +
      geom_boxplot() +
      labs(subtitle = "Actual to potential E.T. ratio") +
      theme(plot.subtitle = element_text(hjust = 0.5)) +
      ylim(aet2pet_range[1], aet2pet_range[2])
    
    
    ### Climatic space
    
    p_cortest <- cor.test(data_tmp$sgdd, data_tmp$aet2pet)$p.value
    
    climate_space <- ggplot(data_tmp, aes(x = sgdd, y = aet2pet)) +
      geom_hex(bins = 50) +
      theme(legend.position = "none") +
      scale_fill_viridis_c() +
      xlim(sgdd_range[1], sgdd_range[2]) +
      ylim(aet2pet_range[1], aet2pet_range[2]) +
      geom_smooth(method = "lm", formula = y~x, color = "red") +
      labs(title = "Climatic space",
           subtitle = paste0("R2 = ", round(
             summary(lm(sgdd ~ aet2pet, data_tmp))$r.squared, 2),
             " and Person's correlation = ", 
             round(cor(data_tmp$sgdd, data_tmp$aet2pet), 2),
             " (",
             case_when(p_cortest > 0.05 ~ "no significant", 
                       p_cortest > 0.01 ~ "*",
                       p_cortest > 0.001 ~ "**",
                       TRUE ~ "***"),
             ")")) + 
      theme(plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5))

    climate_space_xdens <- axis_canvas(climate_space, axis = "x") +
      geom_density(data = data_tmp, mapping = aes(x = sgdd))

    climate_space_ydens <- axis_canvas(climate_space, axis = "y", coord_flip = T) +
      geom_density(data = data_tmp, mapping = aes(x = aet2pet)) +
      coord_flip()

    climate_space <- insert_xaxis_grob(climate_space, climate_space_xdens,
                                       grid::unit(.2, "null"), position = "top")
    climate_space <- insert_yaxis_grob(climate_space, climate_space_ydens,
                                       grid::unit(.2, "null"), position = "right")
      
    
    
  
    ### Create final plot 
    plot_list[[name_sp]] <- cowplot::plot_grid(
      
      # Title 
      cowplot::ggdraw() +
        cowplot::draw_label(paste("Environmental variables of", spe)),
      
      # Plots
      cowplot::plot_grid(
        cowplot::plot_grid(boxplot_sgdd, boxplot_aet2pet, nrow = 1),
        cowplot::plot_grid(NULL, climate_space, NULL, nrow = 1, rel_widths = c(2, 5, 2)),
        ncol = 1
      ),
      
      ncol = 1, rel_heights = c(1, 10)
    )
    
  }
  
  ### SAVE PLOTS
  print("Saving...")
  
  # Create folder if it does not exist
  if (!file.exists(output_folderpath)) {
    dir.create(output_folderpath, recursive = T) # create folder
  }
  
  # Create filepaths
  filepaths <- file.path(output_folderpath,
                         paste0(output_names_pattern, names(plot_list), ".png"))
  
  # Save all plots
  for (i in 1:length(plot_list)) {
    
    png(filepaths[i], width = 1200, height = 900)
    print(plot_list[[i]])
    dev.off()
    
  }
  
  return(filepaths)
  
}