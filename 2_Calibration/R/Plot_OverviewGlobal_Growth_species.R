Plot_OverviewGlobal_Growth_species <- function(data_growth, 
                                               output_folderpath, output_names_pattern) {
  
  plot_list <- list()
  
  for (spe in unique(data_growth$species)) {
    
    print(spe)
    
    name_sp <- gsub(" ", "", spe)
    
    ### Trees per country
    data_growth_species <- data_growth %>% 
      dplyr::filter(species == spe)
    
    plot1 <- ggplot(data_growth_species, aes(x = country)) +
      geom_bar(stat = "count") +
      geom_text(stat = "count", aes(label = format(after_stat(count), big.mark=",", trim=TRUE)), vjust=-0.25) +
      labs(subtitle = "Number of trees per country") +
      theme(plot.subtitle = element_text(hjust = 0.5))

    
    ### Plots where there is at least one occurence of each species
    plot_with_species <- data_growth %>% 
      dplyr::group_by(plotcode, country) %>% 
      dplyr::summarize(keep = spe %in% species) %>% 
      dplyr::filter(keep) %>% 
      dplyr::ungroup()
    
    plot2 <- ggplot(plot_with_species, aes(x = country)) +
      geom_bar(stat = "count") +
      geom_text(stat = "count", aes(label = format(after_stat(count), big.mark=",", trim=TRUE)), vjust=-0.25) +
      labs(subtitle = "Number of plots per country") +
      theme(plot.subtitle = element_text(hjust = 0.5))
    
    
    ### Mean % of specific G per plot (% of monospecific plots), per country
    gProps_per_plots <- data_growth %>%
      dplyr::filter(plotcode %in% plot_with_species$plotcode) %>%
      dplyr::mutate(is_species = species==spe) %>%
      dplyr::group_by(plotcode, country) %>%
      dplyr::mutate(G_m2 = pi * ((dbh/1000)**2) / 4,
                    Gtot = sum(G_m2)) %>% 
      dplyr::group_by(plotcode, country, is_species) %>% 
      dplyr::filter(is_species) %>% 
      dplyr::summarize(Gprop = sum(G_m2)/unique(Gtot)) %>%
      dplyr::select(country, Gprop) %>% 
      dplyr::ungroup()
    
    plot3 <- ggplot(gProps_per_plots, aes(y = Gprop, x = country)) +
      geom_boxplot() +
      labs(subtitle = "Prop of specific G per plot per country") +
      theme(plot.subtitle = element_text(hjust = 0.5))
    
    ### DBH distribution
    plot4 <- ggplot(data_growth_species, aes(x = dbh)) +
      geom_histogram() +
      labs(subtitle = "DBH distribution (in mm)") +
      theme(plot.subtitle = element_text(hjust = 0.5))
  
    
    ### DBH distribution within climatic space
    n_boxes <- 3
    for (var in c("sgdd", "aet2pet")) {
      
      # Split range into n boxes of same length between min and max values
      range_var <- range(data_growth_species[[var]])
      step_var <- (range_var[2] - range_var[1]) / n_boxes 
      
      # Find group of each individuals and assign a probability as the inverse of the number of individuals within the class
      data_growth_species <- data_growth_species %>% 
        dplyr::mutate("group_{var}" := case_when(
          !!sym(var) == range_var[1] ~ 1,
          !!sym(var) == range_var[2] ~ n_boxes,
          TRUE ~ ceiling((!!sym(var) - range_var[1]) / step_var)
        )
        )
    }
    
    # Rename modalities
    data_growth_species <- data_growth_species %>% 
      dplyr::mutate(
        group_sgdd = case_when(
          group_sgdd == 1 ~ "cold",
          group_sgdd == 2 ~ "mid",
          group_sgdd == 3 ~ "hot"),
        group_aet2pet = case_when(
          group_aet2pet == 1 ~ "dry",
          group_aet2pet == 2 ~ "mid",
          group_aet2pet == 3 ~ "wet"
        ))
    data_growth_species$group_sgdd = factor(data_growth_species$group_sgdd, 
                                 levels=c("cold", "mid", "hot"))
    data_growth_species$group_aet2pet = factor(data_growth_species$group_aet2pet, 
                                   levels=c("wet", "mid", "dry"))
    
    txt_climategroups <- data_growth_species %>% 
      dplyr::group_by(group_sgdd, group_aet2pet) %>% 
      dplyr::summarize(n_ind = n()) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(label = paste0("N = ", n_ind))
    
    
    # For each compet variable
    plot5 <- ggdraw() +
      cowplot::draw_plot(ggplot(data = data_growth_species, aes(x = sgdd, y = aet2pet)) +
                           theme(panel.grid.major = element_blank(), 
                                 panel.grid.minor = element_blank(),
                                 panel.background = element_blank(), 
                                 axis.line = element_line(colour = "black")) +
                           labs(subtitle = "DBH distribution (in mm) along climate space") +
                           theme(plot.subtitle = element_text(hjust = 0.5))) +
      cowplot::draw_plot(ggplot(data = data_growth_species, aes(x = dbh)) +
                           facet_grid(rows = vars(group_aet2pet),
                                      cols = vars(group_sgdd),
                                      drop = F) +
                           geom_density() +
                           theme(axis.title.y = element_blank(),
                                 axis.text.y = element_blank(),
                                 axis.ticks.y = element_blank()) +
                           geom_text(
                             size    = 4,
                             data    = txt_climategroups,
                             mapping = aes(x = Inf, y = Inf, label = label),
                             hjust   = 1.05,
                             vjust   = 1.5
                           ),
                         width = 0.80, height = 0.80, x = 0.15, y = 0.15)
    
    ### DBH and LCI distribution
    plot6 <- ggplot(data_growth_species, aes(x = bat)) +
      geom_histogram() +
      labs(subtitle = "Total basal area (in m2/ha) distribution") +
      theme(plot.subtitle = element_text(hjust = 0.5))
    
    ### Create final plot 
    plot_list[[name_sp]] <- cowplot::plot_grid(
      
      # Title 
      cowplot::ggdraw() +
        cowplot::draw_label(paste("Global overview of", spe)),
      
      # Plots
      cowplot::plot_grid(
        cowplot::plot_grid(plot1, plot2, plot3, nrow = 1),
        cowplot::plot_grid(plot4, plot5, plot6, nrow = 1),
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