Plot_OverviewCompet_Growth_species <- function(data_growth, 
                                               output_folderpath, 
                                               output_names_pattern) {
  
  plot_list <- list()
  for (spe in unique(data_growth$species)) {
    
    print(spe)
    
    name_sp <- gsub(" ", "", spe)
    
    data_tmp <- data_growth %>% 
      dplyr::filter(species == spe)
    
    
    # Plot distribution of light competition
    plot_lci <- ggplot(data_tmp, aes(x = lci)) +
      geom_histogram() +
      labs(subtitle = "LCI distribution") +
      theme(plot.subtitle = element_text(hjust = 0.5)) +
      geom_vline(xintercept = mean(data_tmp$lci),
                 color = "yellow",
                 linetype = "dashed") +
      geom_vline(xintercept = quantile(data_tmp$lci, 0.9),
                 color = "orange",
                 linetype = "dashed") +
      geom_vline(xintercept = quantile(data_tmp$lci, 0.1),
                 color = "orange",
                 linetype = "dashed") +
      geom_vline(xintercept = quantile(data_tmp$lci, 0.975),
                 color = "red",
                 linetype = "dashed") +
      geom_vline(xintercept = quantile(data_tmp$lci, 0.025),
                 color = "red",
                 linetype = "dashed")
      
    
    
    # Plot distribution of light within dbh space
    plot_lciXdbh <- ggplot(data_tmp, aes(x = dbh, y = lci)) +
      geom_hex(bins = 60) +
      labs(subtitle = "DBH and LCI distribution") +
      theme(plot.subtitle = element_text(hjust = 0.5)) +
      geom_vline(xintercept = mean(data_tmp$dbh),
                 color = "yellow",
                 linetype = "dashed") +
      geom_vline(xintercept = quantile(data_tmp$dbh, 0.9),
                 color = "orange",
                 linetype = "dashed") +
      geom_vline(xintercept = quantile(data_tmp$dbh, 0.1),
                 color = "orange",
                 linetype = "dashed") +
      geom_vline(xintercept = quantile(data_tmp$dbh, 0.975),
                 color = "red",
                 linetype = "dashed") +
      geom_vline(xintercept = quantile(data_tmp$dbh, 0.025),
                 color = "red",
                 linetype = "dashed")
    
    
    # Plot distribution within climatic space
    n_boxes <- 3
    for (var in c("sgdd", "aet2pet")) {
      
      # Split range into n boxes of same length between min and max values
      range_var <- range(data_tmp[[var]])
      step_var <- (range_var[2] - range_var[1]) / n_boxes 
      
      # Find group of each individuals and assign a probability as the inverse of the number of individuals within the class
      data_tmp <- data_tmp %>% 
        dplyr::mutate("group_{var}" := case_when(
          !!sym(var) == range_var[1] ~ 1,
          !!sym(var) == range_var[2] ~ n_boxes,
          TRUE ~ ceiling((!!sym(var) - range_var[1]) / step_var)
        )
        )
    }
    
    # Rename modalities
    data_tmp <- data_tmp %>% 
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
    data_tmp$group_sgdd = factor(data_tmp$group_sgdd, 
                                 levels=c("cold", "mid", "hot"))
    data_tmp$group_aet2pet = factor(data_tmp$group_aet2pet, 
                                   levels=c("wet", "mid", "dry"))
    
    txt_climategroups <- data_tmp %>% 
      dplyr::group_by(group_sgdd, group_aet2pet) %>% 
      dplyr::summarize(n_ind = n()) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(label = paste0("N = ", n_ind))
    
    plot_lci_climate <- ggdraw() +
      cowplot::draw_plot(ggplot(data = data_tmp, aes(x = sgdd, y = aet2pet)) +
                           theme(panel.grid.major = element_blank(), 
                                 panel.grid.minor = element_blank(),
                                 panel.background = element_blank(), 
                                 axis.line = element_line(colour = "black"))) +
      cowplot::draw_plot(ggplot(data = data_tmp, aes(x = lci)) +
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
    width = 0.80, height = 0.80, x = 0.17, y = 0.18)
    
    
    # Distribution along climate space
    plot_lciqt_sgdd <- ggplot(data_tmp, aes(y = lci, x = sgdd)) +
      geom_hex(bins = 60) +
      geom_quantile(quantiles = c(0.025, 0.975), color = "red", linewidth = 1.1) +
      geom_quantile(quantiles = c(0.1, 0.9), color = "orange", linewidth = 1.1) +
      geom_smooth(method = "lm", formula = y~x, color = "yellow", linewidth = 1.1) +
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5)) +
    labs(subtitle = "Regressions across temperature") +
      scale_y_continuous(breaks = seq(0, 1, by = 0.1),
                         labels = as.character(seq(0, 1, by = 0.1))) +
      theme(plot.subtitle = element_text(hjust = 0.5)) +
      geom_vline(xintercept = mean(data_tmp$sgdd),
                 color = "yellow",
                 linetype = "dashed") +
      geom_vline(xintercept = quantile(data_tmp$sgdd, 0.9),
                 color = "orange",
                 linetype = "dashed") +
      geom_vline(xintercept = quantile(data_tmp$sgdd, 0.1),
                 color = "orange",
                 linetype = "dashed") +
      geom_vline(xintercept = quantile(data_tmp$sgdd, 0.975),
                 color = "red",
                 linetype = "dashed") +
      geom_vline(xintercept = quantile(data_tmp$sgdd, 0.025),
                 color = "red",
                 linetype = "dashed")
    
    plot_lciqt_aet2pet <- ggplot(data_tmp, aes(y = lci, x = aet2pet)) +
      geom_hex(bins = 60) +
      geom_quantile(quantiles = c(0.025, 0.975), color = "red", linewidth = 1.1) +
      geom_quantile(quantiles = c(0.1, 0.9), color = "orange", linewidth = 1.1) +
      geom_smooth(method = "lm", formula = y~x, color = "yellow", linewidth = 1.1) +
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5)) +
      labs(subtitle = "Regressions across water stress") +
      scale_y_continuous(breaks = seq(0, 1, by = 0.1),
                         labels = as.character(seq(0, 1, by = 0.1))) +
      geom_vline(xintercept = mean(data_tmp$aet2pet),
                 color = "yellow",
                 linetype = "dashed") +
      geom_vline(xintercept = quantile(data_tmp$aet2pet, 0.9),
                 color = "orange",
                 linetype = "dashed") +
      geom_vline(xintercept = quantile(data_tmp$aet2pet, 0.1),
                 color = "orange",
                 linetype = "dashed") +
      geom_vline(xintercept = quantile(data_tmp$aet2pet, 0.975),
                 color = "red",
                 linetype = "dashed") +
      geom_vline(xintercept = quantile(data_tmp$aet2pet, 0.025),
                 color = "red",
                 linetype = "dashed")
    
    
    plot_list[[spe]] <- cowplot::plot_grid(
      
      # Title 
      cowplot::ggdraw() +
        cowplot::draw_label(paste("Compet overview of", spe)),
      
      cowplot::ggdraw() +
        cowplot::draw_label("Yellow is mean, orange are qts 10/90% and red are qts 2.5/97.5%",
                            size = 10),
  
      plot_grid(plot_lci, plot_lciXdbh, nrow = 1),
      
      
      # Density along climate space
      plot_grid(
        plot_grid(
          ggdraw() +
            cowplot::draw_label("Competition density along climatic space", size = 10),
          
          plot_lci_climate, ncol = 1, rel_heights = c(1, 10)
        ),
        
        
        # Distribution along climate space
        plot_lciqt_aet2pet, plot_lciqt_sgdd,
        
        nrow = 1),
      
      
      
      ncol = 1, rel_heights = c(1, 1, 10, 10)
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