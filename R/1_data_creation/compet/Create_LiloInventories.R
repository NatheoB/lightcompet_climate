Create_LiloInventories <- function(data_NFIslight, data_check,
                                   plot_dim_m, cell_dim_m,
                                   seed) {

  # Check if dataset is clean
  if (!data_check) stop("dataset is not clean")
  
  
  # Set seed for same randomization of position of trees within plot
  set.seed(seed)

  
  # Create Samsara code for each species
  species_samsara_code <- data_NFIslight$trees %>% 
    dplyr::select(species) %>% 
    dplyr::distinct() %>% 
    dplyr::mutate(samsara_code = row_number())

  # Create inventory string for each plot
  data_NFIslight$trees %>%

    # Bind species samsara code
    dplyr::left_join(species_samsara_code, by = "species") %>% 
    
    # Create tree string for inventory
    dplyr::mutate(tree_str = paste0(lilo_id, "\t",
                                    samsara_code, "\t",
                                    runif(n(), 0, plot_dim_m), "\t", runif(n(), 0, plot_dim_m), "\t", # Random location within 0,plot_dim_m square plot
                                    dbh1/10, "\t",
                                    height, "\t",
                                    cbh, "\t",
                                    ifelse(order == "g", # Gymnosperm:hmax=hbase, Angiosperm:hmax = hbase + 1/2(htot-hbase)
                                           cbh,
                                           cbh + 1/2*(height - cbh)), "\t",
                                    cr, "\t", cr, "\t", cr, "\t", cr, "\t", # radial symetry of crown (same radius in 4 directions)
                                    0.035, "\t", # random value because we don't mind using crown openess (we use LAD in turbid medium)
                                    lad # species specific lad
    )) %>% 
    
    # Bind all tree rows into a single string table for inventory of the plot
    dplyr::group_by(plotcode) %>% 
    dplyr::summarise(trees_str = paste0(tree_str, collapse = "\n")) %>% 
    dplyr::mutate(trees_str = paste0("#tree\n#id	species	x	y	dbh	h	hbase	hmax	rn	re	rs	rw	crownOpeness	CrownLAD\n",
                                     trees_str)) %>% 
    
    # Bind plot data
    dplyr::left_join(data_NFIslight$plots, by = "plotcode") %>% 
    
    # Create plot strings
    dplyr::mutate(
      
      records_str = paste0(
            "#Key records", "\n"	,
            "plotName = ", plotcode, "\n",
            "date = ", lubridate::year(surveydate1), "\n",
            "cellWidth = ", cell_dim_m, "\n",
            "sensorLightOnly = ", "false", "\n",
            "plotXMin = ", 0, "\n",
            "plotXMax = ", plot_dim_m, "\n",
            "plotYMin = ", 0, "\n",
            "plotYMax = ", plot_dim_m, "\n",
            "plotSlope = ", 0, "\n",
            "plotaspect = ", 0, "\n",
            "plotNorthToXAngle = ", 0, "\n",
            "plotLattitude = ", latitude, "\n",
            "plotLongitude = ", longitude, "\n",
            "addVirtualBufferStand = ", "false"
          ),
      
      species_str = paste0("# Species\n# id : nombre entier\n# name : nom de l'espèce\n# color : RGB values, nombres entiers separes par une virgule\n# speciesId name color crownShape\n",
                           paste(apply(species_samsara_code %>% 
                                  dplyr::select(samsara_code, species) %>% 
                                  dplyr::mutate(color = "150,150,150"), 
                                MARGIN=1, paste, 
                                collapse = "\t"),
                          collapse = "\n")),
      
      invlines_str = "#inventory zone line\n#x	y\n0.0\t0.0\n0.0\t100.0\n100.0\t100.0\n100.0\t0.0"
      
      ) %>% 
    
    # Unite all strings into a single one for each plotcode
    tidyr::unite("out_str", c(records_str, species_str, invlines_str, trees_str), sep = "\n\n") %>% 
    
    # Select only final string
    dplyr::select(plotcode, out_str)

}



### OLD VERSION BUT TOO LONG : PREFER THE USE OF VECTORS ###

# Create_and_Save_LiloInventory_plot <- function(row_plot,
#                                                data_trees, dataList_allom, data_plots,
#                                                sp_params_name, dataList_sp_params, 
#                                                plot_size,
#                                                output_folderpath) {
#   # print(row_plot)
#   
#   # Get interest plot
#   plot <- data_plots[row_plot,]
#   trees <- data_trees %>% filter(plotcode == plot$plotcode[1])
#   
#   # Get interest data for the given inventory name
#   data_allom <- dataList_allom[[sp_params_name]] %>% filter(plotcode == plot$plotcode[1])
#   data_sp_params <- dataList_sp_params[[sp_params_name]]
# 
# 
#   ##### KEY RECORDS #####
#   
#   records_str <- paste0(
#     "#Key records", "\n"	,										
#     "plotName = ", paste0("SamsaraEurope_", plot$plotcode), "\n",													
#     "date = ", lubridate::year(plot$surveydate1), "\n",													
#     "cellWidth = ", 5, "\n",													
#     "sensorLightOnly = ", "false", "\n",													
#     "plotXMin = ", 0, "\n",												
#     "plotXMax = ", plot_size, "\n",													
#     "plotYMin = ", 0, "\n",													
#     "plotYMax = ", plot_size, "\n",													
#     "plotSlope = ", 0, "\n",													
#     "plotaspect = ", 0, "\n",													
#     "plotNorthToXAngle = ", 0, "\n",													
#     "plotLattitude = ", plot$latitude, "\n",													
#     "plotLongitude = ", plot$longitude, "\n",
#     "addVirtualBufferStand = ", "false"
#   )
#   
#   
#   ##### SPECIES #####
#   
#   species_str <- paste(lapply(1:nrow(data_sp_params), function(X) {
#     sp_params <- data_sp_params[X,]
#     paste(sp_params$samsara_code,
#           sp_params$samsara_name,
#           "150,150,150", 
#           sep = "\t")
#   }), collapse = "\n")
#   
#   species_str <- paste0("# Species\n# id : nombre entier\n# name : nom de l'espèce\n# color : RGB values, nombres entiers separes par une virgule\n# speciesId name color crownShape\n",
#                         species_str)
#   
#   
#   ##### INVENTORY LINES #####
#   
#   invlines_str <- "#inventory zone line\n#x	y\n0.0\t0.0\n0.0\t100.0\n100.0\t100.0\n100.0\t0.0"
#   
#   
#   ##### TREES #####
#   virtualplot_weight <- plot_size**2/10000
# 
#   tree_corresp <- trees %>% 
#     select(treecode, weight1) %>%
#     mutate(ntrees = round(weight1*virtualplot_weight)) %>% 
#     uncount(ntrees) %>% 
#     mutate(lilo_id = row_number()) %>% 
#     select(lilo_id, treecode) 
#   
#   lilo_id <- 0
#   trees_str <- paste0(lapply(1:nrow(trees), function(i) {
#     
#     # Get considered NFI tree
#     tree <- trees[i,]
# 
#     # Create row for each virtual tree of the considered NFI tree
#     paste0(lapply(1:round(tree$weight1*virtualplot_weight), function(j) {
#       
#       # Update outer scope lilo id 
#       lilo_id <<- lilo_id + 1
# 
#       # Create string inventory
#       paste0(lilo_id, "\t",
#              tree$samsara_code, "\t",
#              runif(1, 0, plot_size), "\t", runif(1, 0, plot_size), "\t", # Random location within 0,100 square plot
#              tree$dbh1/10, "\t",
#              data_allom$height1[i], "\t", 
#              data_allom$cbh1[i], "\t",
#              ifelse(tree$type == "Conifer", # Conifer:hmax=hbase, Broadleaved:hmax = hbase + 1/2(htot-hbase)
#                     data_allom$cbh1[i], 
#                     data_allom$cbh1[i] + 1/2*(data_allom$height1[i] - data_allom$cbh1[i])), "\t", 
#              data_allom$cr1[i], "\t",
#              data_allom$cr1[i], "\t", 
#              data_allom$cr1[i], "\t",
#              data_allom$cr1[i], "\t", # radial symetry of radius
#              0.035, "\t", # random value because we don't mind using crown openess (we use LAD in turbid medium)
#              data_sp_params$lad[which(data_sp_params$samsara_code == tree$samsara_code)]) # species specific lad
#       
#       
#     }), collapse = "\n")
#     
#   }), collapse = "\n")
# 
#   trees_str <- paste0("#tree\n#id	species	x	y	dbh	h	hbase	hmax	rn	re	rs	rw	crownOpeness	CrownLAD\n",
#                       trees_str)
#   
#   
#   ##### ALL INVENTORY #####
#   inventory_str <- paste(records_str, species_str, invlines_str, trees_str,
#                          sep = "\n\n")
#   
#   
#   ##### SAVE FILE #####
#   
#   # Create a treecorresp folder specific to the plot in output/simus_tmp/plotcode
#   treecorresp_folderpath <- file.path(output_folderpath, "treecorresp", sp_params_name)
#   if (!file.exists(treecorresp_folderpath)) {
#     dir.create(treecorresp_folderpath, recursive = TRUE)
#   }
#   
#   # Create an inventory folder specific to the plot and inventory in output/simus_tmp/plotcode
#   inventory_folderpath <- file.path(output_folderpath, "inv", sp_params_name, plot$plotcode)
#   if (!file.exists(inventory_folderpath)) {
#     dir.create(inventory_folderpath, recursive = TRUE)
#   }
#   
#   # Save tree corresp
#   corresp_filepath <- file.path(treecorresp_folderpath, paste0("treecorresp_", plot$plotcode, "_", sp_params_name, ".txt"))
#   write.table(tree_corresp, corresp_filepath, sep = "\t", dec = ".", row.names = FALSE)
#   
#   # Save inventory in output/simus_tmp/plotcode/inventory
#   inventory_filepath <- file.path(inventory_folderpath, paste0("lilo_inventory_", plot$plotcode, "_", sp_params_name, ".inv"))
#   writeLines(inventory_str, inventory_filepath)
#   
#   return(list(inv = inventory_filepath, corresp = corresp_filepath))
# }
# 
# 
# Create_and_Save_LiloInventories <- function(data_trees, dataList_allom, data_plots, 
#                                             sp_params_name, dataList_sp_params, 
#                                             plot_size,
#                                             output_folderpath) {
#   
#     # Create output folder if it does not exist
#   if (!file.exists(output_folderpath)) {
#     dir.create(output_folderpath, recursive = TRUE)
#   }
#   
#     # Create inventory for each plot and return filepath of the inventory
#   inventories_fp <- lapply(1:nrow(data_plots), Create_and_Save_LiloInventory_plot,
#            data_trees, dataList_allom, data_plots,
#            sp_params_name, dataList_sp_params, 
#            plot_size,
#            output_folderpath)
#   
#   
#   return(list(name = sp_params_name, val = inventories_fp))
# }
# 
# 
