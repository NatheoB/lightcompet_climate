# _targets.R file
library(targets)

library(future)
library(future.callr)
plan(callr)

# Source functions in R folder
lapply(grep("R$", list.files("R", recursive = TRUE), value = TRUE), function(x) source(file.path("R", x)))

# Set options (i.e. clustermq.scheduler for multiprocess computing)
options(tidyverse.quiet = TRUE, clustermq.scheduler = "multiprocess")

tar_option_set(packages = c("dplyr", "tidyr", "data.table", "vroom",
                            "foreign", "sp", "terra", "readxl",
                            "httr", "rgdal", "purrr", "lubridate",
                            "truncnorm", "RCapsis", "TNRS",
                            "RPostgreSQL", "nlme", "pROC",
                            "stringr", "Hmisc", "FactoMineR", "factoextra"))

# install.packages("https://sourceforge.net/projects/repiceasource/files/latest", repos = NULL,  type="source")
# install.packages("https://sourceforge.net/projects/rcapsis.capsisbridge.p/files/latest", repos = NULL,  type="source")
# devtools::install_github("ecoinfor/U.Taxonstand")
# devtools::install_github("EnquistLab/RTNRS")

# List of targets
list(
 
  
  ############################################################
  ##################### DATA CREATION ########################
  ############################################################
  
  # Forest inventories ----
  
  ## Filtering and harmonizing ----
  
  ### FunDivEUROPE NFIs ----
  
    # Import trees, plots and species corresp dataset for FUNDIV
  tar_target(data_plots_fundiv_raw_fp, "data/NFIs/FunDivEurope/FunDiv_plots_Nadja.csv", format = "file"),
  tar_target(data_plots_fundiv_raw, fread(data_plots_fundiv_raw_fp)),
  tar_target(data_trees_fundiv_raw_fp, "data/NFIs/FunDivEurope/tree.csv", format = "file"),
  tar_target(data_trees_fundiv_raw, fread(data_trees_fundiv_raw_fp)),
  tar_target(data_fundiv_raw, list(plots = data_plots_fundiv_raw, trees = data_trees_fundiv_raw)),

  tar_target(data_species_fundiv_fp, "data/NFIs/FunDivEurope/FunDiv_species_Nadja.csv", format = "file"),
  tar_target(data_species_fundiv, fread(data_species_fundiv_fp)),

    # Remove France and germany plots (new NFI harmonization, cf below)
  tar_target(data_fundiv_noGerFra, Remove_FunDiv_country(data_fundiv_raw, c("DE", "FG"))),

    # Get srtm30 altitude of Spanish plots (from true long/lat) and add altitude column
  tar_target(coords_spain, Get_Coords_country(data_fundiv_noGerFra, "ES")),

  tar_target(coords_spain_srtm30, Find_SRTM30_Tile(coords_spain)),
  tar_target(coords_spain_srtm30_group, coords_spain_srtm30 %>% group_by(srtm30_tile) %>% tar_group(), iteration = "group"),

  tar_target(data_srtm30_spain_list, Extract_SRTM30_tile(coords_spain_srtm30_group, unique(coords_spain_srtm30_group$srtm30_tile),
                                                   folderpath_srtm = "data/syno/SRTM30"), pattern = map(coords_spain_srtm30_group), iteration = "list"),
  tar_target(data_srtm30_spain, Merge_SRTM30_List(data_srtm30_spain_list)),

  tar_target(data_fundiv_noGerFra_altEsp, Add_Altitude_to_DataPlots(data_fundiv_noGerFra, data_srtm30_spain %>% select(plotcode, altitude = srtm30))),

    # Find and join latin species name in data_tree
  tar_target(data_species_fundiv_latinname, Find_LatinName_FunDiv(data_species_fundiv)),
  tar_target(data_fundiv_noGerFra_altEsp_latinname, Add_Latinname_to_DataTrees(data_fundiv_noGerFra_altEsp, data_species_fundiv_latinname %>% select(speciesid = id, latinname), "speciesid")),

    # Harmonize FunDivEUROPE datasets
  tar_target(data_fundiv, Harmonize_Datasets_FunDiv(data_fundiv_noGerFra_altEsp_latinname)),



  ### Germany NFI ----

    # Import trees, plots and altitude datasets for German NFI in FUNDIV format
  tar_target(data_plots_germany_raw_fp, "data/NFIs/NFI_Germany/FUNDIV_plot_GermanNFI.csv", format = "file"),
  tar_target(data_plots_germany_raw, fread(data_plots_germany_raw_fp)),
  tar_target(data_trees_germany_raw_fp, "data/NFIs/NFI_Germany/FUNDIV_tree_GermanNFI.csv", format = "file"),
  tar_target(data_trees_germany_raw, fread(data_trees_germany_raw_fp)),
  tar_target(data_germany_raw, list(plots = data_plots_germany_raw, trees = data_trees_germany_raw)),

  tar_target(data_alt_germany_fp, "data/NFIs/NFI_Germany/GermanNFI_plot_coordinates.csv", format = "file"),
  tar_target(data_alt_germany, fread(data_alt_germany_fp)),

    # Add altitude column
  tar_target(data_germany_altAdded, Add_Altitude_to_DataPlots(data_germany_raw, data_alt_germany %>% select(plotcode, altitude = elevation))),

    # Harmonize German datasets
  tar_target(data_germany, Harmonize_Datasets_Germany(data_germany_altAdded)),



  ### France NFI ----

    # Import trees, plots, altitude and species corresp datasets
  tar_target(data_plots_france_raw_fp, "data/NFIs/NFI_France/PLACETTE.csv", format = "file"),
  tar_target(data_plots_france_raw, fread(data_plots_france_raw_fp)),
  tar_target(data_trees_france_raw_fp, "data/NFIs/NFI_France/ARBRE.csv", format = "file"),
  tar_target(data_trees_france_raw, fread(data_trees_france_raw_fp)),
  tar_target(data_france_raw, list(plots = data_plots_france_raw, trees = data_trees_france_raw)),

  tar_target(data_species_france_fp, "data/NFIs/NFI_France/correspondance_matthieu_final.csv", format = "file"),
  tar_target(data_species_france, fread(data_species_france_fp)),

  tar_target(data_alt_france_fp, "data/NFIs/NFI_France/foret_placettes_inrae.csv", format = "file"),
  tar_target(data_alt_france, fread(data_alt_france_fp)),

    # Join altitude to plot dataset
  tar_target(data_france_altAdded, Add_Altitude_to_DataPlots(data_france_raw, data_alt_france %>% select(IDP = idp, altitude = zp), "IDP")),

    # Join latinname to trees dataset
  tar_target(data_france_altAdded_latinname, Add_Latinname_to_DataTrees(data_france_altAdded, data_species_france %>% select(ESPAR = code, latinName), "ESPAR")),

    # Filter and harmonize France NFI datasets
  tar_target(data_france_filtered, Filter_Datasets_France(data_france_altAdded_latinname)),
  tar_target(data_france, Harmonize_Datasets_France(data_france_filtered)),



  ### Poland NFI ----

    # Import trees, plots datasets
  tar_target(data_plots_poland_raw_fp, "data/NFIs/NFI_Poland/Polish_NFI_plots.csv", format = "file"),
  tar_target(data_plots_poland_raw, fread(data_plots_poland_raw_fp)),
  tar_target(data_trees_poland_raw_fp, "data/NFIs/NFI_Poland/Polish_NFI_trees.csv", format = "file"),
  tar_target(data_trees_poland_raw, fread(data_trees_poland_raw_fp)),
  tar_target(data_poland_raw, list(plots = data_plots_poland_raw, trees = data_trees_poland_raw)),

    # Filter and harmonize Polish NFI datasets
  tar_target(data_poland_filtered, Filter_Datasets_Poland(data_poland_raw)),
  tar_target(data_poland, Harmonize_Datasets_Poland(data_poland_filtered)),



  ### Slovenia NFI ----

    # Import trees, plots and species corresp datasets
  tar_target(data_plots_slovenia_raw_fp, "data/NFIs/NFI_Slovenia/NFI Slovenia_plots_displaced.dbf", format = "file"),
  tar_target(data_plots_slovenia_raw, read.dbf(data_plots_slovenia_raw_fp)),
  tar_target(data_trees_slovenia_raw_fp, "data/NFIs/NFI_Slovenia/NFI_Slovenia_trees_nolocation.dbf", format = "file"),
  tar_target(data_trees_slovenia_raw, read.dbf(data_trees_slovenia_raw_fp)),
  tar_target(data_slovenia_raw, list(plots = data_plots_slovenia_raw, trees = data_trees_slovenia_raw)),

  tar_target(data_species_slovenia_fp, "data/NFIs/NFI_Slovenia/NFI Slovenia_tree species_completed.csv", format = "file"),
  tar_target(data_species_slovenia, fread(data_species_slovenia_fp)),

    # Add slovenia latinname to trees dataset
  tar_target(data_slovenia_speciesChar, Change_Typeof_dataset_col_funct(data_slovenia_raw, "trees", "species", "as.character")),
  tar_target(data_slovenia_speciesInt, Change_Typeof_dataset_col_funct(data_slovenia_speciesChar, "trees", "species", "as.integer")),
  tar_target(data_slovenia_latinname, Add_Latinname_to_DataTrees(data_slovenia_speciesInt, data_species_slovenia %>% select(species = Code, Latinname), "species")),

    # Filter and harmonize Slovenian NFI datasets
  tar_target(data_slovenia_filtered, Filter_Datasets_Slovenia(data_slovenia_latinname)),
  tar_target(data_slovenia, Harmonize_Datasets_Slovenia(data_slovenia_filtered)),


  ### Slovakia NFI ----

    # Import dataset
  tar_target(data_slovakia_raw_fp, "data/NFIs/NFI_Slovakia/Slovak NFI data for Kunstler.xlsx", format = "file"),
  tar_target(data_slovakia_raw, read_excel(data_slovakia_raw_fp, sheet = "data")),

    # Filter and harmonize Slovakian NFI dataset
  tar_target(data_slovakia_noalt, Filter_and_Harmonize_Datasets_Slovakia(data_slovakia_raw)),

    # Get srtm30 altitude of slovakia plots (from true long/lat) and add altitude column
  tar_target(coords_slovakia, data_slovakia_noalt$plots[,c("plotcode", "longitude", "latitude")]),

  tar_target(coords_slovakia_srtm30, Find_SRTM30_Tile(coords_slovakia)),
  tar_target(coords_slovakia_srtm30_group, coords_slovakia_srtm30 %>% group_by(srtm30_tile) %>% tar_group(), iteration = "group"),

  tar_target(data_srtm30_slovakia_list, Extract_SRTM30_tile(coords_slovakia_srtm30_group, unique(coords_slovakia_srtm30_group$srtm30_tile),
                                                            folderpath_srtm = "data/syno/SRTM30"), pattern = map(coords_slovakia_srtm30_group), iteration = "list"),
  tar_target(data_srtm30_slovakia, Merge_SRTM30_List(data_srtm30_slovakia_list)),

    # Correct Slovakia altitude
  tar_target(data_slovakia, Correct_Slovakia_Altitude_from_SRTM30(data_slovakia_noalt, data_srtm30_slovakia)),



  ### Czech Republic NFI ----

    # Import datasets
  tar_target(data_trees_czechrep_raw_fp, "data/NFIs/NFI_CzechRep/trees.csv", format = "file"),
  tar_target(data_trees_czechrep_raw, vroom(data_trees_czechrep_raw_fp, locale = locale(decimal_mark = ","))),

  tar_target(data_plots1_czechrep_raw_fp, "data/NFIs/NFI_CzechRep/plots1.csv", format = "file"),
  tar_target(data_plots1_czechrep_raw, vroom(data_plots1_czechrep_raw_fp, locale = locale(decimal_mark = ","))),

  tar_target(data_plots2_czechrep_raw_fp, "data/NFIs/NFI_CzechRep/plots2.csv", format = "file"),
  tar_target(data_plots2_czechrep_raw, vroom(data_plots2_czechrep_raw_fp, locale = locale(decimal_mark = ","))),

  tar_target(data_species_czechrep_raw_fp, "data/NFIs/NFI_CzechRep/species.csv", format = "file"),
  tar_target(data_species_czechrep_raw, vroom(data_species_czechrep_raw_fp)),



    # Filter and harmonize Czech Republic NFI
  tar_target(data_czechrep, Filter_and_Harmonize_Datasets_CzechRep(data_trees_czechrep_raw,
                                                                   data_plots1_czechrep_raw, data_plots2_czechrep_raw,
                                                                   data_species_czechrep_raw)),


  ## Merge all NFI datasets ----
  tar_target(data_NFIs_merged, Merge_NFIs_Datasets(list(data_fundiv$plots, data_germany$plots, data_france$plots, data_poland$plots, data_slovenia$plots, data_slovakia$plots, data_czechrep$plots),
                                                   list(data_fundiv$trees, data_germany$trees, data_france$trees, data_poland$trees, data_slovenia$trees, data_slovakia$trees, data_czechrep$trees))),


  ## Harmonize latin names ----
  tar_target(NFIs_species, unique(data_NFIs_merged$trees$species)),
  # tar_target(NFIs_species_standardized_uncompleted, Standardized_NFIs_Species(NFIs_species)),

    # tar_load(NFIs_species_standardized)
    # write.table(NFIs_species_standardized %>%
    #               dplyr::filter(NFIs_species_standardized$latinname == "") %>%
    #               dplyr::arrange(rawname),
    #             "unaccepted_names.csv", sep = ";", dec = ".", row.names = F)


  # tar_target(NFIs_species_unaccepted_corresp_fp, "data/NFIs/unaccepted_names_corresp.csv", format = "file"),
  # tar_target(NFIs_species_unaccepted_corresp, vroom(NFIs_species_unaccepted_corresp_fp)),
  # tar_target(NFIs_species_standardized, Correct_UnacceptedNames_Species(NFIs_species_standardized_uncompleted,
  #                                                                       NFIs_species_unaccepted_corresp)),

  tar_target(NFIs_species_standardized_fp, "data/NFIs/nfis_species_names.csv", format = "file"),
  tar_target(NFIs_species_standardized, vroom(NFIs_species_standardized_fp, delim =)),
  
  tar_target(data_NFIs_unchecked, Homogenize_Latinnames(data_NFIs_merged, NFIs_species_standardized)),


  ## Check NAs ----
  tar_target(data_NFIs, Check_NFIs_Datasets(data_NFIs_unchecked)),


  ## Save datasets ----
  tar_target(data_plots_fp, Save_Dataframe(data_NFIs$plots, "output/data_SamsaraEurope_plots.csv")),
  tar_target(data_trees_fp, Save_Dataframe(data_NFIs$trees, "output/data_SamsaraEurope_trees.csv")),


  ## Get all coordinates ----
  tar_target(coords, data_NFIs$plots %>% select(plotcode, longitude, latitude, altitude)),



  
  
  
  
  # Radiation dataset ----

    ## Fetch raw radiation data for each plot ----
  tar_target(pvgis_raw, Get_DataPVGIS_from_API(coords, max_requests_per_sec = 30)),

    ## Create and save radiation dataset ----
  tar_target(pvgis_list, Create_Radiation_ListOfDatasets(pvgis_raw)),
  tar_target(pvgis_partial, Merge_Radiation_Datasets(pvgis_list)),
  tar_target(data_radiation, Add_PVGIS_MissingPlots(pvgis_partial, coords)),
  tar_target(data_radiation_fp, Save_Dataframe(data_radiation, "output/data_SamsaraEurope_radiation.csv")),



  
  

  # Soil dataset ----

    ## Dynamic branching through variables, depths and values ----
  tar_target(sg_var, c("nitrogen", "phh2o", "soc", "cec", "bdod", "clay", "silt", "sand")),
  tar_target(sg_depth, c("0-5cm", "5-15cm", "15-30cm", "30-60cm", "60-100cm", "100-200cm")),
  tar_target(sg_value, c("mean", "uncertainty")),

    ## Extract variables for each var, each depth and each group of coords ----
  tar_target(sg_raw, Extract_SoilGrids_from_Serv_var_depth_value(coords, sg_var, sg_depth, sg_value,
                                                                 "/vsicurl/https://files.isric.org/soilgrids/latest/data"), pattern = cross(sg_var, sg_depth, sg_value), iteration = "list"),

    ## Merge list of datasets for each var and depth into a single dataframe ----
  tar_target(sg_merged, Merge_SoilGrids(sg_raw)),

  
    ## Add missing plots ----
  tar_target(sg_completed, Correct_SoilGrids_MissingPlots(sg_merged, coords)),


    ## Rescale SoilGrids datasets ----
  tar_target(sg_rescaled, Rescale_SoilGrids(sg_completed)),
  tar_target(sg_rescaled_fp, Save_Dataframe(sg_rescaled, "output/data_SoilGrids_allHorizons.csv")),


    ## Extract rooting depth (from raster or fix it at 60cm) ----
  tar_target(data_rootingdepths_locDep, Extract_and_Check_RootingDepth(coords, "data/STU_EU_DEPTH_ROOTS.rst")),
  tar_target(data_rootingdepths_fixed, coords %>% dplyr::mutate(rooting_depth = 60)),


    ## Compute SWHC in volumetric proportion SWHCProp and SWHC in mm SWHCTot (with fixed and location dependent depth) ----
  tar_target(data_swhcProp, Compute_SoilGridsDerived_SWHCProp(sg_rescaled, sg_depth)),

  tar_target(data_swhc_locDepDepth, Compute_SWHCTot(data_swhcProp, data_rootingdepths_locDep, sg_depth)),
  tar_target(data_swhc_fixedDepth, Compute_SWHCTot(data_swhcProp, data_rootingdepths_fixed, sg_depth)),


    ## Compute weighted mean for all variables ----
  tar_target(data_soil_mean, Compute_SoilGrids_WeightedMean(sg_rescaled, sg_var)),

  
    ## Select and save final variables ----
  tar_target(data_soil, Select_SoilVariables(data_soil_mean, data_swhc_locDepDepth, data_swhc_fixedDepth)),
  tar_target(data_soil_reduced, data_soil %>% select(plotcode, phh2o, cec, nitrogen, soc,
                                                     silt, sand, clay, bdod,
                                                     rooting_depth, contains("swhcProp"))),
  tar_target(data_soil_fp, Save_Dataframe(data_soil_reduced, "output/data_SamsaraEurope_soil.csv")),



  
  
  

  # Climate dataset ----

    ## Use dynamic branching through years and vars ----
  tar_target(years, seq(1983,2018)),
  tar_target(vars, c("pet", "pr", "tas", "tasmin", "tasmax")),


    ## Extract chelsa values (produce one file per variable per year with values for all coordinates) ----
  tar_target(chelsa_raw_vars_years, Extract_Chelsa_var_year(coords, vars, years, folderpath_chelsa_vars = "data/syno/envicloud/chelsa/chelsa_V2/GLOBAL/monthly"), pattern = cross(vars, years), iteration = "list"),
  
  
    ## merge them into one file per year with all variables for each month ----
  tar_target(chelsa_merged_years, Merge_ChelsaVars_year(chelsa_raw_vars_years, years, save_file = FALSE), pattern = map(years), iteration = "list"),


    ## Compute and write lapserate rasters for each month of each year within the extent of the data ----
    ## (in output folder if rasters are not already computed in folderpath_try_lr)
    ## (slope of regression of chelsa tas against wc elevation)
  tar_target(months, c(paste0("0", 1:9), 10:12)),
  tar_target(crop_ext, Get_CropExt(coords)),
  tar_target(rast_lr_filepath_years_months, Compute_and_Write_LapseRate_Raster_year_month(years, months, crop_ext, window_size = 11,
                                                                                          folderpath_tas = "data/syno/envicloud/chelsa/chelsa_V2/GLOBAL/monthly/tas",
                                                                                          filepath_alt = "data/syno/WorldClim/wc2.1_30s_elev.tif",
                                                                                          folderpath_try_lr = "data/syno/lapserate"), pattern = cross(years, months), iteration = "list"),


    ## Extract and bind worldclim elevation and tas lapserate ----
  tar_target(data_lr_years, Extract_LapseRate_year(coords, years, rast_lr_filepath_years_months), pattern = map(years), iteration = "list"),
  tar_target(data_alt_wc, Extract_WCalt(coords, filepath_alt = "data/syno/WorldClim/wc2.1_30s_elev.tif")),
  tar_target(chelsa_lr_alts_years, Bind_ChelsaLrAlt_years(chelsa_merged_years, data_lr_years, data_alt_wc, coords[,c("plotcode", "altitude")], years), pattern = map(years), iteration = "list"),
  
    ## Compute corrected chelsa tas (tascorrect) ----
  tar_target(chelsa_corrected_years, Correct_TAS_year(chelsa_lr_alts_years, save_file = FALSE), pattern = map(chelsa_lr_alts_years), iteration = "list"),


    ## Rescale chelsa variables for each year ----
  tar_target(chelsa_rescaled_years, Rescale_Chelsa_year(chelsa_corrected_years, c(vars, "tascorrect"), save_file = TRUE), pattern = map(chelsa_corrected_years), iteration = "list"),

    ## Compute water balance with climate and soil variables ----
  tar_target(water_balance_params, Get_WaterBalance_Params(c("PrModif", "expAET", "locDepDepth"))),
  tar_target(waterbalance_years, Compute_WaterBalance_year(chelsa_rescaled_years, data_soil, water_balance_params, years, save_file = FALSE), pattern = map(years), iteration = "list"),

    ## Compute available water as precipitations limited by snow ----
  tar_target(liquid_water_years, Compute_LiquidWater_year(chelsa_rescaled_years, years, save_file = FALSE), pattern = map(years), iteration = "list"),

    ## Compute climatic proxy variables (SGDD, wai, wa...) for each year ----
  tar_target(climate_proxy_years, Compute_ClimateProxy_year(chelsa_rescaled_years, liquid_water_years, years, save_file = FALSE), pattern = map(years), iteration = "list"),


    ## Average variables on increment period ----
  tar_target(data_climatic_incrperiod, Compute_Climate_MeanIncrementPeriod(waterbalance_years, water_balance_params,
                                                                           climate_proxy_years,
                                                                           years, data_NFIs$plots)),
  
    ## Average variables on the whole Chelsa period (1983-2018) ----
  tar_target(data_climatic_chelsaperiod, Compute_Climate_MeanChelsaPeriod(waterbalance_years, water_balance_params,
                                                                          climate_proxy_years,
                                                                          years)),
    ## Compute climate anomaly ----
  tar_target(data_climatic_anomaly, Compute_Climate_Anomalies(data_climatic_incrperiod,
                                                              data_climatic_chelsaperiod)),
  
    ## Bind climatic data ---- 
  tar_target(data_climatic, purrr::reduce(list(data_alt_wc, 
                                               data_climatic_incrperiod,
                                               data_climatic_chelsaperiod,
                                               data_climatic_anomaly),
                                          left_join, by = "plotcode")),
  
    ## Save dataframe ----
  tar_target(data_climatic_fp, Save_Dataframe(data_climatic, "output/data_SamsaraEurope_climate.csv")),




  
  
  # Competition dataset ----
  
  
  ## Crowding indices ----
  tar_target(data_compet_ba, Compute_BACompetition_Variables(data_NFIs$trees)),


  ## Light competition ----

    ### Set seed for random algorithm ----
  tar_target(seed, 38),

    ### Lower and higher limits for assignment of shade tolerance group ----
  tar_target(shadetol_limits, c(2.25, 3.75)),

    ### Parameters for samsaralight simulations ----
  tar_target(output_samsalight_folderpath, "output/samsalight"),
  tar_target(capsis_dir, "/home/beauchamp/capsis4"),

  tar_target(plot_dim_m, 100), # Size of the side of the squared plot in meters
  tar_target(cell_dim_m, 5), # Size of the side of the squared cell within the plot in meters


  ### Filter forest inventories for light dataset ----

    #### Remove treestatus1 trees (ingrowth) ----
  tar_target(data_NFIslight_noRege, Filter_NFIsDataset_treestatus(data_NFIs, 2:6)),

    #### Filter plots and trees datasets for light computation ----
  tar_target(data_NFIslight_noRege_reduceSI, Filter_NFIsDataset_country(data_NFIslight_noRege, c("SI" = 0.05), seed)),
  tar_target(data_NFIslight_noRege_reduceSI_noManag, Filter_NFIsDataset_noManagement(data_NFIslight_noRege_reduceSI, 0)),

    #### Filter species (remove plots with rare species and inconsistent species) ----
  tar_target(species_table_before_filter, Get_Species_Occurence_treestatus(data_NFIslight_noRege_reduceSI_noManag, c(2, 4))),

  tar_target(genus_to_remove, c("Olea", "Eucalyptus", "Phillyrea")),
  tar_target(species_to_remove, unique(data_NFIslight_noRege_reduceSI_noManag$trees$species)[grepl(paste0("^(", paste0(genus_to_remove, collapse = "|"), ")"),
                                                                                                   unique(data_NFIslight_noRege_reduceSI_noManag$trees$species))]),
  tar_target(data_NFIslight_noRege_reduceSI_noManag_spfilter1, Filter_NFIsDataset_removeSp(data_NFIslight_noRege_reduceSI_noManag,
                                                                                           species_to_remove)),

  tar_target(data_NFIslight_filtered, Filter_NFIsDataset_rareSpecies(data_NFIslight_noRege_reduceSI_noManag_spfilter1,
                                                                     min_plots = 100, min_trees = 200)),

  tar_target(species_table_after_filter, Get_Species_Occurence_treestatus(data_NFIslight_filtered, c(2, 4))),

  tar_target(species_NFIslight, species_table_after_filter$species),
  tar_target(species_removed, setdiff(unique(data_NFIslight_noRege_reduceSI_noManag$trees$species), species_NFIslight)),


  ### Prepare dataset for SamsaraLight (lad and allometries) ----

    #### Get order and shade tolerance group ----
    # Shade tolerance from Niinemets and Valladares 2006
    # Gymnosperm or Angiosperm and l = low, m = mid, h = high
    # Fill by hand species not in Niinemets and Valladares
  tar_target(data_NandV_fp, "data/samsara/data_Niinemets&Valladares_2006.csv", format = "file"),
  tar_target(data_NandV, vroom(data_NandV_fp)),

  tar_target(speciesinfo_shadetol_uncompleted, Get_NandV_Groups_sp(data_NandV, species_NFIslight, shadetol_limits)),

    # tar_load(speciesinfo_shadetol_uncompleted)
    # write.table(speciesinfo_shadetol_uncompleted %>% dplyr::arrange(species),
    #               "data/samsara/to_complete/species_shadetol.csv", sep = ";", dec = ".", row.names = F)

  tar_target(speciesinfo_shadetol_fp, "data/samsara/to_complete/species_shadetol.csv", format = "file"),
  tar_target(speciesinfo_shadetol, vroom(speciesinfo_shadetol_fp)),


    #### Assign leaf area density value for each species (according to shade tolerance group) ----
  tar_target(data_lad_fp, "data/samsara/lad_leuschner&meier.csv", format = "file"),
  tar_target(data_lad, vroom(data_lad_fp)),

  tar_target(speciesinfo_shadetol_lad, Find_LADgroup_sp(speciesinfo_shadetol)),
  tar_target(data_NFIslight_filtered_lad, Assign_LAD_sp(data_lad, data_NFIslight_filtered,
                                                        speciesinfo_shadetol_lad)),

    #### Assign order (Gymnosperm or Angiosperm) ----
  tar_target(data_NFIslight_filtered_lad_order, Assign_Order_sp(data_NFIslight_filtered_lad, speciesinfo_shadetol_lad)),


    #### Assign height, crown ratio and crown diameter allometries ----

      # Load species specific allometries and averaged per group of shade tolerance and order
  tar_target(params_alloms_sp_fp, "data/samsara/params_alloms_sp.csv", format = "file"),
  tar_target(params_alloms_sp, vroom(params_alloms_sp_fp)),
  tar_target(params_alloms_sp_list, Split_ParamsAlloms_by_Var(params_alloms_sp)),
  
  tar_target(params_alloms_group_fp, "data/samsara/params_alloms_group_nocomp.csv", format = "file"),
  tar_target(params_alloms_group, vroom(params_alloms_group_fp)),


      # Find allometries and assign parameters
  tar_target(speciesinfo_shadetol_lad_alloms_uncompleted, Find_Allomgroup_sp(speciesinfo_shadetol_lad, params_alloms_sp_list)),

    # tar_load(speciesinfo_shadetol_lad_alloms_uncompleted)
    # write.table(speciesinfo_shadetol_lad_alloms_uncompleted %>% dplyr::arrange(species),
    #               "data/samsara/to_complete/species_shadetol_alloms.csv", sep = ";", dec = ".", row.names = F)

  tar_target(speciesinfo_shadetol_lad_alloms_fp, "data/samsara/to_complete/species_shadetol_alloms.csv", format = "file"),
  tar_target(speciesinfo_shadetol_lad_alloms, vroom(speciesinfo_shadetol_lad_alloms_fp)),

  tar_target(speciesinfo, Correct_Allomgroup_sp(speciesinfo_shadetol_lad_alloms)),
  tar_target(speciesinfo_fp, Save_Dataframe(speciesinfo, "data/samsara/species_info.csv")),



  tar_target(data_NFIslight_filtered_lad_order_allomParams, Assign_Alloms_sp(params_alloms_sp_list, params_alloms_group,
                                                                             data_NFIslight_filtered_lad_order, speciesinfo)),

    # Compute height and crown allometries for each trees (from Touzot et al.)
  tar_target(data_NFIslight_filtered_lad_order_alloms, Compute_Trees_Allometries(data_NFIslight_filtered_lad_order_allomParams, data_compet_ba)),


  #### Create virtual trees ----
  # (Duplicate NFI trees based on tree weight and plot weight and assign a unique id)
  tar_target(data_NFIslight, Create_VirtualTrees(data_NFIslight_filtered_lad_order_alloms, plot_dim_m)),
  
  #### Check tree light dataset to avoid crashing SamsaLight ----
  tar_target(check_NFIslight, Check_TreesLightDataset(data_NFIslight)),


  #### Create tree inventories ----
  tar_target(lilo_inventories, Create_LiloInventories(data_NFIslight, check_NFIslight, plot_dim_m, cell_dim_m, seed)),
  tar_target(lilo_inventories_fp, Save_SamsaFiles(lilo_inventories, file.path(output_samsalight_folderpath, "inv"))),


  #### Create weather files (radiation table and growing season) ----
  tar_target(weather_files_rad, Compute_Radiation_MonthlyMean(data_radiation %>% dplyr::filter(plotcode %in% data_NFIslight$plots$plotcode))),
  tar_target(weather_files_vegetperiod_fixed, Compute_VegetPeriod_Fixed(data_NFIslight$plots, 1, 365)),

  tar_target(weather_files, Create_WeatherFiles(weather_files_rad, weather_files_vegetperiod_fixed)),
  tar_target(weather_files_fp, Save_SamsaFiles(weather_files, file.path(output_samsalight_folderpath, "weather"))),


  #### Run Samsara LightLoader (Lilo) for trees  ----
  tar_target(lilo_outputs, Run_SamsaraLight_for_Trees(weather_files_fp, lilo_inventories_fp,
                                                      file.path(output_samsalight_folderpath, "export"),
                                                      capsis_dir)),
    tar_target(data_light, Compute_LightCompetition_Variables(lilo_outputs, data_NFIslight)),


  #### Run Samsara LightLoader (Lilo) for cells  ----
  tar_target(lilo_outputs_cells, Run_SamsaraLight_for_Cells(weather_files_fp, lilo_inventories_fp,
                                                            capsis_dir)),
  tar_target(data_lightcells_fp, Save_Dataframe(lilo_outputs_cells, "output/data_SamsaraEurope_lightcells.csv")),
  
  
  ## Merge and save datasets ----
  tar_target(data_compet, dplyr::full_join(data_compet_ba, data_light, by = "treecode")),
  tar_target(data_compet_fp, Save_Dataframe(data_compet, "output/data_SamsaraEurope_compet.csv")),
  
  
  
  
  ############################################################
  ###################### Models fit ##########################
  ############################################################
  
  # GLOBAL PARAMETERS ----
  tar_target(seed, 38),
  
  
  # PREPARE DATASETS ----
  ## Create global dataset ----
  tar_target(dataset_global, 
             Create_GlobalDataset()),
  
  ## Prepare global dataset ----
  tar_target(dataset_global_cleaned,
             Prepare_GlobalDataset(dataset_global)),
  
  ## Group some species ----
  tar_target(species_to_group_list, list(
    "Betula spp." = c("Betula pendula", "Betula pubescens", "Betula spp."),
    "Populus spp." = c("Populus nigra", "Populus spp.", "Populus tremula"),
    "Quercus spp." = c("Quercus petraea", "Quercus robur", "Quercus spp."),
    "Salix spp." = c("Salix alba", "Salix caprea", "Salix cinerea", "Salix spp."),
    "Sorbus spp." = c("Sorbus aria", "Sorbus aucuparia", "Sorbus torminalis"),
    "Tilia spp." = c("Tilia cordata", "Tilia platyphyllos", "Tilia spp."),
    "Ulmus spp." = c("Ulmus glabra", "Ulmus minor", "Ulmus spp.")
  )),
  
  tar_target(dataset_global_cleaned_grouped,
             Group_Species(dataset_global_cleaned,
                           species_to_group_list, 
                           c("Quercus petraea", "Quercus robur",
                             "Betula pendula", "Betula pubescens"))),
  
  
  
  ## Create specific growth and mortality datasets ---- 
  
  ### Growth dataset
  tar_target(data_growth,
             Prepare_GrowthDataset(dataset_global_cleaned_grouped,
                                   nplots_min = 500,
                                   nliving_min = 1000,
                                   nliving_min_per_country = 100,
                                   sp_to_remove = c("Quercus spp.", "Betula spp."))),
  
  tar_target(data_growth_fp,
             Save_Dataframe(data_growth, "output/data_growth.csv")),
  
  tar_target(data_growth_scaled,
             Scale_Dataset(data_growth,
                           c("dbh", "dbh_log", "lci", "bat", "bal",
                             "sgdd_inv", "aet2pet_inv", "sgdd", "aet2pet",
                             "sgdd2", "aet2pet2"))),
  
  tar_target(data_growth_scaled_fp,
             Save_Dataframe(data_growth_scaled$data, "output/data_growth_scaled.csv")),
  
  tar_target(attr_growth_scaled_fp,
             Save_Dataframe(data_growth_scaled$attr, "output/attr_growth_scaled.csv")),
  
  tar_target(species_calib_growth, names(sort(table(data_growth$species), decreasing = T))),
  
  
  ### Mortality dataset
  tar_target(data_mortality,
             Prepare_MortalityDataset(dataset_global_cleaned_grouped,
                                      nplots_min = 500,
                                      nliving_min = 1000,
                                      ndead_min = 100,
                                      nliving_min_per_country = 100,
                                      ndead_min_per_country = 20,
                                      sp_to_remove = c("Quercus spp.", "Betula spp."))),
  
  tar_target(data_mortality_fp,
             Save_Dataframe(data_mortality, "output/data_mortality.csv")),
  
  
  tar_target(data_mortality_scaled,
             Scale_Dataset(data_mortality,
                           c("dbh", "dbh_log", "lci", "bat", "bal",
                             "sgdd_inv", "aet2pet_inv", "sgdd", "aet2pet",
                             "sgdd2", "aet2pet2"))),
  
  tar_target(data_mortality_scaled_fp,
             Save_Dataframe(data_mortality_scaled$data, "output/data_mortality_scaled.csv")),
  
  tar_target(attr_mortality_scaled_fp,
             Save_Dataframe(data_mortality_scaled$attr, "output/attr_mortality_scaled.csv")),
  
  
  tar_target(species_calib_mortality, names(sort(table(data_mortality$species), decreasing = T))),
  
  
  ## Get mean environment dataset ----
  tar_target(data_mean_env, Get_Mean_Env(data_growth)),
  
  
  # DATASET OVERVIEW ----
  # tar_target(plots_overview_global,
  #             Plot_OverviewGlobal_Growth_species(data_growth,
  #                                                "output/plots/overview/global",
  #                                                "overview_global_")),
  # tar_target(plots_overview_compet,
  #            Plot_OverviewCompet_Growth_species(data_growth,
  #                                               "output/plots/overview/compet",
  #                                               "overview_compet_")),
  # 
  # tar_target(plots_overview_climate,
  #            Plot_OverviewClimate_Growth_species(data_growth,
  #                                                "output/plots/overview/climate",
  #                                                "overview_climate_")),
  
  
  # CALIBRATE MODELS ----
  tar_target(fit_growth, Fit_Growth(sp = species_calib_growth,
                                    data_gr = data_growth_scaled$data,
                                    n_samples = 20,
                                    resampling_weighted_by = c("dbh", "aet2pet", "sgdd"),
                                    prop_resampling = 0.8,
                                    n_folds = 5,
                                    use_dredge = TRUE,
                                    get_only_coefs = TRUE,
                                    seed = 38),
             pattern = map(species_calib_growth),
             iteration = "list"),
  
  tar_target(fit_mortality, Fit_Mortality(sp = species_calib_mortality,
                                          data_morta = data_mortality_scaled$data,
                                          n_samples = 20,
                                          resampling_weighted_by = c("dbh", "aet2pet", "sgdd"),
                                          prop_resampling = 0.8,
                                          n_folds = 5,
                                          use_dredge = TRUE,
                                          get_only_coefs = TRUE,
                                          seed = 38),
             pattern = map(species_calib_mortality),
             iteration = "list"),
  
  
  # GET OUTPUT TABLES ----
  tar_target(out_growth, Get_OutputTable_Growth(fit_growth, data_growth)),
  tar_target(out_mortality, Get_OutputTable_Mortality(fit_mortality, data_mortality)),
  
  tar_target(out_growth_fp, Save_Dataframe(out_growth, "output/out_growth.csv")),
  tar_target(out_mortality_fp, Save_Dataframe(out_mortality, "output/out_mortality.csv")),
  
  
  
  
  NULL
  )
