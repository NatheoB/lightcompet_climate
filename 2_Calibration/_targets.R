# _targets.R file
library(targets)

library(future)
library(future.callr)
plan(callr)

# Source functions in R folder
lapply(grep("R$", list.files("R", recursive = TRUE), value = TRUE), 
       function(x) source(file.path("R", x)))

# Set options (i.e. clustermq.scheduler for multiprocess computing)
options(tidyverse.quiet = TRUE, clustermq.scheduler = "multiprocess")

tar_option_set(packages = c("dplyr", "tidyr", "data.table", "vroom",
                            "purrr", "lubridate", "MuMIn", "openxlsx2",
                            "truncnorm", "RPostgreSQL", "nlme", "pROC",
                            "stringr", "Hmisc", "FactoMineR", "factoextra"))

# List of targets
list(
  
  # GLOBAL PARAMETERS ----
  tar_target(seed, 38),
  
  
  # PREPARE DATASETS ----
  ## Create global dataset ----
  tar_target(dataset_global, 
             Create_GlobalDataset(from_inrae_database = FALSE, 
                                  datasets_folderpath = "../1_Database/output")),
  
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
  
  tar_target(species_calib_growth,
             names(sort(table(data_growth$species), decreasing = T))),
  
  
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
  
  
  tar_target(species_calib_mortality, 
             names(sort(table(data_mortality$species), decreasing = T))),
  
  
  ## Get mean environment dataset ----
  tar_target(data_mean_env, Get_Mean_Env(data_growth)),
  
  ## Get shade tolerance dataset ----
  tar_target(data_shadetol, Get_Shadetol("data/data_Niinemets&Valladares_2006.csv",
                                         "data/data_Poorter_2012.csv",
                                         species_calib_growth)),
  
  
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
  
  ## Global parameters ----
  tar_target(id_samples, 1:20),
  tar_target(prop_resampling, 0.8),
  tar_target(resampling_weighted_by, c("dbh", "aet2pet", "sgdd")),
  tar_target(n_folds, 5),
  
  ## Calibrate growth ----
  tar_target(fit_growth_scaled, Fit_Growth(sp = species_calib_growth,
                                           data_gr = data_growth_scaled$data,
                                           id_sample = id_samples,
                                           resampling_weighted_by = resampling_weighted_by,
                                           prop_resampling = prop_resampling,
                                           n_folds = n_folds,
                                           use_dredge = TRUE,
                                           get_only_coefs = TRUE,
                                           compet_type = c("lci", "bat", "batXdbh", "bal", "control"),
                                           seed = seed),
             pattern = cross(species_calib_growth, id_samples),
             iteration = "list"),
  
  ## Calibrate mortality ----
  tar_target(fit_mortality_scaled, Fit_Mortality(sp = species_calib_mortality,
                                                 data_morta = data_mortality_scaled$data,
                                                 id_sample = id_samples,
                                                 resampling_weighted_by = resampling_weighted_by,
                                                 prop_resampling = prop_resampling,
                                                 n_folds = n_folds,
                                                 use_dredge = TRUE,
                                                 get_only_coefs = TRUE,
                                                 compet_type = c("lci", "bat", "batXdbh", "bal", "control"),
                                                 seed = seed),
             pattern = cross(species_calib_mortality, id_samples),
             iteration = "list"),
  
  
  # GET OUTPUT TABLES ----
  tar_target(out_growth, Get_OutputTable_Growth(fit_growth_scaled, data_growth)),
  tar_target(out_mortality, Get_OutputTable_Mortality(fit_mortality_scaled, data_mortality)),
  
  tar_target(out_growth_fp, Save_Dataframe(out_growth, "output/out_growth.csv")),
  tar_target(out_mortality_fp, Save_Dataframe(out_mortality, "output/out_mortality.csv")),
  
  
  # CREATE FINAL EXCEL PARAMETER FILE ----
  tar_target(out_final_file, Create_Final_File(out_growth, out_mortality,
                                               data_growth_scaled$attr,
                                               data_mortality_scaled$attr,
                                               data_mean_env,
                                               data_shadetol,
                                               output_fp = "output/output_params.xlsx")),
  
  NULL
)
