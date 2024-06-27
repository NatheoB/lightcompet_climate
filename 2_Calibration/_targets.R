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
                            "purrr", "lubridate", "MuMIn",
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
  # tar_target(fit_growth_scaled, Fit_Growth(sp = species_calib_growth,
  #                                   data_gr = data_growth_scaled$data,
  #                                   n_samples = 20,
  #                                   resampling_weighted_by = c("dbh", "aet2pet", "sgdd"),
  #                                   prop_resampling = 0.8,
  #                                   n_folds = 5,
  #                                   use_dredge = TRUE,
  #                                   get_only_coefs = TRUE,
  #                                   seed = 38),
  #            pattern = map(species_calib_growth),
  #            iteration = "list"),
  # 
  # tar_target(fit_mortality_scaled, Fit_Mortality(sp = species_calib_mortality,
  #                                         data_morta = data_mortality_scaled$data,
  #                                         n_samples = 20,
  #                                         resampling_weighted_by = c("dbh", "aet2pet", "sgdd"),
  #                                         prop_resampling = 0.8,
  #                                         n_folds = 5,
  #                                         use_dredge = TRUE,
  #                                         get_only_coefs = TRUE,
  #                                         seed = 38),
  #            pattern = map(species_calib_mortality),
  #            iteration = "list"),

  
  tar_target(fit_growth, Fit_Growth(sp = species_calib_growth,
                                    data_gr = data_growth,
                                    n_samples = 20,
                                    resampling_weighted_by = c("dbh", "aet2pet", "sgdd"),
                                    prop_resampling = 0.8,
                                    n_folds = 5,
                                    use_dredge = TRUE,
                                    get_only_coefs = TRUE,
                                    compet_type = c("lci"),
                                    seed = 38),
             pattern = map(species_calib_growth),
             iteration = "list"),

  tar_target(fit_mortality, Fit_Mortality(sp = species_calib_mortality,
                                          data_morta = data_mortality,
                                          n_samples = 20,
                                          resampling_weighted_by = c("dbh", "aet2pet", "sgdd"),
                                          prop_resampling = 0.8,
                                          n_folds = 5,
                                          use_dredge = TRUE,
                                          get_only_coefs = TRUE,
                                          compet_type = c("lci"),
                                          seed = 38),
             pattern = map(species_calib_mortality),
             iteration = "list"),
  
  # GET OUTPUT TABLES ----
  tar_target(out_growth, Get_OutputTable_Growth(fit_growth, data_growth)),
  tar_target(out_mortality, Get_OutputTable_Mortality(fit_mortality, data_mortality)),

  # CREATE FINAL DATASETS ----
  tar_target(params_growth_all, Get_Params(out_growth)),
  tar_target(params_mortality_all, Get_Params(out_mortality)),
  
  tar_target(params_growth_mean, Summarise_Params(params_growth_all)),
  tar_target(params_mortality_mean, Summarise_Params(params_mortality_all)),
  
  # SAVE DATASETS ----
  tar_target(out_growth_fp, Save_Dataframe(out_growth, "output/out_growth.csv")),
  tar_target(out_mortality_fp, Save_Dataframe(out_mortality, "output/out_mortality.csv")),
  
  tar_target(params_growth_all_fp, Save_Dataframe(params_growth_all, "output/params_growth_all.csv")),
  tar_target(params_mortality_all_fp, Save_Dataframe(params_mortality_all, "output/params_mortality_all.csv")),
  
  tar_target(params_growth_mean_fp, Save_Dataframe(params_growth_mean, "output/params_growth_mean.csv")),
  tar_target(params_mortality_mean_fp, Save_Dataframe(params_mortality_mean, "output/params_mortality_mean.csv")),
  
  NULL
  )
