#' @description Create a single filtered and merged dataset from all Samsara calibration datasets within INRAE database
#'
#' @return data.frame - Global dataset used to construct Samsara growth and mortality calibration datasets
#' 
Create_GlobalDataset <- function() {

  # Get login and password from .Renviron file ----
  # You have to create a file called ".Renviron" with :
  # LOGIN = "yourlogin"
  # PW = "yourpw"
  # Do not forget to .gitignore the file to not share your logs
  login <- Sys.getenv("LOGIN")
  password <- Sys.getenv("PASSWORD")
  
  
  print("Importing datasets from database...")
  # Import all datasets from INRAE database ----
  datasets_name <- c("calib_plots", "calib_trees",
                     "calib_compet", "calib_climate")
  
  datasets_list <- sapply(datasets_name, 
                          Get_SamsaraDataset_from_PgAdmin, login, password, 
                          simplify = T, USE.NAMES = T)

  
  print("Filtering plots/trees from SamsaraLight outputs...")
  # Remove trees/plots where light has not been computed ----
  datasets_list[["calib_trees"]] <- 
    Filter_DataTrees_from_SamsaLightOutputs(datasets_list[["calib_trees"]],
                                            datasets_list[["calib_compet"]])


  print("Merging all datasets...")
  # Bind datasets ----
  dataset_global <- Merge_Datasets_from_DataTrees(datasets_list[["calib_trees"]],
                                                  datasets_list[["calib_plots"]],
                                                  datasets_list[["calib_compet"]],
                                                  datasets_list[["calib_climate"]])
  
  
  dataset_global
}