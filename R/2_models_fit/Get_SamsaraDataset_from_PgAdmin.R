#' @description Import datasets from samsara scheme within INRAE database basedtm
#' 
#' @param dataset_name string - Name of the dataset to import
#' @param username string - Login for INRAE database access
#' @param password string - Password for INRAE database access
#' 
#' @return data.frame - Dataset imported from the database
#' 
#' @example 
#' dataset <- Get_SamsaraDataset_from_PgAdmin("calib_trees", "jean_dupont", "myPW")
#' 
Get_SamsaraDataset_from_PgAdmin <- function(dataset_name, username, password) {
  
  # Declaration of driver
  drv <- dbDriver("PostgreSQL")
  
  # Prepare connection
  con <- dbConnect(dbDriver("PostgreSQL"), 
                   dbname = "basedtm", 
                   host   = "siddt.grenoble.irstea.priv", 
                   port   = "5432", 
                   user   = username, 
                   password = password)
  
  # Declare encoding (often different from database in UTF8)
  postgresqlpqExec(con, "SET client_encoding = 'windows-1252'")
  
  # Write SQL request
  sql <- paste0("SELECT * FROM samsara.", dataset_name)
  
  # Run request
  print(paste0("Importing ", dataset_name, "..."))
  data <- dbGetQuery(con, sql)
  
  # Disconnect from db
  dbDisconnect(con)
  
  return(data)
}


