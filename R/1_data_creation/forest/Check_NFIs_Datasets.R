Check_NFIs_Datasets <- function(data_NFIs) {
  
  ####### TREES DATASET ######

  ### CHECK IF UNIQUE TREECODE 
  print("treecode")
  if (nrow(data_NFIs$trees) != length(unique(data_NFIs$trees$treecode))) stop("Not unique treecode")
  
  
  ### SPECIES VARIABLE
  print("species variable")

    # Find plots with at least one NA species
  plotcodes_naspecies <- unique(data_NFIs$trees %>% filter(is.na(species)) %>% pull(plotcode))
  print(length(plotcodes_naspecies))
  
    # Return error if unclean
  if (length(plotcodes_naspecies) > 0) stop("NA species")


  
  ### TREESTATUS VARIABLE
  print("treestatus variable")

    # Check NA treestatus
  treecodes_natreestatus <- data_NFIs$trees %>% filter(is.na(treestatus)) %>% pull(treecode)
  print(length(treecodes_natreestatus))
  
    # Return error if unclean
  if (length(treecodes_natreestatus) > 0) stop("NA treestatus")

  
  
  ### DBH1 VARIABLE
  
  print("dbh1 variable NAs")

    # Find NA dbh1
  rows_nadbh1 <- data_NFIs$trees %>% filter(is.na(dbh1))

    # Check which tree status are NA DBH
  tab_nadbh1 <- table(rows_nadbh1$treestatus, useNA = "always")
  print(tab_nadbh1) # Modality 1 (ingrowth ==> OK)

    # Return error if unclean
  if (sum(!names(tab_nadbh1) %in% c("1", NA)) > 0 | tail(tab_nadbh1, 1) > 0) stop("Wrong NA dbh1")
  
    # If OK, set all NAs to 0
  data_NFIs$trees <- data_NFIs$trees %>% 
    dplyr::mutate(dbh1 = replace(dbh1, is.na(dbh1), 0))
  
  print(table(data_NFIs$trees %>% 
                dplyr::filter(is.na(dbh1)) %>% 
                dplyr::pull(treestatus), useNA = "always")) # OK
  
  
  
  print("dbh1 variable is 0")
  
    # Check 0 dbh1
  rows_0dbh1 <- data_NFIs$trees %>% filter(dbh1 == 0)
  
    # Check which tree status are 0
  tab_0dbh1 <- table(rows_0dbh1$treestatus, useNA = "always")
  print(tab_0dbh1) # Modality 1, 3, 4 (3 and 4 ==> not OK)
  
    # Return error if unclean
  if (sum(!names(tab_0dbh1) %in% c("1", NA)) > 0 | tail(tab_0dbh1, 1) > 0) stop("Wrong dbh1 equal 0")
  
  
  
  ### DBH2 VARIABLE
  
  print("dbh2 variable NAs")
  
  # Find NA dbh2
  rows_nadbh2 <- data_NFIs$trees %>% dplyr::filter(is.na(dbh2))
  
  # Check which tree status are NA DBH
  tab_nadbh2 <- table(rows_nadbh2$treestatus, useNA = "always")
  print(tab_nadbh2) # Modality 3, 4, 5, 6 (dead or missing trees ==> OK)
  
  # Return error if unclean
  if (sum(!names(tab_nadbh2) %in% c("3", "4", "5", "6", NA)) > 0 | tail(tab_nadbh2, 1) > 0) stop("Wrong NA dbh2")
  
  # If OK, set all NAs to 0
  data_NFIs$trees <- data_NFIs$trees %>% 
    dplyr::mutate(dbh2 = replace(dbh2, is.na(dbh2), 0))
  
  print(table(data_NFIs$trees %>% 
                dplyr::filter(is.na(dbh2)) %>% 
                dplyr::pull(treestatus), useNA = "always")) # OK
  
  
  
  print("dbh2 variable is 0")
  
  # Check 0 dbh2
  rows_0dbh2 <- data_NFIs$trees %>% filter(dbh2 == 0)
  
  # Check which tree status are 0
  tab_0dbh2 <- table(rows_0dbh2$treestatus, useNA = "always")
  print(tab_0dbh2) # Modality 3, 4, 5, 6
  
  # Return error if unclean
  if (sum(!names(tab_0dbh2) %in% c("3", "4", "5", "6", NA)) > 0 | tail(tab_0dbh2, 1) > 0) stop("Wrong dbh2 equal 0")

  
  
  
  ### WEIGHT1 VARIABLE
  
  print("weight1 variable NAs")

    # Find NA weight1
  rows_naweight1 <- data_NFIs$trees %>% filter(is.na(weight1))

    # Check which tree status are NA weight1
  tab_naweight1 <- table(rows_naweight1$treestatus, useNA = "always")
  print(tab_naweight1) # 1 : ingrowth OK

    # Return error if unclean
  if (sum(!names(tab_naweight1) %in% c("1", NA)) > 0 | tail(tab_naweight1, 1) > 0) stop("Wrong NA weight1")
  
    # If OK, set all NAs to 0
  data_NFIs$trees <- data_NFIs$trees %>% 
    dplyr::mutate(weight1 = replace(weight1, is.na(weight1), 0))
  
  print(table(data_NFIs$trees %>% 
                dplyr::filter(is.na(weight1)) %>% 
                dplyr::pull(treestatus), useNA = "always")) # OK
  
  
  
  print("weight1 variable 0s")
  
    # Check 0 weight1
  rows_0w1 <- data_NFIs$trees %>% filter(weight1 == 0)
  tab_0w1 <- table(rows_0w1$treestatus, useNA = "always")
  print(tab_0w1) # Modality 1 ngrowth ==> OK
  
    # Return error if unclean
  if (sum(!names(tab_naweight1) %in% c("1", NA)) > 0 | tail(tab_naweight1, 1) > 0) stop("Wrong weight1 equal 0")
  
  
  
  ### WEIGHT2 VARIABLE
  
  print("weight2 variable NAs")
  
    # Find NA weight2
  rows_naweight2 <- data_NFIs$trees %>% filter(is.na(weight2))
  
    # Check which tree status are NA weight2
  tab_naweight2 <- table(rows_naweight2$treestatus, useNA = "always")
  print(tab_naweight2) # 3, 4, 5, 6 : dead or missing OK
  
    # Return error if unclean
  if (sum(!names(tab_naweight2) %in% c("3", "4", "5", "6", NA)) > 0 | tail(tab_naweight2, 1) > 0) stop("Wrong NA weight2")
  
    # If OK, set all NAs to 0
  data_NFIs$trees <- data_NFIs$trees %>% 
    dplyr::mutate(weight2 = replace(weight2, is.na(weight2), 0))
  
  print(table(data_NFIs$trees %>% 
                dplyr::filter(is.na(weight2)) %>% 
                dplyr::pull(treestatus), useNA = "always")) # OK
  
  
  
  print("weight2 variable 0s")
  
    # Check 0 weight2
  rows_0w2 <- data_NFIs$trees %>% filter(weight2 == 0)
  tab_0w2 <- table(rows_0w2$treestatus, useNA = "always")
  print(tab_0w2) # 3, 4, 5, 6 : dead or missing OK
  
  # Return error if unclean
  if (sum(!names(tab_0w2) %in% c("3", "4", "5", "6", NA)) > 0 | tail(tab_0w2, 1) > 0) stop("Wrong weight2 equal 0")

  
  

  ###### PLOTS DATASET ######

  ### CHECK IF UNIQUE PLOTCODE 
  print("plotcode")
  if (nrow(data_NFIs$plots) != length(unique(data_NFIs$plots$plotcode))) stop("Not unique plotcode")
  
  ### LONGITUDE AND LATITUDE VARIABLES
  print("longitude and latitude variables")

    # Check plots with NA longitude or latitude
  plots_nalonglat <- data_NFIs$plots %>% filter(is.na(longitude) | is.na(latitude)) %>% pull(plotcode)
  print(plots_nalonglat) # 0 plot

    # Error if unclean
  if (length(plots_nalonglat) > 0) stop("NA coords")
  
  
  ### ALTITUDE VARIABLE
  print("altitude variable")
  
  country_naalt <- unique(data_NFIs$plots %>% filter(is.na(altitude)) %>% pull(country))
  print(country_naalt)
  
  prob_naalt_country <- setdiff(country_naalt, c("FI", "SW", "WA"))
  if (length(prob_naalt_country) > 0) stop(paste("NA alt in country :", paste(prob_naalt_country, collapse = "-")))
  
  
  ### SURVEYDATE VARIABLES
  print("surveydate variables")
  
    # Check plots with NA surveydate1 (but no NA surveydate2)
  plots_nasurveydates <- data_NFIs$plots %>% filter(is.na(surveydate1) | is.na(surveydate2)) %>% pull(plotcode)
  
    # Error if unclean
  if (length(plots_nasurveydates) > 0) stop("NA dates")
  
  
  return(data_NFIs)
}




