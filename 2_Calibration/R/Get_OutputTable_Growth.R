Get_OutputTable_Growth <- function(fit_growth, data_growth) {
  
  # Get occurence of individuals within countries for each species
  table_country <- data_growth %>% 
    dplyr::group_by(species, country) %>% 
    dplyr::summarise(n = n()) %>% 
    dplyr::ungroup()
  
  # Output dataframe
  cur_sp <- NA
  cur_n_sp <- 0
  n_sp <- length(fit_growth)
  
  out <- fit_growth %>% 
    purrr::map_depth(
      5,
      function(x) {

        # Print current species
        if (is.na(cur_sp) || cur_sp != x$species) {
          cur_sp <<- x$species
          cur_n_sp <<- cur_n_sp + 1
          message(cur_sp, " - ", cur_n_sp, "/", n_sp)
        }

        # Create row
        out_tmp <- data.frame(
          species = x$species,
          sample = x$sample,
          fold = x$fold,
          mod_id = x$mod_id,
          submod_id = x$submod_id,
          formula_full = x$formula_full,
          formula = x$formula,
          weight = x$weight,
          aic = x$AICc,
          mae = x$error[["MAE"]],
          rmse = x$error[["RMSE"]],
          coef.sigma = x$sigma
        )
        
        # Compute country intercept as country specific weighted mean by number of observations within each country
        table_country_sp <- table_country %>% 
          dplyr::filter(species == x$species)
        
        country_intercept <- x$coefs["(Intercept)","Value"]
        
        if (nrow(table_country_sp) > 1) {
          country_intercept <- country_intercept + 
            data.frame(
              var = rownames(x$coefs),
              coef = x$coefs[,"Value"]
            ) %>% 
            dplyr::filter(startsWith(var, "country")) %>% 
            dplyr::mutate(country = sub("country", "", var)) %>% 
            dplyr::right_join(table_country_sp, by = "country") %>% 
            dplyr::mutate(coef = coalesce(coef, 0)) %>% 
            dplyr::summarise(intercept = weighted.mean(coef, n)) %>% 
            dplyr::pull(intercept)
        }
        
        out_tmp$coef.intercept.est <- country_intercept
        
        # Add other coefs and associated p-value
        out_tmp <- out_tmp %>% 
          dplyr::bind_cols(
            data.frame(as.list(x$coefs[,"Value"])) %>% 
              dplyr::select(-contains("country"), -'X.Intercept.') %>% 
              dplyr::rename_all(~paste0("coef.", ., ".est"))
          ) %>% 
          dplyr::bind_cols(
            data.frame(as.list(x$coefs[,"p-value"])) %>% 
              dplyr::select(-contains("country"), -'X.Intercept.') %>% 
              dplyr::rename_all(~paste0("coef.", ., ".pval"))
          ) %>% 
          dplyr::rename_all(~gsub("I\\.sgdd\\.2\\.", "sgdd2", .))
        
        out_tmp
      }
    )

  # Bind recursively dataframes from the nested output list
  out <- out %>% 
    purrr::map(~purrr::map(.x, ~purrr::map(.x, ~purrr::map(.x, bind_rows)))) %>% 
    purrr::map(~purrr::map(.x, ~purrr::map(.x, bind_rows))) %>% 
    purrr::map(~purrr::map(.x, bind_rows)) %>% 
    purrr::map(bind_rows) %>% 
    dplyr::bind_rows()
             
  # Add sgdd model and compet model type
  out <- out %>%     
    dplyr::mutate(
      type_sgdd = ifelse(grepl("I\\(sgdd\\^2\\)", formula_full), "poly", "inv"),
      type_compet = case_when(
        grepl("bal", formula_full) ~ "bal",
        grepl("bat:dbh", formula_full) ~ "batXdbh",
        grepl("bat", formula_full) ~ "bat",
        grepl("lci", formula_full) ~ "lci",
        TRUE ~ "control"
      ),
      .before = formula_full
    )
    
  return(out)
}