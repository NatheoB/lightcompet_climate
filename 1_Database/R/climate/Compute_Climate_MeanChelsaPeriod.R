Compute_Climate_MeanChelsaPeriod <- function(waterbalance_years, water_balance_params,
                                             climate_proxy_years,
                                             chelsa_years) {
  
  print("Binding years dataframes...")
  # Select the interest variables for each yearly dataframe and bind them into a single one
  
  ## Get Water balance variables for each water balance parameters (i.e. var.param)
  ## And climatic proxy variables for each years
  vars_wb <- expand.grid(var = c("swci", "aet2pet", "swd"),
                         param = water_balance_params$id) %>%
    tidyr::unite("vars", c("var", "param"), sep = ".") %>%
    dplyr::pull(vars)
  
  vars_proxy <- c("sgdd",
                  expand.grid(var = c("tasmin", "tasmax",
                                      "pr", 
                                      "wa", "wai", "wd",
                                      "lpr_MM", "spr_MM", "snostor_MM",
                                      "lpr_DD", "spr_DD", "snostor_DD",
                                      "lw_MM", "lwa_MM", "lwai_MM", "lwd_MM",
                                      "lw_DD", "lwa_DD", "lwai_DD", "lwd_DD"),
                              param = c("", ".summer", ".winter", ".spring", ".autumn", ".gs")) %>%
                    tidyr::unite("vars", c("var", "param"), sep = "") %>%
                    dplyr::pull(vars)
  )
  
  vars <- c(vars_proxy, vars_wb)
  
  ## Create a variable for each year (var_year)
  vars_years <- expand.grid(var = vars, year = chelsa_years) %>%
    dplyr::arrange(var) %>% 
    tidyr::unite("var_year", c(var, year), sep = "_") %>% 
    dplyr::pull(var_year)
  
  
  print("Averaging years dataframes...")
  ## Select, rename and average variables for each chelsa year
  df_wb <- waterbalance_years %>%
    purrr::map(~.$data %>% 
                 dplyr::select(plotcode, 
                               dplyr::any_of(vars_years)) %>% 
                 dplyr::rename_at(vars(any_of(vars_years)),
                                  list(~substr(.x,1,nchar(.x)-5))))
  df_wb <- tibble(rbindlist(df_wb)[,lapply(.SD,mean), plotcode])
  
  df_proxy <- climate_proxy_years %>%
    purrr::map(~.$data %>% 
                 dplyr::select(plotcode, 
                               dplyr::any_of(vars_years)) %>% 
                 dplyr::rename_at(vars(any_of(vars_years)),
                                  list(~substr(.x,1,nchar(.x)-5))))
  df_proxy <- tibble(rbindlist(df_proxy)[,lapply(.SD,mean), plotcode])
  
  
  ## Bind the two dataframes and add suffix
  dplyr::left_join(df_wb, df_proxy, by = "plotcode") %>% 
    dplyr::rename_at(vars(contains(vars)), ~paste0(.x, "_chelsaperiod"))

}