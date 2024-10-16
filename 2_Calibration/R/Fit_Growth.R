Fit_Growth <- function(sp = "Abies alba",
                       data_gr,
                       id_sample,
                       resampling_weighted_by = c("dbh", "aet2pet", "sgdd"),
                       prop_resampling = 0.8,
                       n_folds = 5,
                       use_dredge = TRUE,
                       get_only_coefs = FALSE,
                       compet_type = c("lci", "bat", "batXdbh", "bal", "control"),
                       seed = 38
) {
  
  print(paste("Growth", sp, "sample", id_sample))
  
  set.seed(seed + id_sample)
  
  # Filter dataset
  data_gr_sp <- data_gr %>% 
    dplyr::filter(species == sp)
  
  countries <- unique(data_gr_sp$country)
  
  ### Weight individuals by rarity ----
  
  # Compute weight for each variable
  n_boxes <- 5
  for (var_weight in resampling_weighted_by) {
    
    # Split range into n boxes of same length between min and max values
    range_var <- range(data_gr_sp[[var_weight]])
    step_var <- (range_var[2] - range_var[1]) / n_boxes 
    
    # Find group of each individuals and assign a probability as the inverse of the number of individuals within the class
    data_gr_sp <- data_gr_sp %>% 
      dplyr::mutate("group_{var_weight}" := case_when(
        !!sym(var_weight) == range_var[1] ~ 1,
        !!sym(var_weight) == range_var[2] ~ n_boxes,
        TRUE ~ ceiling((!!sym(var_weight) - range_var[1]) / step_var)
      ))
    
  }
  
  # Combine groups
  if (length(resampling_weighted_by) > 0) {
    
    data_gr_sp <- data_gr_sp %>% 
      tidyr::unite(sampling_group, 
                   paste0("group_", resampling_weighted_by), 
                   remove = F) %>% 
      dplyr::group_by(sampling_group) %>% 
      dplyr::mutate(n_ind_sampling = n()) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(sampling_prob = 1/n_ind_sampling)
    
  } else {
    
    data_gr_sp$sampling_prob <- 1
    
  }
  
  ### Resample the dataset based on sampling variables ----
  data_sampling <- data_gr_sp %>% 
    dplyr::group_by(country) %>% 
    dplyr::sample_frac(prop_resampling, weight = sampling_prob)
  
  # hist(data_gr_sp$aet2pet, xlim = c(-3, 1.5), ylim = c(0, 50000))
  # hist(data_sampling$aet2pet, xlim = c(-3, 1.5), ylim = c(0, 50000))
  # hist(data_gr_sp$sgdd, xlim = c(-2.3, 2.3), ylim = c(0, 55000))
  # hist(data_sampling$sgdd, xlim = c(-2.3, 2.3), ylim = c(0, 55000))
  # hist(data_gr_sp$dbh, xlim = c(-2, 6), ylim = c(0, 85000))
  # hist(data_sampling$dbh, xlim = c(-2, 6), ylim = c(0, 85000))
  
  
  ### Set k folds ----
  # random same number of trees within each country and dead/alive modality
  data_sampling <- data_sampling %>% 
    dplyr::group_by(country) %>% 
    dplyr::mutate(group = sample(c(rep(1:n_folds, n()%/%n_folds), 
                                   sample(1:n_folds, n()%%n_folds))))
  
  
  ### Fit folds ----
  mods_gr_sp_sample <- vector(mode = "list", length = n_folds)
  for (f in 1:n_folds) {
    
    ### Split dataset into train and test ----
    data_gr_train <- data_sampling %>% 
      dplyr::mutate(test = group==f)
    
    data_gr_test <- data_gr_train %>% dplyr::filter(test)
    data_gr_train <- data_gr_train %>% dplyr::filter(!test)
    
    
    ### Get weights of each model ----
    
    # Check if one or more country modalities
    # If one, remove country variable
    n_countries <- length(unique(data_gr_sp$country))
    formula_country <- ifelse(n_countries > 1,
                              " + country",
                              "")
    
    # Full model with all variables and interactions
    formula_base <- "dbh + dbh_log + aet2pet_inv"
    
    ## Add given competition index
    formulas_sgdd_compet <- c()
    
    ### Control model
    if ("control" %in% compet_type) formulas_sgdd_compet <- c(formulas_sgdd_compet, 
                                                              " + sgdd_inv",
                                                              " + sgdd + I(sgdd^2)")
    
    ### Basal area of larger trees
    if ("bal" %in% compet_type) formulas_sgdd_compet <- c(formulas_sgdd_compet, 
                                                          " + sgdd_inv + bal + bal:aet2pet_inv + bal:sgdd_inv",
                                                          " + sgdd + I(sgdd^2) + bal + bal:aet2pet_inv + bal:sgdd")
    
    ### Total basal area
    if ("bat" %in% compet_type) formulas_sgdd_compet <- c(formulas_sgdd_compet, 
                                                          " + sgdd_inv + bat + bat:aet2pet_inv + bat:sgdd_inv",
                                                          " + sgdd + I(sgdd^2) + bat + bat:aet2pet_inv + bat:sgdd")
    
    ### Total basal area with intercation with dbh
    if ("batXdbh" %in% compet_type) formulas_sgdd_compet <- c(formulas_sgdd_compet, 
                                                              " + sgdd_inv + bat + bat:aet2pet_inv + bat:sgdd_inv + bat:dbh",
                                                              " + sgdd + I(sgdd^2) + bat + bat:aet2pet_inv + bat:sgdd + bat:dbh")
    
    ### Light competition index
    if ("lci" %in% compet_type) formulas_sgdd_compet <- c(formulas_sgdd_compet, 
                                                          " + sgdd_inv + lci + lci:aet2pet_inv + lci:sgdd_inv",
                                                          " + sgdd + I(sgdd^2) + lci + lci:aet2pet_inv + lci:sgdd")
    
    ## Add common variables
    formulas_full <- paste0(formula_base, formulas_sgdd_compet, formula_country)
    
    
    
    # Fit and dredge all full models
    n_formulas_full <- length(formulas_full)
    if (n_formulas_full == 0) stop("no formula to fit")
    
    mods_gr_sp_sample_fold <- vector(mode = "list", length = n_formulas_full)
    for (i_formula in 1:n_formulas_full) {
      
      formula_full <- paste0("dD_log ~ ", formulas_full[i_formula])
      # print(paste0("species ", sp,
      #              " - sample ", id_sample,
      #              " - fold ", f, "/", n_folds,
      #              " - formula ", i_formula, "/", n_formulas_full))
      # print(formula_full)
      
      # Apply dredge on the given full formula
      if (use_dredge) {
        
        # message("Dredging...")
        
        mod_full_gr <- lme(
          fixed = as.formula(formula_full),
          random = ~1|plotcode,
          data = data_gr_train,
          method = "ML",
          control = lmeControl(opt = "optim")
        )
        
        # Test AICc of all submodels (fitting by ML)
        fixed_dredge <- c("dbh", "dbh_log", "lci", "bat", "bal", "bat:dbh")
        if (n_countries > 1) fixed_dredge <- c(fixed_dredge, "country")
        
        options(na.action = "na.fail")
        if (grepl("I\\(sgdd\\^2\\)", formulas_full[i_formula])) {
          # If quadratic term, force to have always both terms of the polynom
          out_dredge <- dredge(
            mod_full_gr,
            beta = "none",
            rank = "AICc",
            fixed = fixed_dredge,
            subset = "sgdd" & "I(sgdd^2)"
          )
        } else {
          out_dredge <- dredge(
            mod_full_gr,
            beta = "none",
            rank = "AICc",
            fixed = fixed_dredge
          )
        }
        options(na.action = "na.omit")
        
        # Get best models (AICc <= 2), their associated AICc weight and rescale weights
        formulas_weighted_gr <- get_weighted_formulas(out_dredge)
      } else {
        
        formulas_weighted_gr <- data.frame(
          id = 1,
          formula = formulas_full[i_formula],
          weight = 1
        )
        
      }
      
      
      ## Run, test and store the models
      mods_gr_sp_sample_fold_formulafull <- vector(mode = "list", length = nrow(formulas_weighted_gr))
      for (i in 1:nrow(formulas_weighted_gr)) {
        
        # message("mod ", i, "/", nrow(formulas_weighted_gr))
        
        # Init output list
        mods_gr_sp_sample_fold_formulafull[[i]] <- 
          list(species = sp, sample = id_sample, fold = f, 
               mod_id = i_formula, submod_id = i,
               samplesize = nrow(data_gr_train),
               formula_full = formulas_full[i_formula],
               formula = formulas_weighted_gr$formula[i],
               weight = formulas_weighted_gr$weight[i],
               AICc = formulas_weighted_gr$AICc[i],
               mod = NULL, error = NULL, coefs = NULL,
               sigma = NULL)
        
        # Run model fit on train dataset by REML optimization
        # https://stackoverflow.com/questions/63680096/using-formula-to-predict-inside-of-r-function-generates-object-not-found-error
        # CAREFUL : predict.lme do not accept formulas as string
        mod <- do.call("lme", list(fixed = as.formula(paste0("dD_log ~", formulas_weighted_gr$formula[i])),
                                   random = ~1|plotcode,
                                   data = data_gr_test,
                                   method = "REML",
                                   control = lmeControl(opt = "optim")))
        
        if (get_only_coefs) {
          mods_gr_sp_sample_fold_formulafull[[i]]$coefs <- summary(mod)$tTable
        } else {
          mods_gr_sp_sample_fold_formulafull[[i]]$mod <- mod
        }
        
        if (nrow(data_gr_test) > 0) {
          
          # Predict the survival probability of test dataset (prediction using new data)
          data_gr_test$pred <- 
            predict(mod, 
                    newdata = data_gr_test,
                    level = 0)
          
          # Compute MAE (mean absolute error) and RMSE (root mean standard error) on predictions VS observations
          sigma_mod <- sigma(mod)
          mods_gr_sp_sample_fold_formulafull[[i]]$sigma <- sigma_mod
          
          df_errors <- data_gr_test %>%
            dplyr::mutate(
              pred_unlog = exp(pred)*exp(sigma_mod^2/2),
              
              AE = abs(pred_unlog - dD),
              SE = (pred_unlog - dD)^2
            )
          
          mods_gr_sp_sample_fold_formulafull[[i]]$error <- c(
            "MAE" = mean(df_errors$AE),
            "RMSE" = sqrt(mean(df_errors$SE))
          )
        }
        
      }
      
      mods_gr_sp_sample_fold[[i_formula]] <- mods_gr_sp_sample_fold_formulafull
    }
    
    mods_gr_sp_sample[[f]] <- mods_gr_sp_sample_fold 
  }
  
  mods_gr_sp_sample
}