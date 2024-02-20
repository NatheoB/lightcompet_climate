Fit_Mortality <- function(sp = "Abies alba",
                          data_morta,
                          n_samples = 2,
                          resampling_weighted_by = c("dbh", "aet2pet", "sgdd"),
                          prop_resampling = 0.8,
                          n_folds = 5,
                          use_dredge = TRUE,
                          get_only_coefs = FALSE,
                          seed = 38
) {
  
  print(sp)
  
  set.seed(seed)
  
  # Filter dataset
  data_morta_sp <- data_morta %>% 
    dplyr::filter(species == sp)
  
  countries <- unique(data_morta_sp$country)
  
  ### Weight individuals by rarity ----
  
  # Compute weight for each variable
  n_boxes <- 5
  for (var_weight in resampling_weighted_by) {
    
    # Split range into n boxes of same length between min and max values
    range_var <- range(data_morta_sp[[var_weight]])
    step_var <- (range_var[2] - range_var[1]) / n_boxes 
    
    # Find group of each individuals and assign a probability as the inverse of the number of individuals within the class
    data_morta_sp <- data_morta_sp %>% 
      dplyr::mutate("group_{var_weight}" := case_when(
        !!sym(var_weight) == range_var[1] ~ 1,
        !!sym(var_weight) == range_var[2] ~ n_boxes,
        TRUE ~ ceiling((!!sym(var_weight) - range_var[1]) / step_var)
      ))
    
  }
  
  # Combine groups
  if (length(resampling_weighted_by) > 0) {
    
    data_morta_sp <- data_morta_sp %>% 
      tidyr::unite(sampling_group, 
                   paste0("group_", resampling_weighted_by), 
                   remove = F) %>% 
      dplyr::group_by(sampling_group) %>% 
      dplyr::mutate(n_ind_sampling = n()) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(sampling_prob = 1/n_ind_sampling)
    
  } else {
    
    data_morta_sp$sampling_prob <- 1
    
  }
  
  
  ## Fit samples ----
  mods_morta_sp <- vector(mode = "list", length = n_samples)
  for (s in 1:n_samples) {
    
    ### Resample 70% of the dataset ----
    data_sampling <- data_morta_sp %>% 
      dplyr::group_by(country) %>% 
      dplyr::sample_frac(prop_resampling)
    
    
    ### Set k folds ----
    # random same number of trees within each country and dead/alive modality
    data_sampling <- data_sampling %>% 
      dplyr::group_by(country, dead) %>% 
      dplyr::mutate(group = sample(c(rep(1:n_folds, n()%/%n_folds), 
                                     sample(1:n_folds, n()%%n_folds))))
    
    
    ### Fit folds ----
    mods_morta_sp_sample <- vector(mode = "list", length = n_folds)
    for (f in 1:n_folds) {
      
      ### Split dataset into train and test ----
      data_morta_train <- data_sampling %>% 
        dplyr::mutate(test = group==f)
      
      data_morta_test <- data_morta_train %>% dplyr::filter(test)
      data_morta_train <- data_morta_train %>% dplyr::filter(!test)
      
      
      ### Get weights of each model ----
      
      # Check if one or more country modalities
      # If one, remove country variable
      n_countries <- length(unique(data_morta_sp$country))
      formula_country <- ifelse(n_countries > 1,
                                " + country",
                                "")
      
      # Full model with all variables and interactions
      formulas_full <- paste0(
        "dbh + dbh_log + aet2pet_inv",
        
        c(
          # Control
          " + sgdd_inv",
          " + sgdd + I(sgdd^2)",
          
          # Light competition index          
          " + lci + lci:aet2pet_inv + sgdd_inv + lci:sgdd_inv",
          " + lci + lci:aet2pet_inv + sgdd + I(sgdd^2) + lci:sgdd",
          
          # Basal area of larger trees
          " + bal + bal:aet2pet_inv + sgdd_inv + bal:sgdd_inv",
          " + bal + bal:aet2pet_inv + sgdd + I(sgdd^2) + bal:sgdd",
          
          # Total basal area
          " + bat + bat:aet2pet_inv + sgdd_inv + bat:sgdd_inv",
          " + bat + bat:aet2pet_inv + sgdd + I(sgdd^2) + bat:sgdd",
          
          # Total basal area and interaction with diameter
          " + bat + bat:dbh + bat:aet2pet_inv + sgdd_inv + bat:sgdd_inv",
          " + bat + bat:dbh + bat:aet2pet_inv + sgdd + I(sgdd^2) + bat:sgdd"
        ),
        
        formula_country
      )
      n_formulas_full <- length(formulas_full)
      
      mods_morta_sp_sample_fold <- vector(mode = "list", length = n_formulas_full)
      for (i_formula in 1:n_formulas_full) {
        
        # print(paste0("species ", sp, 
        #              " - sample ", s, "/", n_samples,
        #              " - fold ", f, "/", n_folds,
        #              " - formula ", i_formula, "/", n_formulas_full))
        
        if (use_dredge) {
          
          mod_full_morta <- 
            glm(formula = as.formula(paste0("dead ~ ", formulas_full[i_formula])),
                data = data_morta_train, 
                family = binomial(link=cloglog))
          
          
          # Test AICc of all submodels
          fixed_dredge <- c("dbh", "dbh_log", "offset(log(dyears))", "lci", "bat", "bal")
          if (n_countries > 1) fixed_dredge <- c(fixed_dredge, "country")
          
          options(na.action = "na.fail")
          if (grepl("I\\(sgdd\\^2\\)", formulas_full[i_formula])) {
            # If quadratic term, force to have always both terms of the polynom
            out_dredge <- dredge(
              mod_full_morta,
              beta = "none",
              rank = "AICc",
              fixed = fixed_dredge,
              subset = "sgdd" & "I(sgdd^2)"
            )
          } else {
            out_dredge <- dredge(
              mod_full_morta,
              beta = "none",
              rank = "AICc",
              fixed = fixed_dredge
            )
          }
          options(na.action = "na.omit")
          
          # Get best models (AICc <= 2), their associated AICc weight and rescale weights
          formulas_weighted_morta <- get_weighted_formulas(out_dredge)
          
        } else {
          
          formulas_weighted_morta <- data.frame(
            id = 1,
            formula = formulas_full[i_formula],
            weight = 1
          )
          
        }
        
        ## Run, test and store the models
        mods_morta_sp_sample_fold_formulafull <- vector(mode = "list", length = nrow(formulas_weighted_morta))
        for (i in 1:nrow(formulas_weighted_morta)) {
          
          # message("mod ", i, "/", nrow(formulas_weighted_morta))
          
          # Init output list
          mods_morta_sp_sample_fold_formulafull[[i]] <- 
            list(species = sp, sample = s, fold = f, 
                 mod_id = i_formula, submod_id = i,
                 samplesize = nrow(data_morta_train),
                 formula_full = formulas_full[i_formula],
                 formula = formulas_weighted_morta$formula[i],
                 weight = formulas_weighted_morta$weight[i],
                 AICc = formulas_weighted_morta$AICc[i],
                 mod = NULL, error = NULL, coefs = NULL)
          
          # Run and store the model
          mods_morta_sp_sample_fold_formulafull[[i]]$mod <- 
            glm(formula = paste(
              "dead ~", formulas_weighted_morta$formula[i]),
              data = data_morta_train, 
              family = binomial(link=cloglog))
          
          if (nrow(data_morta) > 0) {
            
            # Predict the survival probability of test dataset (prediction using new data)
            data_morta_test$pred <- 
              predict(mods_morta_sp_sample_fold_formulafull[[i]]$mod, 
                      newdata = data_morta_test, type = "response")
            
            # Compute AUC ROC of the predictions (control = dead, case = alive, because probability of survival)
            mods_morta_sp_sample_fold_formulafull[[i]]$error <- 
              as.numeric(pROC::roc(data_morta_test$dead, 
                                   data_morta_test$pred, quiet = TRUE)$auc)
            
          }
          
          
          if (get_only_coefs) {
            mods_morta_sp_sample_fold_formulafull[[i]]$coefs <- summary(mods_morta_sp_sample_fold_formulafull[[i]]$mod)$coefficients
            mods_morta_sp_sample_fold_formulafull[[i]]$mod <- NULL
          }
          
        }
        
        mods_morta_sp_sample_fold[[i_formula]] <- mods_morta_sp_sample_fold_formulafull
      }
      
      mods_morta_sp_sample[[f]] <- mods_morta_sp_sample_fold 
    }
    
    mods_morta_sp[[s]] <- mods_morta_sp_sample
  }
  
  
  mods_morta_sp
}