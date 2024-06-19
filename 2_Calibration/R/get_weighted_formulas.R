# Function to get best formulas with their associated weight
get_weighted_formulas <- function(out_dredge) {
  
  tibble(out_dredge) %>% 
    dplyr::mutate(weight = as.numeric(weight)) %>% 
    dplyr::filter(delta <= 2) %>% 
    dplyr::select(-c(`(Intercept)`, df, logLik, delta)) %>% 
    dplyr::mutate(across(-c(weight, AICc), ~ifelse(!is.na(.), cur_column(), NA))) %>% 
    tidyr::unite(formula, -c(weight, AICc), sep = " + ", na.rm = T) %>% 
    dplyr::mutate(id = row_number(),
                  weight = weight/sum(weight)) # Normalize weight with sum of weights beign 1
  
}