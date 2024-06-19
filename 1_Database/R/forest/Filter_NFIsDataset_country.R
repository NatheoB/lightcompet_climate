Filter_NFIsDataset_country <- function(data_NFIs, vect_reduce_prop_countries, seed) {
  
  set.seed(seed)
  
  # Get countries in dataset
  vect_country_codes <- unique(data_NFIs$plots$country)
  
  # Set prop = 1 (do not reduce) for countries not in vect_reduce_prop_countries
  df_prop_countries <- data.frame(
    country = vect_country_codes,
    prop = rep(1, length(vect_country_codes))
  )
  df_prop_countries$prop[df_prop_countries$country %in% names(vect_reduce_prop_countries)] <- vect_reduce_prop_countries
  
  # Reduce datasets by randomly selecting rows in the proportion given by df_prop_countries for each country
  data_NFIs$plots <- data_NFIs$plots %>%
    dplyr::group_by(country) %>% 
    tidyr::nest() %>% 
    dplyr::ungroup() %>% 
    dplyr::left_join(df_prop_countries, by = "country") %>% 
    dplyr::mutate(samp = purrr::pmap(list(.data = data, prop = prop), slice_sample)) %>% 
    dplyr::select(-data) %>% 
    tidyr::unnest(samp) %>% 
    dplyr::select(-prop)
  
  data_NFIs$trees <- data_NFIs$trees %>% 
    dplyr::filter(plotcode %in% unique(data_NFIs$plots$plotcode))
  
  return(data_NFIs)
}


# Compare filter with dplyr::filter or with dplyr::right_join and select

# Filter_plotcodes_with_Filter <- function(data_plots_filtered, data_trees) {
#   
#   data_trees %>% 
#     dplyr::filter(plotcode %in% unique(data_plots_filtered$plotcode))
# 
# }
# 
# Filter_plotcodes_with_Join <- function(data_plots_filtered, data_trees) {
#   
#   data_trees %>% 
#     dplyr::right_join(data_plots_filtered, by = "plotcode") %>% 
#     dplyr::select(all_of(names(data_trees)))
#   
# }
# 
# data_plots_filtered <- data_plots %>% 
#   dplyr::sample_frac(0.5)
# 
# data_trees_filtered_filter <- Filter_plotcodes_with_Filter(data_plots_filtered, data_trees)
# data_trees_filtered_join <- Filter_plotcodes_with_Join(data_plots_filtered, data_trees)
# all.equal(data_trees_filtered_filter, data_trees_filtered_join)
# 
# microbenchmark::microbenchmark(
#   filter = Filter_plotcodes_with_Filter(data_plots_filtered, data_trees),
#   join = Filter_plotcodes_with_Join(data_plots_filtered, data_trees),
#   times = 10
# )

