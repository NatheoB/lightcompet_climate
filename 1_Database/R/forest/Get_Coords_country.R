Get_Coords_country <- function(data_list, country_str) {
  data_list$plots %>% filter(country == country_str) %>% select(plotcode, country, longitude, latitude)
}