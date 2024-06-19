Remove_FunDiv_country <- function(data_FunDiv, countries_rm) {
  
  data_FunDiv$trees <- data_FunDiv$trees %>% filter(!country %in% countries_rm)
  data_FunDiv$plots <- data_FunDiv$plots %>% filter(!country %in% countries_rm)
  
  return(data_FunDiv)
}