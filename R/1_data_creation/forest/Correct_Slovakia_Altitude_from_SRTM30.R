Correct_Slovakia_Altitude_from_SRTM30 <- function(data_slovakia, data_srtm30) {
  
  data_slovakia$plots <- data_slovakia$plots %>% 
    dplyr::left_join(data_srtm30 %>% dplyr::select(plotcode, srtm30), by = "plotcode") %>% 
    dplyr::mutate(altitude = srtm30) %>% 
    dplyr::select(-srtm30)
  
  data_slovakia
}