Merge_SRTM30_List <- function(srtm30_list) {
    # Set colnames of dataframes
  srtm30_list <- lapply(srtm30_list, function(X) {
    names(X) <- c("plotcode", "srtm30")
    X
  })
  
    # Bind by rows and remove row_names
  srtm30_df <- dplyr::bind_rows(srtm30_list)
  rownames(srtm30_df) <- NULL
  
  return(srtm30_df)
}