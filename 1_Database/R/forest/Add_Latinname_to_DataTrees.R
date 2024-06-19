Add_Latinname_to_DataTrees <- function(data_country_list, sp_id_corresp, colname.speciesid)
{
  
  # Add latinname to fundiv data_trees
  data_country_list$trees <- data_country_list$trees %>% 
    left_join(sp_id_corresp, by = colname.speciesid)
  
  return(data_country_list)
}
