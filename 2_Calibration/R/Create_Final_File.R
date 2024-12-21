Create_Final_File <- function(out_growth, out_mortality,
                              attr_growth, attr_mortality,
                              data_mean_env,
                              data_shadetol,
                              output_fp) {
  
  list_of_df <- list("params_growth" = out_growth, 
                     "params_mortality" = out_mortality,
                     "scale_growth" = attr_growth, 
                     "scale_mortality" = attr_mortality,
                     "mean_env" = data_mean_env,
                     "shade_tolerance" = data_shadetol)
  
  openxlsx2::write_xlsx(list_of_df, output_fp, na.strings = "NA")
}