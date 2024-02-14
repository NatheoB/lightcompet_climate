#' ## Properties
#' 
#' |Name     |Description                                                                        |Mapped units   | Conversion factor|Conventional units |
#' |:--------|:----------------------------------------------------------------------------------|:--------------|-----------------:|:------------------|
#' |bdod     |Bulk density of the fine earth fraction                                            |cg/cm^3        |               100|kg/dm^3            |
#' |cec      |Cation Exchange Capacity of the soil                                               |mmol(c)/kg     |                10|cmol(c)/kg         |
#' |cfvo     |Volumetric fraction of coarse fragments (> 2 mm)                                   |cm^3/dm^3 (vol per mil)  |         10|cm^3/100cm^3 (vol%)|
#' |clay     |Proportion of clay particles (< 0.002 mm) in the fine earth fraction               |g/kg           |                10|g/100g (%)         |
#' |nitrogen |Total nitrogen (N)                                                                 |cg/kg          |               100|g/kg               |
#' |phh2o    |Soil pH                                                                            |pH*10          |                10|pH                 |
#' |sand     |Proportion of sand particles (> 0.05 mm) in the fine earth fraction                |g/kg           |                10|g/100g (%)         |
#' |silt     |Proportion of silt particles (= 0.002 mm and = 0.05 mm) in the fine earth fraction |g/kg           |                10|g/100g (%)         |
#' |soc      |Soil organic carbon content in the fine earth fraction                             |dg/kg          |                10|g/kg               |
#' |ocd      |Organic carbon density                                                             |hg/m^3         |                10|kg/m^3             |
#' |ocs      |Organic carbon stocks                                                              |t/ha           |                10|kg/m^2             |
#' 

conversion_factor <- c(
  "bdod" = 100, "cec" = 10, "cfvo" = 10, "clay" = 10, 
  "nitrogen" = 100, "phh2o" = 10, "sand" = 10, "silt" = 10, 
  "soc" = 10, "ocd" = 10, "ocs" = 10
)


Rescale_SoilGrids <- function(data_sg, save_file) {
  
    # Rescale each column of the soilgrids dataset
  data_sg_rescaled <- as.data.frame(sapply(names(data_sg %>% select(-plotcode)), function(X) {
      # var is the first part of the colname (e.g. cec_0-5cm_mean)
    var <- strsplit(X, "_")[[1]][1]
      # value is the third part of the colname (e.g. cec_0-5cm_mean)
    value <- strsplit(X, "_")[[1]][3]
    
    if (value == "mean") {
        # Divide the raw value by the conversion factor for means
      round(data_sg[,X]/conversion_factor[[var]], digits = 5)
    } else if (value == "uncertainty") {
        # Divide by 10
      round(data_sg[,X]/10, digits = 5)
    }
      
  }))
  
    # Rebind plotcode
  data_sg_rescaled <- cbind(plotcode = data_sg$plotcode, data_sg_rescaled)
  
  return(data_sg_rescaled)
}



