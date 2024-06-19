# https://bsssjournals.onlinelibrary.wiley.com/doi/full/10.1111/ejss.12192

#### (12)	θWP
# cm3cm-3	
# LRt	
# θWP =  0.09878 + 0.002127* Cl - 0.0008366 * Si - 0.07670 *(1/(OC+1)) + 0.00003853 * Si * Cl + 0.002330 * Cl * (1/(OC+1)) + 0.0009498 * Si * (1/(OC+1))

#### (9)	θFC
# cm3cm-3	
# LRt	
# θFC = 0.2449 - 0.1887 * (1/(OC+1)) + 0.004527 * Cl + 0.001535 * Si + 0.001442 * Si * (1/(OC+1)) - 0.00005110 * Si * Cl + 0.0008676 * Cl * (1/(OC+1))


Compute_SoilGridsDerived_SWHCProp <- function(data_soil, depths) {
  
  swhc_prop <- sapply(depths, function(depth) {
    
      # Get variables for the given depth
    OC <- data_soil[[paste("soc", depth, "mean", sep = "_")]] / 1000 * 100 # soc in g/kg but OC in formula is in %
    Cl <- data_soil[[paste("clay", depth, "mean", sep = "_")]]
    Si <- data_soil[[paste("silt", depth, "mean", sep = "_")]]
    
      # Compute water content at field capacity (cm3cm-3)
    thetaFC <-
      0.2449 - 0.1887 * (1/(OC+1)) + 0.004527 * Cl + 0.001535 * Si + 0.001442 * Si * (1/(OC+1)) - 0.00005110 * Si * Cl + 0.0008676 * Cl * (1/(OC+1))
    
      # Compute water content at wilting point (cm3cm-3)
    thetaWP <-
      0.09878 + 0.002127* Cl - 0.0008366 * Si - 0.07670 *(1/(OC+1)) + 0.00003853 * Si * Cl + 0.002330 * Cl * (1/(OC+1)) + 0.0009498 * Si * (1/(OC+1))

      # Compute ASWC = thetaFC - thetaWP (in volumetric proportion = cm3cm-3)
    out <- data.frame(swcFCProp = thetaFC, swcWPProp = thetaWP, swhcProp = thetaFC - thetaWP)
    names(out) <- paste0(names(out), "_", depth)

    out
    
  }, simplify = F, USE.NAMES = T)

  bind_cols(plotcode = data_soil$plotcode, swhc_prop)
}
