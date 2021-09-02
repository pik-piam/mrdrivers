
convertARIADNE_ReferenceScenario <- function(x, subtype){

  # Convert region codons from Eurostat to iso3c
  getItems(x, 1) <- countrycode::countrycode(getRegions(x), "eurostat", "iso3c")

  if (subtype %in% c("gdp", "gdp_corona")) {
    # Convert currency: 
    # First we use the country-sepcific defltors to change the base year, and then we 
    # divide by 0.8041, a fixed value sourced from the World Bank's WDI 
    # (1 US$2005 = 0.8041 €2005) to get the values in 2005 US$MER.
    # In a second step we convert from US$MER to Int$PPP
    # The reason we can't do the conversin at once here is that x is not in LCU but in €. 
    x <- GDPuc::convertGDP(x, "constant 2010 LCU", "constant 2005 LCU") / 0.8041
    x <- GDPuc::convertGDP(x, "constant 2005 US$MER", "constant 2005 Int$PPP")

    # Rename
    getNames(x) <- "GDP|PPP (million US$2005/yr)"
  }

  
  x <- toolCountryFill(x)
  x[is.na(x)] <- 0
  return(x)
}

