#' Converts Eurostat historical emissions 
#' 
#' @param x MAgPIE object to be converted
#' @param subtype emissions for original eurostat emissions split, MACCemi for MACC historical emissions, or
#' sectorEmi for sector specific emissions
#' @return A MAgPIE object containing the Eurostat historical emissions (MtCO2) 
#' @family Eurostat functions
#' @examples \dontrun{
#' convertEurostat(x, subtype = "population")
#' }
convertEurostatPopGDP <- function(x, subtype) {

  switch(
    subtype,
    "population"             = convEurostatPopulation(x),
    "population_projections" = convEurostatPopulation(x),
    "GDP"                    = convEurostatGDP(x),
    stop("Bad input for convertEurostat. Invalid 'subtype' argument.")
  )
}

######################################################################################
# Functions
######################################################################################
convEurostatPopulation <- function(x) {
  # Fix names of sets, and of variable
  x <- collapseDim(x, dim = 3)
  getNames(x) <- "population"
  # Use the "DE_TOT" values for Germany, if they exist (DE_TOT = East + West Germany)
  x["DE",,] <- if ("DE_TOT" %in% getRegions(x)) x["DE_TOT",,] else x["DE",,]
  # Drop any countries with more than 2 charachters in their Eurostat identifier. Those are aggregates.
  my_countries <- getRegions(x)[purrr::map_lgl(getRegions(x), ~ nchar(.x) == 2)]
  x <- x[my_countries,,]
  # Convert the eurostat countrycodes to iso3c codes
  getItems(x, 1) <- countrycode::countrycode(getRegions(x), "eurostat", "iso3c", warn = FALSE)
  # ABOVE Warning: Some values were not matched unambiguously: FX, XK
  # Fix set names
  
  toolGeneralConvert(x, note = FALSE)
 }

convEurostatGDP <- function(x) {
  # Convert the eurostat countrycodes to iso3c codes
  getItems(x, 1) <- countrycode::countrycode(getRegions(x), "eurostat", "iso3c", warn = FALSE)
  # ABOVE warning that is being ignored: 
  # Some values were not matched unambiguously: EA, EA12, EA19, EU15, EU27_2020, EU28

  x <- toolGeneralConvert(x, note = FALSE)
  
  # Convert from constant 2005 LCU to constant 2005 Int$PPP.
  getNames(x) <- "GDP"
  x <- GDPuc::convertGDP(x, "constant 2005 LCU", "constant 2005 Int$PPP", replace_NAs = 1) 
}
