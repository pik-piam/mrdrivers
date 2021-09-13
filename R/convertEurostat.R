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
convertEurostat <- function(x, subtype) {

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
  getItems(x, 1) <- countrycode::countrycode(getRegions(x), "eurostat", "iso3c")
  # Fix set names
  getSets(x) <- c("iso3c", "year", "value")
  # Filter out any countries that don't have a iso3c code (in this case Kosovo, and Mainland-France)
  x <- x[!is.na(getCells(x)),,]
  # Sort by year
  x <- x[,sort(getYears(x)),]
  # Replace NAs with 0
  x[is.na(x)] <- 0
  # Fill in 0 for all missing countries
  x <- toolCountryFill(x, fill = 0)
 }

convEurostatGDP <- function(x) {
  # Fix names of sets, and of variable
  x <- collapseDim(x, dim = 3)
  getNames(x) <- "GDP"

  # Drop any countries with more than 2 charachters in their Eurostat identifier. Those are aggregates.
  my_countries <- getRegions(x)[purrr::map_lgl(getRegions(x), ~ nchar(.x) == 2)]
  x <- x[my_countries,,]

  # Convert the eurostat countrycodes to iso3c codes
  getItems(x, 1) <- countrycode::countrycode(getRegions(x), "eurostat", "iso3c")

  # Fix set names
  getSets(x) <- c("iso3c", "year", "value")

  # Filter out any countries that don't have a iso3c code
  x <- x[!is.na(getCells(x)),,]

  # Convert from constant 2005 LCU to constant 2005 Int$PPP
  x <- GDPuc::convertGDP(x, "constant 2005 LCU", "constant 2005 Int$PPP")

  # Sort by year
  x <- x[,sort(getYears(x)),]

  # Replace NAs with 0
  x[is.na(x)] <- 0

  # Fill in 0 for all missing countries
  x <- toolCountryFill(x, fill = 0)
}
