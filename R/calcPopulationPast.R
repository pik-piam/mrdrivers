#' calcPopulationPast
#'
#' Calculates a time series of Population. Different sources are available:
#' \itemize{
#'   \item \code{WDI}: Source: Worldbank. Taiwan was estimated as the
#'         difference between all countries and the global total.
#'   \item \code{UN_PopDiv}: UN Population Division data. Taiwan is estimated
#'         from "other, non-specified areas". Missing countries have their
#'         values set to zero.
#' }
#'
#' @inheritParams calcPopulation
#' @inherit calcPopulation return
#'
#' @seealso [madrat::calcOutput()]
#' @family Population functions
#'
#' @examples \dontrun{
#' library(mrdrivers)
#' calcOutput("PopulationPast")
#' }
#'
calcPopulationPast <- function(PopulationPast = "WDI-UN_PopDiv-MI") {
  # Call internalCalcPopulationPast function the appropriate number of times
  toolInternalCalc("PopulationPast",
                   list("PopulationPast" = strsplit(PopulationPast, "-")[[1]]),
                   mbindOrFillWith = "fillWith")
}


######################################################################################
# Internal Function
######################################################################################
internalCalcPopulationPast <- function(PopulationPast) {
  data <- switch(
    PopulationPast,
    "WDI"      = readSource("WDI", "SP.POP.TOTL"),
    "UN_PopDiv" = readSource("UN_PopDiv", "WPP2019_estimates") * 1e-3,
    "MI"       = readSource("MissingIslands", "pop"),
    "Eurostat" = cPopulationPastEurostat(),
    stop("Bad input for PopulationPast. Invalid 'PopulationPast' argument.")
  )

  getNames(data) <- "population"
  data <- toolFinishingTouches(data)

  list(x = data,
       weight = NULL,
       unit = "million",
       description = paste0("Population data from ", PopulationPast))
}


######################################################################################
# Functions
######################################################################################
cPopulationPastEurostat <- function() {
  # Scale to milions
  dataEurostat <- readSource("EurostatPopGDP", "population") * 1e-6
  dataWDI <- readSource("WDI", "SP.POP.TOTL")

  # Get EUR countries.
  euCountries <- toolGetEUcountries()

  # Fill in missing ( == 0) eurostat data using wdi growth rates
  for (c in euCountries) {
    if (any(dataEurostat[c, , ] == 0) && !all(dataEurostat[c, , ] == 0)) {
       dataEurostat[c, , ] <- toolHarmonizeFutureGrPast(
         past = dataWDI[c, , ],
         future = dataEurostat[c, dataEurostat[c, , ] != 0, ]
        )
    }
  }

  dataEurostat[!getItems(dataEurostat, 1) %in% euCountries, , ] <- 0
  dataEurostat
}
