#' @describeIn calcPopulation Get historic population data
#' @examples \dontrun{
#' library(mrdrivers)
#' calcOutput("PopulationPast")
#' }
#'
calcPopulationPast <- function(PopulationPast = "WDI-UN_PopDiv-MI") { # nolint
  # Check user input
  toolCheckUserInput("PopulationPast", as.list(environment()))
  # Call calcInternalPopulationPast function the appropriate number of times (map) and combine (reduce)
  # !! Keep formula syntax for madrat caching to work
  purrr::pmap(list("PopulationPast" = unlist(strsplit(PopulationPast, "-"))),
              ~calcOutput("InternalPopulationPast", aggregate = FALSE, supplementary = TRUE, ...)) %>%
    toolReduce(mbindOrFillWith = "fillWith")
}


######################################################################################
# Internal Function
######################################################################################
calcInternalPopulationPast <- function(PopulationPast) { # nolint
  data <- switch(
    PopulationPast,
    "WDI"       = readSource("WDI", "SP.POP.TOTL"),
    "UN_PopDiv" = readSource("UN_PopDiv", "WPP2019_estimates") * 1e-3,
    "MI"        = readSource("MissingIslands", "pop"),
    "Eurostat"  = calcOutput("InternalPopulationPastEurostat", aggregate = FALSE),
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
calcInternalPopulationPastEurostat <- function() { # nolint
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
  list(x = dataEurostat, weight = NULL, unit = "million", description = "Population data from Eurostat")
}
