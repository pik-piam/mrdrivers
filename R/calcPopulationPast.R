#' Get Population and Labour scenario building blocks
#'
#' See the "Combining data sources with '-'" section below for how to combine data sources.
#'
#' @param PopulationPast A string designating the source for the historical population data.
#'   Available sources are:
#'   \itemize{
#'     \item "WDI": World development indicators from the World Bank
#'     \item "UN_PopDiv": United Nations
#'     \item "MI": Missing island dataset
#'     \item "Eurostat": Eurostat
#'   }
#' @inheritSection calcScenarioConstructor Combining data sources with "-"
#' @keywords internal
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
    "UN_PopDiv" = readSource("UN_PopDiv") * 1e-3,
    "MI"        = readSource("MissingIslands", "pop"),
    "Eurostat"  = calcOutput("InternalPopulationPastEurostat", aggregate = FALSE),
    stop("Bad input for PopulationPast. Invalid 'PopulationPast' argument.")
  )

  getNames(data) <- "population"
  data <- toolFinishingTouches(data)

  list(x = data, weight = NULL, unit = "million", description = glue("{PopulationPast} data"))
}


######################################################################################
# Functions
######################################################################################
calcInternalPopulationPastEurostat <- function() { # nolint
  # Scale to milions
  dataEurostat <- readSource("EurostatPopGDP", "population") * 1e-6
  # Set all eurostat countries with data, but that are not apart of the EUR region or GBR to 0
  euCountries <- toolGetEUcountries()
  dataEurostat[!getItems(dataEurostat, 1) %in% euCountries, , ] <- 0
  # Fill in gaps in data (do not extrapolate)
  dataEurostat <- toolInterpolateAndExtrapolate(dataEurostat, extrapolate = FALSE)
  # Extrapolate missing data for eu countries using wdi growth rates
  dataWDI <- readSource("WDI", "SP.POP.TOTL")
  for (c in euCountries) {
    # Skip countries with no data, or with complete data
    if (all(dataEurostat[c, , ] == 0) || all(dataEurostat[c, , ] != 0)) next
    # Get WDI years with 0 in dataEurostat
    yearsWith0 <- getYears(dataWDI)[dataEurostat[c, getYears(dataWDI), ] == 0]
    if (purrr::is_empty(yearsWith0)) next
    yearsWithout0 <- getYears(dataWDI)[dataEurostat[c, getYears(dataWDI), ] != 0]
    # Extrapolate into the past, in the dataWDI years
    if (min(yearsWith0) < min(yearsWithout0)) {
      pastYears <- getYears(dataWDI)[getYears(dataWDI) <= max(yearsWithout0)]
      pastYearsWithout0 <- pastYears[pastYears %in% yearsWithout0]
      dataEurostat[c, pastYears, ] <- toolHarmonizeFutureGrPast(
        past = dataWDI[c, , ],
        future = dataEurostat[c, pastYearsWithout0, ]
      )
    }
    # Extrapolate into the future, in the dataWDI years
    if (max(yearsWith0) > max(yearsWithout0)) {
      futureYears <- getYears(dataWDI)[getYears(dataWDI) >= min(yearsWithout0)]
      futureYearsWithou0 <- futureYears[futureYears %in% yearsWithout0]
      dataEurostat[c, futureYears, ] <- toolHarmonizePastGrFuture(
        past = dataEurostat[c, futureYearsWithou0, ],
        future = dataWDI[c, , ]
      )
    }
  }
  # Extrapolate in years outside of WDI range
  dataEurostat <- toolInterpolateAndExtrapolate(dataEurostat)
  list(x = dataEurostat, weight = NULL, unit = "million", description = "Eurostat data")
}
