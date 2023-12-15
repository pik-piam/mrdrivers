#' Get Harmonized Population Data
#'
#' @param args Arguments passed on to harmonization functions
#' @inherit madrat::calcOutput return
#' @keywords internal
calcPopulationHarmonized <- function(args) {
  # Combine "past" and "future" time series.
  harmonizedData <- switch(args$harmonization,
    "withPEAPandFuture" = toolHarmonizeWithPEAPandFuture(args$past, args$future),
    "calibSSP2EU"       = toolHarmonizeSSP2EU(args$past, args$future),
    "calibISIMIP"       = toolHarmonizeISIMIP(args$past, args$future, yEnd = 2030),
    "past_transition"   = toolHarmonizePastTransition(args$past$x, args$future$x, yEnd = 2050, aslist = TRUE),
    stop(glue("Bad input for calcPopulationHarmonized. Argument harmonization = '{args$harmonization}' is invalid."))
  )
  list(x = harmonizedData$x, weight = NULL, unit = "million", description = harmonizedData$description)
}


toolHarmonizeWithPEAPandFuture <- function(past, future) {
  # Get PEAP data and fill in missing islands. Then drop everything that is not
  # "short-term", defined as being later than the last year of the IMF WEO data.
  shortTerm <- readSource("PEAP")
  fill <- readSource("MissingIslands", subtype = "pop", convert = FALSE)
  shortTerm <- shortTerm %>% toolFillWith(fill) %>% toolInterpolateAndExtrapolate()
  lastYearIMF <- max(getYears(readSource("IMF"), as.integer = TRUE))
  shortTerm <- shortTerm[, getYears(shortTerm, as.integer = TRUE) <= lastYearIMF, ]

  # Use PEAP growth rates until last year of IMF WEO data, and future growth rates after that
  x <- past$x %>% toolHarmonizePastGrFuture(shortTerm) %>% toolHarmonizePastGrFuture(future$x)

  lastPastYear <- max(getYears(past$x, as.integer = TRUE))
  list(x = x,
       description = glue("use {past$description} until {lastPastYear}, \\
                          growth rates from the Wolrld Bank's PEAP until {lastYearIMF}, \\
                          and growth rates from {future$description} thereafter."))
}

toolHarmonizeSSP2EU <- function(past, future) {
  harmonizedData <- toolHarmonizeWithPEAPandFuture(past, future)

  # For EUR countries use only growth rates of EUROSTAT projections (load in fresh: future only has 5 year steps)
  euCountries <- toolGetEUcountries()
  dataEurostat <- readSource("EurostatPopGDP", "population_projections") * 1e-6
  x <- toolHarmonizePastGrFuture(past$x[euCountries, , ], dataEurostat[euCountries, , ])
  harmonizedData$x[euCountries, , ] <- x[euCountries, getYears(harmonizedData$x), ]

  list(x = harmonizedData$x,
       description = glue("equal to SSP2 in all countries except for EU countries. \\
                          For EU countries use {past$description} until 2021, \\
                          and growth rates from projections ({future$description}) thereafter."))
}

toolHarmonizeISIMIP <- function(past, future, yEnd) {
  # Extend past by the first year of future, to make sure there is 1 year overlap between past and future
  data20xx <- calcOutput("PopulationFuture", PopulationFuture = "UN_PopDiv-MI", aggregate = FALSE)[, 1, ]
  getNames(data20xx) <- getNames(past$x)
  past$x <- mbind(past$x, data20xx)

  # Then use toolHarmonizePastTransition
  x <- toolHarmonizePastTransition(past$x, future$x, yEnd)

  list(x = x,
       description = glue("use {past$description} until 2020, UN_PopDiv projections for 2021, \\
                          and converge towards {future$description} by {yEnd}."))
}
