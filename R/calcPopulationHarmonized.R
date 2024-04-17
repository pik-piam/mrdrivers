#' Get Harmonized Population Data
#'
#' @inheritParams calcGDPpcHarmonized
#' @inherit madrat::calcOutput return
#' @keywords internal
calcPopulationHarmonized <- function(harmonization, past, future, yEnd, ...) {
  # Combine "past" and "future" time series.
  harmonizedData <- switch(harmonization,
    "withPEAPandFuture" = toolHarmonizeWithPEAPandFuture(past, future),
    "calibSSP2EU"       = toolHarmonizeSSP2EU(past, future),
    "calibISIMIP"       = toolHarmonizeISIMIP(past, future, yEnd = if(rlang::is_missing(yEnd)) 2030 else yEnd),
    stop(glue("Bad input for calcPopulationHarmonized. Argument harmonization = '{harmonization}' is invalid."))
  )
  list(x = harmonizedData$x, weight = NULL, unit = "million", description = harmonizedData$description)
}


toolHarmonizeWithPEAPandFuture <- function(past, future) {
  # Get PEAP data and fill in missing islands. Then drop everything that is not
  # "short-term", defined as being later than the last year of the IMF WEO data.
  shortTerm <- readSource("PEAP")
  fill <- readSource("MissingIslands", subtype = "pop", convert = FALSE)
  shortTerm <- shortTerm %>% toolFillWith(fill) %>% toolInterpolateAndExtrapolate()
  lastYearIMF <- max(getYears(readSource("IMF", "GDPpc"), as.integer = TRUE))
  shortTerm <- shortTerm[, getYears(shortTerm, as.integer = TRUE) <= lastYearIMF, ]

  # Use PEAP growth rates until last year of IMF WEO data, and future growth rates after that
  x <- past$x %>% toolHarmonizePast(shortTerm, method = "growth") %>% toolHarmonizePast(future$x, method = "growth")

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
  x <- toolHarmonizePast(past$x[euCountries, , ], dataEurostat[euCountries, , ], method = "growth")
  harmonizedData$x[euCountries, , ] <- x[euCountries, getYears(harmonizedData$x), ]

  list(x = harmonizedData$x,
       description = glue("equal to SSP2 in all countries except for EU countries. \\
                          For EU countries use {past$description} until 2021, \\
                          and growth rates from projections ({future$description}) thereafter."))
}

toolHarmonizeISIMIP <- function(past, future, yEnd) {
  x <- toolHarmonizePast(past$x, future$x, method = "transition", yEnd = yEnd)

  list(x = x,
       description = glue("use {past$description} until {max(getYears(past$x, as.integer = TRUE))}, \\
                          and converge towards {future$description} by {yEnd}."))
}
