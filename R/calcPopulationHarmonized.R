#' Get Harmonized Population Data
#'
#' @param args Arguments passed on to harmonization functions
#' @inherit madrat::calcOutput return
#' @keywords internal
calcPopulationHarmonized <- function(args) {
  # Combine "past" and "future" time series.
  harmonizedData <- switch(args$harmonization,
    "calibSSPs"       = toolHarmonizeSSPsSDPs(args$past$x, args$future$x),
    "calibSDPs"       = toolHarmonizeSSPsSDPs(args$past$x, args$future$x),
    "calibNoCovid"    = toolHarmonizeSSPsSDPs(args$past$x, args$future$x),
    "calibLongCovid"  = toolHarmonizeSSPsSDPs(args$past$x, args$future$x),
    "calibShortCovid" = toolHarmonizeSSPsSDPs(args$past$x, args$future$x),
    "calibSSP2EU"     = toolHarmonizeSSP2EU(args$past$x, args$future$x),
    "calibISIMIP"     = toolHarmonizeISIMIP(args$past$x, args$future$x, yEnd = 2030),
    "past"            = toolPopHarmonizePast(args$past$x, args$future$x),
    "past_transition" = toolHarmonizePastTransition(args$past$x, args$future$x, yEnd = 2050),
    stop("Bad input for calcHarmonizedPopulationData Invalid 'harmonization' argument.")
  )

  # Get description of harmonization function.
  description <- switch(args$harmonization,
    "calibSSPs"       = glue("use past data from {args$pastData}, then the growth rates from the Wolrld Bank's PEAP \\
                             until {max(getYears(readSource('IMF'), as.integer = TRUE))}, and then the growth rates \\
                             from {args$futureData}."),
    "calibSDPs"       = glue("use past data from {args$pastData}, then the growth rates from the Wolrld Bank's PEAP \\
                             until {max(getYears(readSource('IMF'), as.integer = TRUE))}, and then the growth rates \\
                             from {args$futureData}."),
    "calibSSP2EU"     = glue("use past data from {args$pastData}, then the growth rates from the Wolrld Bank's PEAP \\
                             until {max(getYears(readSource('IMF'), as.integer = TRUE))}, and then the growth rates \\
                             from {args$futureData}. For European countries, just glue past with future."),
    "calibISIMIP"     = glue("use past data from {args$pastData} - should be UN_PopDiv with data, currently, until \\
                             2020. Add the 2021 projections from UN_PopDiv. Then converge towards {args$futureData} \\
                             by 2030."),
    "calibUN_PopDiv"  = glue("use past data from {args$pastData} and then future data from {args$futureData}."),
    "past"            = args$pastData,
    "past_transition" = glue("use past data and afterwards transition between {args$pastData} and \\
                             {args$futureData} with a transition period until 2050"),
    "calibNoCovid"    = glue("use past data from {args$pastData}, then the growth rates from the Wolrld Bank's PEAP \\
                             until {max(getYears(readSource('IMF'), as.integer = TRUE))}, and then the growth rates \\
                             from {args$futureData}."),
    "No description available.."
  )

  list(x = harmonizedData, weight = NULL, unit = "million", description = description)
}


toolHarmonizeSSPsSDPs <- function(past, future) {
  # Get PEAP data and fill in missing islands. Then drop everything that is not
  # "short-term", defined as being later than the last year of the IMF WEO data.
  shortTerm <- readSource("PEAP")
  fill <- readSource("MissingIslands", subtype = "pop", convert = FALSE)
  shortTerm <- shortTerm %>% toolFillWith(fill) %>% toolInterpolateAndExtrapolate()
  lastYearIMF <- max(getYears(readSource("IMF"), as.integer = TRUE))
  shortTerm <- shortTerm[, getYears(shortTerm, as.integer = TRUE) <= lastYearIMF, ]

  # Use PEAP growth rates until last year of IMF WEO data, and future growth rates after that
  past %>% toolHarmonizePastGrFuture(shortTerm) %>% toolHarmonizePastGrFuture(future)
}

toolHarmonizeSSP2EU <- function(past, future) {
  combined <- toolHarmonizeSSPsSDPs(past, future)

  # For SSP2EU: simply glue past (until 2019) with future (starting 2020)
  # Get EUR countries.
  euCountries <- toolGetEUcountries()

  futYears <- getYears(future)[getYears(future, as.integer = TRUE) >= 2020]
  combined[euCountries, futYears, ] <- future[euCountries, futYears, ]

  combined
}

toolHarmonizeISIMIP <- function(past, future, yEnd) {
  # Extend past by the first year of future, to make sure there is 1 year overlap between past and future
  data20xx <- calcOutput("PopulationFuture", PopulationFuture = "UN_PopDiv-MI", aggregate = FALSE)[, 1, ]
  getNames(data20xx) <- getNames(past)
  past <- mbind(past, data20xx)

  # Then use toolHarmonizePastTransition
  toolHarmonizePastTransition(past, future, yEnd)
}


# Legacy?
toolPopHarmonizePast <- function(past, future) {
  firstyear <- min(getYears(future, as.integer = TRUE))
  tmp <- future / setYears(future[, firstyear, ], NULL)
  tmp[is.nan(tmp)] <- 1
  tmp <- dimSums(tmp * setYears(past[, firstyear, ], NULL), dim = 3.2)
  if (firstyear > min(getYears(past, as.integer = TRUE))) {
    yearsPast <- getYears(past)[which(getYears(past, as.integer = TRUE) < firstyear)]
    tmp2       <- setNames(past[, yearsPast, rep(1, ndata(future))], getNames(future))
    combined   <- mbind(tmp2, tmp)
  } else {
    combined <- tmp
  }
  combined[combined == Inf] <- 0
  combined
}
