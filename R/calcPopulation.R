#' calcPopulation
#'
#' Merges time series of population for the past and present. See
#' \code{\link{calcPopulationPast}} for past datasets, and
#' \code{\link{calcPopulationFuture}} for future datasets. The time series are
#' merged via the growth rates. The first year of the future scenarios
#' determines the merging point. All data is calibrated either to the "past" or
#' the "future" dataset as specified by PopulationCalib.
#'
#' @param PopulationCalib to what should be calibrated? past, future or a transition?
#' @param PopulationPast population past data source
#' @param PopulationFuture population future data source
#'
#' @inheritParams calcGDP
#' @inherit calcGDP return
#'
#' @seealso [madrat::calcOutput()]
#' @family Population functions
#' @family Combined scenario functions
#'
#' @examples \dontrun{
#' library(mrdrivers)
#' calcOutput("GDPpc")
#' }
#'
calcPopulation <- function(PopulationCalib  = c("calibSSPs",
                                                "calibSDPs",
                                                "calibSSP2EU"),
                           PopulationPast   = c("WDI-UN_PopDiv-MI",
                                                "WDI-UN_PopDiv-MI",
                                                "Eurostat-WDI-UN_PopDiv-MI"),
                           PopulationFuture = c("SSPs-UN_PopDiv-MI",
                                                "SDPs-UN_PopDiv-MI",
                                                "SSP2EU-UN_PopDiv-MI"),
                           extension2150 = "bezier",
                           FiveYearSteps = TRUE,
                           naming = "indicator_scenario") {
  # Check user input
  toolCheckUserInput("Population", as.list(environment()))
  # Call internalCalcPopulation function the appropriate number of times
  toolInternalCalc("Population", as.list(environment()))
}

######################################################################################
# Internal Function
######################################################################################
internalCalcPopulation <- function(PopulationCalib,
                                   PopulationPast,
                                   PopulationFuture,
                                   extension2150,
                                   FiveYearSteps,
                                   naming) {

  # Compute "past" and "future" time series.
  past <- calcOutput("PopulationPast",
                     PopulationPast = PopulationPast,
                     aggregate = FALSE)
  future <- calcOutput("PopulationFuture",
                       PopulationFuture = PopulationFuture,
                       extension2150 = "none",
                       aggregate = FALSE)

  # Combine "past" and "future" time series.
  combined <- switch(
    PopulationCalib,
    "calibSSPs"       = harmonizeSSPsSDPs(past, future),
    "calibSDPs"       = harmonizeSSPsSDPs(past, future),
    "calibSSP2EU"     = harmonizeSSP2EU(past, future),
    "calibUN_PopDiv"  = harmonizeUN_PopDiv(past, future),
    # Deprecated?
    "past"            = popHarmonizePast(past, future),
    "future"          = toolHarmonizeFuture(past, future),
    "transition"      = toolHarmonizeTransition(past, future, yEnd = 2020),
    "past_transition" = toolHarmonizePastTransition(past, future, yEnd = 2050),
    "past_grFuture"   = toolHarmonizePastGrFuture(past, future),
    stop("Bad input for calcPopulation. Invalid 'PopulationCalib' argument.")
  )

  # Get description of harmonization function.
  datasettype <- switch(
    PopulationCalib,
    "calibSSPs"       = glue("use past data from {PopulationPast}, then the growth rates \\
                             from the Wolrld Bank's PEAP until \\
                             {max(getYears(readSource('IMF'), as.integer = TRUE))}, \\
                             and then the growth rates from {PopulationFuture}."),
    "calibSDPs"       = glue("use past data from {PopulationPast}, then the growth rates \\
                             from the Wolrld Bank's PEAP until \\
                             {max(getYears(readSource('IMF'), as.integer = TRUE))}, \\
                             and then the growth rates from {PopulationFuture}."),
    "calibSSP2EU"     = glue("use past data from {PopulationPast}, then the growth rates \\
                             from the Wolrld Bank's PEAP until \\
                             {max(getYears(readSource('IMF'), as.integer = TRUE))}, \\
                             and then the growth rates from {PopulationFuture}. \\
                             For European countries, just glue past with future."),
    "calibUN_PopDiv"  = glue("use past data from {PopulationPast} and then future data from \\
                              {PopulationFuture}."),
    "past"            = PopulationPast,
    "future"          = PopulationFuture,
    "transition"      = glue("transition between {PopulationPast} and {PopulationFuture} \\
                             with a transition period until 2020"),
    "past_transition" = glue("use past data and afterwards transition between \\
                             {PopulationPast} and {PopulationFuture} with a transition \\
                             period until 2050"),
    "past_grFuture"   = glue("use past data from {PopulationPast} and then the growth rates \\
                             from {PopulationFuture}."),
  )

  # Apply finishing touches to combined time-series
  combined <- toolFinishingTouches(combined, extension2150, FiveYearSteps, naming)

  list(x = combined,
       weight = NULL,
       unit = "million",
       description = glue("Population data. Datasource for the Past: {PopulationPast}. \\
                          Datasource for the Future: {PopulationFuture}. Calibrated \\
                          to {datasettype}"))

}

######################################################################################
# Population Harmonization Functions
######################################################################################
harmonizeSSPsSDPs <- function(past, future) {
  # Get PEAP data and fill in missing islands. Then drop everything that is not
  # "short-term", defined as being later than the last year of the IMF WEO data.
  shortTerm <- readSource("PEAP")
  fill <- readSource("MissingIslands", subtype = "pop", convert = FALSE)
  shortTerm <- shortTerm %>%
    toolFillWith(fill) %>%
    toolInterpolateAndExtrapolate()
  lastYearIMF <- max(getYears(readSource("IMF"), as.integer = TRUE))
  shortTerm <- shortTerm[, getYears(shortTerm, as.integer = TRUE) <= lastYearIMF, ]

  # Use PEAP growth rates until last year of IMF WEO data, and future growth rates after that
  past %>%
    toolHarmonizePastGrFuture(shortTerm) %>%
    toolHarmonizePastGrFuture(future)
}

harmonizeSSP2EU <- function(past, future) {
  combined <- harmonizeSSPsSDPs(past, future)

  # For SSP2EU: simply glue past (until 2019) with future (starting 2020)
  # Get EUR countries.
  euCountries <- toolGetEUcountries()

  futYears <- getYears(future)[getYears(future, as.integer = TRUE) >= 2020]
  combined[euCountries, futYears, ] <- future[euCountries, futYears, ]

  combined
}

harmonizeUN_PopDiv <- function(past, future) {
  # Glue future to past
  # year <- max(intersect(getYears(past, as.integer = TRUE),
  #                       getYears(future, as.integer = TRUE)))
  year <- 2017
  futYears <- getYears(future)[getYears(future, as.integer = TRUE) > year]
  getNames(past) <- getNames(future)
  mbind(past, future[, futYears, ])
}

# Legacy?
popHarmonizePast <- function(past, future) {
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
