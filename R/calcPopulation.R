#' calcPopulation
#'
#' Create population time series by harmonizing future projections onto historical data.
#'
#' @param PopulationCalib A string designating the harmonization function.
#'   Available harmonization functions are:
#'   \itemize{
#'     \item "calibSSPs":
#'     \item "calibSSP2EU":
#'     \item "calibSDPs":
#'     \item "past": deprecated
#'     \item "future": deprecated
#'     \item "transition": deprecated
#'     \item "past_transition":
#'     \item "past_grFuture":
#'   }
#' @param PopulationPast A string designating the source for the historical population data.
#'   Available sources are:
#'   \itemize{
#'     \item "WDI": World development indicators from the World Bank
#'     \item "UN_PopDiv": United Nations
#'     \item "MI": Missing island dataset
#'     \item "Eurostat": Eurostat
#'   }
#' @param PopulationFuture A string designating the source for the future population data.
#'   Available sources are:
#'   \itemize{
#'     \item "SSPs":
#'     \item "SSP2EU":
#'     \item "SDPs":
#'     \item "UN_PopDiv":
#'     \item "MI":
#'     \item "SSPs_old":
#'     \item "SRES": deprecated
#'     \item "IIASApop": deprecated
#'   }
#' @inheritParams calcGDP
#' @inherit calcGDP details return
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
calcPopulation <- function(PopulationCalib  = c("calibSSPs",                  # nolint
                                                "calibSDPs",
                                                "calibSSP2EU"),
                           PopulationPast   = c("WDI-UN_PopDiv-MI",           # nolint
                                                "WDI-UN_PopDiv-MI",
                                                "Eurostat-WDI-UN_PopDiv-MI"),
                           PopulationFuture = c("SSPs-UN_PopDiv-MI",          # nolint
                                                "SDPs-UN_PopDiv-MI",
                                                "SSP2EU-UN_PopDiv-MI"),
                           extension2150 = "bezier",
                           FiveYearSteps = TRUE,                              # nolint
                           naming = "indicator_scenario") {
  # Check user input
  toolCheckUserInput("Population", as.list(environment()))
  # Call calcInternalPopulation function the appropriate number of times (map) and combine (reduce)
  # !! Keep formula syntax for madrat caching to work
  purrr::pmap(as.list(environment()),
              ~calcOutput("InternalPopulation", aggregate = FALSE, supplementary = TRUE, ...)) %>%
    toolReduce()
}

######################################################################################
# Internal Function
######################################################################################
calcInternalPopulation <- function(PopulationCalib,  # nolint
                                   PopulationPast,   # nolint
                                   PopulationFuture, # nolint
                                   extension2150,
                                   FiveYearSteps,    # nolint
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
    "calibSSPs"       = toolHarmonizeSSPsSDPs(past, future),
    "calibSDPs"       = toolHarmonizeSSPsSDPs(past, future),
    "calibSSP2EU"     = toolHarmonizeSSP2EU(past, future),
    # Deprecated?
    "past"            = toolPopHarmonizePast(past, future),
    "future"          = toolHarmonizeFuture(past, future),
    "transition"      = toolHarmonizeTransition(past, future, yEnd = 2020),
    "past_transition" = toolHarmonizePastTransition(past, future, yEnd = 2030),
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
                             period until 2030"),
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
toolHarmonizeSSPsSDPs <- function(past, future) {
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

toolHarmonizeSSP2EU <- function(past, future) {
  combined <- toolHarmonizeSSPsSDPs(past, future)

  # For SSP2EU: simply glue past (until 2019) with future (starting 2020)
  # Get EUR countries.
  euCountries <- toolGetEUcountries()

  futYears <- getYears(future)[getYears(future, as.integer = TRUE) >= 2020]
  combined[euCountries, futYears, ] <- future[euCountries, futYears, ]

  combined
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
