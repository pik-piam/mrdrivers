#' calcPopulation, calcPopulationPast, calcPopulationFuture
#'
#' @description
#' Get complete population scenarios with calcPopulation, or the past/future scenario building blocks with
#' calcPopulationPast and calcPopulationFuture.
#'
#' Complete scenarios are created by harmonizing future projections (returned by calcPopulationFuture) onto historical
#' data (returned by calcPopulationPast) and cover the years between 1960 and 2100.
#'
#' If population data for a scenario is required, even if just for a single year, always use calcPopulation, as what is
#' returned by calcPopulationPast or calcPopulationFuture may not end up as is in the scenario, depending on the
#' harmonization function used (see the Populationcalib argument for more information). Use calcPopulationPast and
#' calcPopulationFuture only when trying to access specific population data, or when constructing new
#' complete scenarios.
#'
#' By default, calcPopulation returns the following scenarios:
#'  \itemize{
#'    \item the SSPs, i.e. SSP1-5 and SSP2EU
#'    \item the SDPs, i.e. SDP, SDP_EI, SDP_RC, and SDP_MC
#'  }
#'
#' @param PopulationCalib A string designating the harmonization function.
#'   Available harmonization functions are:
#'   \itemize{
#'     \item "calibSSPs": use past data from PopulationPast, then the growth rates from the Wolrld Bank's PEAP until
#'                        the final year of the IMF WEO data, and then the growth rates from PopulationFuture.
#'     \item "calibSSP2EU":
#'     \item "calibSDPs": same as calibSSPs
#'     \item "past": deprecated
#'     \item "future": deprecated
#'     \item "transition": deprecated
#'     \item "past_transition":
#'     \item "past_grFuture":
#'   }
#'
#' @param PopulationPast A string designating the source for the historical population data.
#'   Available sources are:
#'   \itemize{
#'     \item "WDI": World development indicators from the World Bank
#'     \item "UN_PopDiv": United Nations
#'     \item "MI": Missing island dataset
#'     \item "Eurostat": Eurostat
#'   }
#'   See the "Combining data sources with '-'" section below for how to combine data sources.
#'
#' @param PopulationFuture A string designating the source for the future population data.
#'   Available sources are:
#'   \itemize{
#'     \item "SSPs": From the Wittgenstein Center [here](http://pure.iiasa.ac.at/id/eprint/17550/) and
#'                   [here](http://pure.iiasa.ac.at/id/eprint/16710/)
#'     \item "SSP2EU": Combined SSP2 and Eurostat (for the EU countries) source
#'     \item "SDPs":
#'     \item "UN_PopDiv": United Nations
#'     \item "MI": Missing island dataset
#'     \item "SSPs_old": Old SSPs from the IIASA database
#'     \item "SRES": deprecated
#'     \item "IIASApop": deprecated
#'   }
#'   See the "Combining data sources with '-'" section below for how to combine data sources.
#'
#' @inheritParams calcGDP
#' @inherit calcGDP return
#' @inheritSection calcGDP Combining data sources with "-"
#' @inheritSection calcGDP Vectorization of arguments
#' @inheritSection calcGDP Return supplementary information
#'
#' @seealso [madrat::calcOutput()]
#' @family mrdrivers functions
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
