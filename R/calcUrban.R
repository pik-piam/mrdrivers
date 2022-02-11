#' calcUrban
#'
#' @description
#' Get urbanization scenarios. By default the following scenarios are returned: 
#' \itemize{
#'   \item the SSPs, i.e. SSP1-5 and SSP2EU
#'   \item the SDPs, i.e. SDP, SDP_EI, SDP_RC, and SDP_MC
#' }
#'
#' The scenarios are created by harmonizing future projections onto historical data.
#'
#' @param UrbanCalib To what should be calibrated? past or future?
#' @param UrbanPast Urban past data source
#' @param UrbanFuture Urban future data source
#'
#' @inheritParams calcGDP
#' @inherit calcGDP return
#' @inheritSection calcGDP Return supplementary information
#' @inheritSection calcGDP Vectorization of arguments
#'
#' @seealso [madrat::calcOutput()]
#' @family Urban functions
#' @family Combined scenario functions
#'
#' @examples \dontrun{
#' library(mrdrivers)
#' calcOutput("Urban")
#' }
#'
calcUrban <- function(UrbanCalib = "past",                       # nolint
                      UrbanPast = "WDI",                         # nolint
                      UrbanFuture = c("SSPs", "SDPs", "SSP2EU"), # nolint
                      extension2150 = "constant",
                      FiveYearSteps = TRUE,                      # nolint
                      naming = "indicator_scenario") {
  # Check user input
  toolCheckUserInput("Urban", as.list(environment()))
  # Call calcInternalUrban function the appropriate number of times (map) and combine (reduce)
  # !! Keep formula syntax for madrat caching to work
  purrr::pmap(as.list(environment()), ~calcOutput("InternalUrban", aggregate = FALSE, supplementary = TRUE, ...)) %>%
    toolReduce()
}

######################################################################################
# Internal Function
######################################################################################
calcInternalUrban <- function(UrbanCalib,    # nolint
                              UrbanPast,     # nolint
                              UrbanFuture,   # nolint
                              FiveYearSteps, # nolint
                              extension2150,
                              naming) {

  # Compute "past" and "future" time series.
  past <- calcOutput("UrbanPast", UrbanPast = UrbanPast, aggregate = FALSE)
  future <- calcOutput("UrbanFuture", UrbanFuture = UrbanFuture, extension2150 = "none", aggregate = FALSE)

  # Combine "past" and "future" time series.
  combined <- switch(
    UrbanCalib,
    "past"   = toolHarmonizePast(past, future),
    "future" = toolHarmonizeFuture(past, future),
    stop("Bad input for calcUrban. Invalid 'UrbanCalib' argument.")
  )

  # Get description of harmonization function.
  datasettype <- switch(
    UrbanCalib,
    "past"   = UrbanPast,
    "future" = UrbanFuture,
  )

  # Apply finishing touches to combined time-series
  combined <- toolFinishingTouches(combined, extension2150, FiveYearSteps, naming)

  # Get weigth
  wp <- calcOutput("Population",
                   PopulationCalib = UrbanCalib,
                   PopulationPast = UrbanPast,
                   PopulationFuture = UrbanFuture,
                   FiveYearSteps = FiveYearSteps,
                   extension2150 = extension2150,
                   naming = naming,
                   aggregate = FALSE)
  # Give weight same names as data, so that aggregate doesn't mess up data dim
  getNames(wp) <- gsub("pop", "urb", getNames(wp))

  combined <- combined[getItems(wp, 1), getYears(wp), ]

  list(x = combined,
       weight = wp,
       unit = "share of population",
       description = glue("Urbanisation data. Datasource for the Past: {UrbanPast}. \\
                           Datasource for the Future: {UrbanFuture}. Calibrated \\
                           to {datasettype}"))
}
