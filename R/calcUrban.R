#' calcUrban
#'
#' Merges time series of urban shares for the past and present.  See
#' \code{\link{calcUrbanPast}} for past datasets, and
#' \code{\link{calcUrbanFuture}} for future datasets.  The time series are
#' merged via the growth rates. The first year of the future scenarios
#' determines the merging point.  All data is calibrated either to the "past"
#' or the "future" dataset.  Currently, the SSP (future) and WDI (past) data
#' have some inconsistencies, which leads to unrealistic figures if the one is
#' scaled on the other.
#'
#' @param UrbanCalib To what should be calibrated? past or future?
#' @param UrbanPast Urban past data source
#' @param UrbanFuture Urban future data source
#'
#' @inheritParams calcGDP
#' @inherit calcGDP return
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
calcUrban <- function(UrbanCalib = "past",
                      UrbanPast = "WDI",
                      UrbanFuture = c("SSPs", "SDPs", "SSP2EU"),
                      extension2150 = "constant",
                      FiveYearSteps = TRUE,
                      naming = "indicator_scenario") {
  # Check user input
  toolCheckUserInput("Urban", as.list(environment()))
  # Call internalCalcUrban function the appropriate number of times
  toolInternalCalc("Urban", as.list(environment()))
}

######################################################################################
# Internal Function
######################################################################################
internalCalcUrban <- function(UrbanCalib,
                              UrbanPast,
                              UrbanFuture,
                              FiveYearSteps,
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
