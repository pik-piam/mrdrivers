#' Get Harmonized Labour Data
#'
#' @param args Arguments passed on to harmonization functions
#' @inherit madrat::calcOutput return
#' @keywords internal
calcLabourHarmonized <- function(args) {
  # Combine "past" and "future" time series.
  harmonizedData <- switch(
    args$harmonization,
    args$future$x
  )
  # Get description of harmonization function.
  description <- switch(
    args$harmonization,
    "No description available.."
  )
  list(x = harmonizedData, weight = NULL, unit = "million", description = description)
}
