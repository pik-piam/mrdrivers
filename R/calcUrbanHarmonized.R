#' Get Harmonized Urban Data
#'
#' @param args Arguments passed on to harmonization functions
#' @inherit madrat::calcOutput return
#' @keywords internal
calcUrbanHarmonized <- function(args) {
  # Combine "past" and "future" time series.
  harmonizedData <- switch(
    args$harmonization,
    "past"   = toolHarmonizePast(args$past$x, args$future$x),
    stop(glue("Bad input for calcUrbanHarmonized. Argument harmonization = '{args$harmonization}' is invalid."))
  )

  # Cap urban share at 99%.
  harmonizedData[harmonizedData > 0.99] <- 0.99

  # Get description of harmonization function.
  description <- switch(
    args$harmonization,
    "past"   = args$pastData,
  )

  list(x = harmonizedData,
       weight = NULL,
       unit = "share of population",
       description = glue("Urban population data. Datasource for the Past: {args$pastData}.\\
                           Datasource for the Future: {args$futureData}. Calibrated to {description}"))
}
