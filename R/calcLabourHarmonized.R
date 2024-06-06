#' Get Harmonized Labour Data
#'
#' @inheritParams calcGDPpcHarmonized
#' @inherit madrat::calcOutput return
#' @keywords internal
calcLabourHarmonized <- function(harmonization, past, future, ...) {
  # Combine "past" and "future" time series.
  harmonizedData <- switch(
    harmonization,
    "pastAndLevel" = toolHarmonizePast(past$x, future$x, method = "level"),
    stop(glue("Bad input for calcLabourHarmonized Argument harmonization = '{harmonization}' is invalid."))
  )
  # Get description of harmonization function.
  description <- switch(
    harmonization,
    "pastAndLevel" = glue("use {past$description} until {max(getYears(past$x, as.integer = TRUE))} \\
                           and then switch directly to {future$description}."),
  )
  list(x = harmonizedData, weight = NULL, unit = "million", description = description)
}
