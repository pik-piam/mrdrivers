#' Get Harmonized Urban Data
#'
#' @inheritParams calcGDPpcHarmonized
#' @inherit madrat::calcOutput return
#' @keywords internal
calcUrbanHarmonized <- function(harmonization, past, future, ...) {
  # Combine "past" and "future" time series.
  harmonizedData <- switch(
    harmonization,
    "pastAndLevel"      = toolHarmonizePast(past$x, future$x, method = "level"),
    "pastAndGrowth"     = toolHarmonizePast(past$x, future$x, method = "growth"),
    "pastAndTransition" = toolHarmonizePast(past$x, future$x, method = "transition", yEnd = 2100),
    stop(glue("Bad input for calcUrbanHarmonized. Argument harmonization = '{harmonization}' is invalid."))
  )
  # Get description of harmonization function.
  description <- switch(
    harmonization,
    "pastAndLevel" = glue("use {past$description} until {max(getYears(past$x, as.integer = TRUE))} \\
                           and then switch directly to {future$description}."),
    ""
  )

  # Cap urban share at 99%.
  harmonizedData[harmonizedData > 0.99] <- 0.99

  list(x = harmonizedData, weight = NULL, unit = "share of population", description = description)
}
