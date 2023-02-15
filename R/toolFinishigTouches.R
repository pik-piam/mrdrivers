toolFinishingTouches <- function(x, extension2150 = "none", naming = "indicator_scenario") {
  x <- toolInterpolateAndExtrapolate(x)

  # Extend to 2150, if opted for
  x <- toolExtend2150(x, extension2150)

  # Order by names
  x <- x[, , order(getNames(x))]

  # Split indicator from scenario
  if (naming == "indicator.scenario") {
    getNames(x) <- sub("_",  ".", getNames(x))
    getSets(x) <- c(getSets(x)[1], getSets(x)[2], "indicator", "scenario")
  }

  # Drop indicator
  if (naming == "scenario") {
    getNames(x) <- sub(".*?_",  "", getNames(x))
  }

  x
}
