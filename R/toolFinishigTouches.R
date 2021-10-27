# Apply finishig touches to the combined object, as found in calcGDP, calcGDPpc and calcPopulation
toolFinishingTouches <- function(x, 
                                 extension2150 = "none", 
                                 FiveYearSteps = FALSE, 
                                 naming = "indicator_scenario",
                                 unit = "none", 
                                 construct_unit = "none") {

  x <- toolInterpolateAndExtrapolate(x)

  # Extend to 2150, if opted for
  x <- toolExtend2150(x, extension2150)

  # LONGTERM: Historically this was done with magpiesets::findest("time"), which returned
  # seq(1965, 2150, 5).. so that is what is being done here. But this is confusing.
  # Is it even necessary? Why not use the 'years' argument of calcOutput...
  # Return only 5-year time steps, if opted for
  if (FiveYearSteps) {
    x <- x[, getYears(x, as.integer = TRUE) %% 5 == 0, ]
    # This operation used to be done using magpiesets::findset("time"), which for some reason
    # doesn't include 1960. So it's taken out as well.
    x <- x[, getYears(x, as.integer = TRUE) != 1960, ]
  }

  if (construct_unit != unit) {
     x <- GDPuc::convertGDP(x, construct_unit, unit, replace_NAs = 1)
  }

  # Order by names
  x <- x[, , order(getNames(x))]

  # Split scenario dimension
  if (naming == "indicator.scenario") {
    getNames(x) <- sub("_",  ".", getNames(x))
    getSets(x) <- c(getSets(x)[1], getSets(x)[2], "indicator", "scenario")
  }

  x
}