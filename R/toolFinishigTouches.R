# Apply finishig touches to the combined object, as found in calcGDP, calcGDPpc and calcPopulation
toolFinishingTouches <- function(x,
                                 extension2150 = "none",
                                 FiveYearSteps = FALSE,
                                 naming = "indicator_scenario",
                                 unit = "none",
                                 constructUnit = "none") {

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

  if (constructUnit != unit) {
    # Convert using regional averages for now.
    regmap <- toolGetMapping("regionmappingH12.csv") %>%
      tibble::as_tibble() %>%
      dplyr::select("iso3c" = .data$CountryCode, "region" = .data$RegionCode)

    x <- GDPuc::convertGDP(x, constructUnit, unit, with_regions = regmap, replace_NAs = "regional_average")
  }

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
