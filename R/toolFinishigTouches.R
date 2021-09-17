# Apply finishig touches to the combined object, as found in calcGDP, calcGDPpc and calcPopulation
finishingTouches <- function(x, extension2150 = "none", FiveYearSteps = FALSE, naming = "indicator_scenario") {

  # Extend to 2150, if opted for
  x <- extend2150(x, extension2150)

  # Return only 5-year time steps, if opted for
  if (FiveYearSteps){
    x <- x[, getYears(x, as.integer = TRUE) %% 5 == 0, ]
    # This operation used to be done using magpiesets::findset("time"), which for some reason
    # doesn't include 1960. So it's taken out as well.
    x <- x[, getYears(x, as.integer = TRUE) != 1960, ]
  }

  # Clean magpie pbject (necessary?)
  x <- clean_magpie(x)
  x <- x[,, order(getNames(x))]
  #combined[combined[] == "Inf"] <- 0    # LB: preliminary bug fix

  # Set names of combined (necessary?)
  #combined <- setNames(combined, future_names)

  # Split scenario dimension
  if(naming == "indicator.scenario"){
    getNames(x) <- sub("_",  ".", getNames(x))
    getSets(x) <- c(getSets(x)[1], getSets(x)[2], "indicator", "scenario")
  } 

  x
}
