# Apply finishig touches to the combined object, as found in calcGDP, calcGDPpc and calcPopulation
finishingTouches <- function(x, extension2150 = "none", FiveYearSteps = FALSE, naming = "indicator_scenario") {

  # Extend to 2150, if opted for
  x <- extend2150(x, extension2150)

  # Return only 5-year time steps, if opted for
  if (FiveYearSteps){
    x <- x[, getYears(x, as.integer = TRUE) %% 5 == 0, ]
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
