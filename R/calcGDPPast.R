#' Get scenario building blocks
#'
#' @description
#' Get the past and future scenario building blocks.
#' If scenario data is required, even if just for a single year, always use the calc call to the final scenario, e.g.
#' [calcGDP()] or [calcPopulation()], as what is returned by the calc...Past and calc...Future functions may not end up
#' as is in the scenario, depending on the harmonization function.
#' These functions are only still exported for compatibility with existing code in the PIAM input data pipeline.
#'
#' See the vignette: \code{vignette("scenarios")} for references for the different data sources.
#'
#' See the "Combining data sources with '-'" section below for how to combine data sources.
#'
#' @param pastData A string specifying the sources for historic data.
#' @param extension1960 A string specifying how the data should be extended backwards to 1960.
#' @inheritSection calcScenarioConstructor Combining data sources with "-"
#' @keywords internal
#' @order 1
calcGDPPast <- function(pastData = "WDI-MI-James", extension1960 = "MI-James") {
  # Check user input
  toolCheckUserInput("GDPPast", as.list(environment()))

  # Call calcInternalGDPPast function the appropriate number of times (map) and combine (reduce)
  # !! Keep formula syntax for madrat caching to work
  data <- purrr::pmap(list("pastData" = unlist(strsplit(pastData, "-"))),
                      ~calcOutput("InternalGDPPast", aggregate = FALSE, supplementary = TRUE, ...)) %>%
    toolListFillWith()

  # If required, project GDP series back to 1960 using growth rates from the extension1960 data set.
  if (extension1960 != "none") {
    data1960 <-  calcOutput("GDPPast",
                            pastData = extension1960,
                            extension1960 = "none",
                            aggregate = FALSE,
                            supplementary = TRUE)
    data <- toolHarmonizeFuture(past = data1960, future = data, method = "growth")
  }

  # Fill in trailing zeros with closest value
  data$x <- toolInterpolateAndExtrapolate(data$x)

  list(x = data$x,
       weight = NULL,
       unit = glue("mil. {toolGetUnitDollar(inPPP = TRUE)}"),
       description = data$description)
}

calcInternalGDPPast <- function(pastData) {
  # Call appropriate calcInternalGDPPast function.
  data <- switch(
    pastData,
    "WDI"   = readSource("WDI", "gdp"),
    "MI"    = readSource("MissingIslands", "gdp"),
    "James" = toolGDPPastJames(),
    stop("Bad input for calcGDPPast. Invalid 'pastData' argument.")
  )

  getNames(data) <- pastData

  list(x = data,
       weight = NULL,
       unit = glue("mil. {toolGetUnitDollar(inPPP = TRUE)}"),
       description = glue("{pastData} data"))
}

toolGDPPastJames <- function() {
  gdppc <- readSource("James", "gdp")
  pop <- calcOutput("PopulationPast", aggregate = FALSE)
  years <- intersect(getYears(gdppc), getYears(pop))
  gdppc[, years, ] * pop[, years, ]
}


#' @rdname calcGDPPast
#' @param scenario A string specifying the scenario, from which the historic GDP and population data sources are taken.
#' @order 2
calcGDPpcPast <- function(scenario = "SSPs") {
  # We can not fill data sources as in GDP and Pop, as GDP and pop on their part are filled with MI, and the countries
  # which are filled in do not match. (WDI has pop data for some countries, but not GDP.) So to make sure that the
  # GDP per capita is consistent, we have to pass it on to the GDP and pop functions.
  gdpPastData <- toolGetScenarioDefinition("GDPpc", scenario, aslist = TRUE)$pastData
  popPastData <- toolGetScenarioDefinition("Population", scenario, aslist = TRUE)$pastData
  gdp <- calcOutput("GDPPast", pastData = gdpPastData, aggregate = FALSE)
  pop <- calcOutput("PopulationPast", pastData = popPastData, aggregate = FALSE)
  years <- intersect(getYears(gdp), getYears(pop))
  data <- gdp[, years, ] / pop[, years, ]
  data[is.nan(data) | data == Inf] <- 0
  getNames(data) <- paste(gdpPastData, "gdp data", "over", popPastData, "pop data")

  # The weight does not go into the weight of the final scenario. So exact matching with GDPpc not necessary...
  weight <- pop
  getNames(weight) <- getNames(data)
  # Make sure weight and data have the same yearly resolution.
  ## Sometimes weght has more years than x, thus the intersect operation.
  weight <- weight[, intersect(getYears(data), getYears(weight)), ]
  ## If x has more years than weight, add these years and interpolate
  missingYears <- getYears(data)[! getYears(data) %in% getYears(weight)]
  weight <- add_columns(weight, missingYears, dim = 2, fill = 0)
  weight <- weight[, sort(getYears(weight)), ]
  weight <- toolInterpolateAndExtrapolate(weight)

  list(x = data, weight = weight, unit = toolGetUnitDollar(inPPP = TRUE), description = glue("{getNames(data)}"))
}
