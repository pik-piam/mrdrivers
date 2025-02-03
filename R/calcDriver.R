#' Vectorised scenario construction
#'
#' @description
#' Internal function called by the main mrdrivers calc functions, that maps over any vectorised arguments. Should only
#' be of interest to package developers.
#'
#' @details # Vectorization of arguments
#'  Vectors are accepted for most arguments.
#'  If given a vector, different combinations are created and returned all at once. If more than one
#'  argument is vectorised, the arguments have to have the same length. Which time series are created can be
#'  illustrated with the following example. Let's say the "harmonization" and "pastdata" arguments are vectors of
#'  length 3. Then there will be in total 3 time series that are produced: the first time series is the result of
#'  combining the first harmonization element with the first pastdata element, the second time series the result of
#'  combining the second harmonization element with the second pastdata element, and the third time series the result
#'  of using the respective third entry. The futuredata element used in each case is the same, since in this example
#'  only one futuredata element is provided.
#'
#' @param driver A string designating the driver. Available drivers are:
#' \itemize{
#'  \item GDP
#'  \item Population
#'  \item GDPpc
#'  \item Labour
#'  \item Urban
#' }
#'
#' @param scenario A string (or vector of strings) designating the scenario(s) to be returned. Use
#' [toolGetScenarioDefinition()] to learn what scenarios are available.
#'
#' @param extension2150 A string specifying if/how the scenarios should be extended until 2150. Can be either:
#' \itemize{
#'   \item "bezier" (default): A bezier curve extension that leads to a smooth flattening of the scenario: the
#'         slope in the last year of the scenario is halved by 2150. Currently only works for scenarios with 2100 as
#'         their last year.
#'   \item "constant": The last value of the scenarios is taken as constant until 2150.
#'   \item "none": No extension.
#' }
#'
#' @param naming DEPRECATED - will be removed in next release. A string giving the naming scheme of the data dimension.
#' Can be either:
#' \itemize{
#'   \item "scenario" (default): Returns names of the type "SSP2".
#'   \item "indicator_scenario": Returns names of the type "gdp_SSP2", or "pop_SSP2".
#' }
#' Set naming to "scenario" when you want to operate on SSP2 gdp and population data for instance, and not have to
#' worry about the conflicting names.
#'
#' @param popAsWeight If TRUE, then population data of the same scenario is used as weight.
#'
#' @inherit madrat::calcOutput return
#' @inherit calcHarmonizedData seealso
#' @keywords internal
calcDriver <- function(driver,
                       scenario,
                       popAsWeight = FALSE,
                       naming = "scenario",
                       extension2150 = "bezier") {
  # Create a list of all the arguments
  l <- as.list(environment())

  # Call ScenarioConstructor function the appropriate number of times (map) and combine (reduce)
  # !! Keep formula syntax for madrat caching to work
  purrr::pmap(l, ~calcOutput("ScenarioConstructor", aggregate = FALSE, supplementary = TRUE, ...)) %>%
    purrr::reduce(~list(x = mbind(.x$x, .y$x),
                        weight = mbind(.x$weight, .y$weight),
                        unit = .x$unit,
                        description = glue("{.x$description} || {.y$description}")))
}

#' Scenario construction
#'
#' @details # Combining data sources with "-"
#' Data sources can be combined with "-" and passed to both the pastData and futureData arguments, i.e. "WDI-MI". This
#' signifies that WDI data will be taken first, but missing data will be then be filled in with data from MI.
#'
#' @inheritParams calcDriver
#' @inherit calcHarmonizedData seealso
#' @inherit madrat::calcOutput return
#' @keywords internal
calcScenarioConstructor <- function(driver, scenario, popAsWeight, naming, extension2150) {

  data <- calcOutput("HarmonizedData", driver = driver, scenario = scenario, aggregate = FALSE, supplementary = TRUE)

  if (extension2150 != "none") data <- toolExtend2150(data, extension2150)

  # If required, add indicators (drivers) to names, or as additional dimension
  if (naming != "scenario") {
    warning("The naming argument is deprecated and will be removed in the next release.")
    indicator <- switch(
      driver,
      "GDP"        = "gdp",
      "GDPpc"      = "gdppc",
      "Population" = "pop",
      "Urban"      = "urb",
      # Label labour scenarios with "pop". Currently required for REMIND to work.
      "Labour"     = "pop"
    )
    if (naming == "indicator_scenario") {
      getNames(data$x) <- paste0(indicator, "_", getNames(data$x))
    }
  }

  # If required, get population as weight
  if (popAsWeight) {
    weight <- calcOutput("Population",
                         scenario = scenario,
                         extension2150 = extension2150,
                         aggregate = FALSE,
                         supplementary = TRUE)
    # Give weight same names as data, so that aggregate doesn't mess up data dim
    getNames(weight$x) <- getNames(data$x)
    # Make sure weight and data have the same yearly resolution.
    ## Sometimes weght has more years than x, thus the intersect operation.
    weight$x <- weight$x[, intersect(getYears(data$x), getYears(weight$x)), ]
    ## If x has more years than weight, add these years and interpolate
    missingYears <- getYears(data$x)[! getYears(data$x) %in% getYears(weight$x)]
    weight$x <- add_columns(weight$x, missingYears, dim = 2, fill = 0)
    weight$x <- weight$x[, sort(getYears(weight$x)), ]
    weight$x <- toolInterpolateAndExtrapolate(weight$x)

    data$description <- glue("{data$description} Associated {weight$description}")
  } else {
    weight <- NULL
  }

  list(x = data$x,
       weight = weight$x,
       unit = data$unit,
       description = glue("{driver} {scenario} scenarios: {data$description}"))
}


#' Get harmonized data
#'
#' @inheritParams calcScenarioConstructor
#' @inherit madrat::calcOutput return
#' @seealso [calcGDPPast()] for the scenario builiding blocks.
#' @keywords internal
calcHarmonizedData <- function(driver, scenario) {
  # Query the pastData, futureData and harmonization arguments of the "scenario".
  l <- toolGetScenarioDefinition(driver, scenario, aslist = TRUE)
  pastData      <- l$pastData
  futureData    <- l$futureData
  harmonization <- l$harmonization

  # Depending on the setup, the scenario construction either requires 'past' and 'future' data, or not!
  # For example, many GDP scenarios are actually constructed as GDPpc scenarios, and then simply multiplied with
  # population scenarios.
  past <- if (pastData != "-") {
    switch(
      driver,
      "GDP"        = calcOutput("GDPPast", pastData = pastData, aggregate = FALSE, supplementary = TRUE),
      "GDPpc"      = calcOutput("GDPpcPast", scenario = scenario, aggregate = FALSE, supplementary = TRUE),
      "Population" = calcOutput("PopulationPast", pastData = pastData, aggregate = FALSE, supplementary = TRUE),
      "Urban"      = calcOutput("UrbanPast", pastData = pastData, aggregate = FALSE, supplementary = TRUE),
      "Labour"     = calcOutput("LabourPast", pastData = pastData, aggregate = FALSE, supplementary = TRUE)
    ) %>% toolInterpolateAndExtrapolate()
  }
  future <- if (futureData != "-") {
    switch(
      driver,
      "GDP"        = calcOutput("GDPFuture", futureData = futureData, aggregate = FALSE, supplementary = TRUE),
      "GDPpc"      = calcOutput("GDPpcFuture", scenario = scenario, aggregate = FALSE, supplementary = TRUE),
      "Population" = calcOutput("PopulationFuture", futureData = futureData, aggregate = FALSE, supplementary = TRUE),
      "Labour"     = calcOutput("LabourFuture", futureData = futureData, aggregate = FALSE, supplementary = TRUE),
      "Urban"      = calcOutput("UrbanFuture", futureData = futureData, aggregate = FALSE, supplementary = TRUE),
    ) %>% toolInterpolateAndExtrapolate()
  }

  # Combine "past" and "future" time series.
  harmonizedData <- switch(
    harmonization,
    "pastAndLevel"        = toolHarmonizePast(past, future, method = "level"),
    "pastAndGrowth"       = toolHarmonizePast(past, future, method = "growth"),
    "pastAndTransition"   = toolHarmonizePast(past, future, method = "transition", yEnd = 2100),
    "PopSSPs"             = toolHarmonizeWithPEAPandFuture(past, future),
    "PopSSP2IndiaDEAs"    = toolHarmonizePopulationSSP2IndiaDEAs(past, future),
    "PopISIMIP"           = toolHarmonizePast(past, future, method = "transition", yEnd = 2030),
    "GDPpcSSPs"           = toolHarmonizeGDPpcSSPs(past, future, yEnd = 2100),
    "GDPpcSDPs"           = toolBuildGDPpcSDPs(),
    "GDPpcSSP2IndiaDEAs"  = toolHarmonizeGDPpcSSP2IndiaDEAs(past, future),
    "GDPoverPop"          = toolDivideGDPbyPop(scenario),
    "GDPpcWithPop"        = toolMultiplyGDPpcWithPop(scenario),
    "LabourSSP2IndiaDEAs" = toolHarmonizeLabourSSP2IndiaDEAs(),
    stop(glue("Bad input for calcHarmonizedData Argument harmonization = '{harmonization}' is invalid."))
  ) %>% toolInterpolateAndExtrapolate()

  unit <- switch(
    driver,
    "GDP"        = glue("mil. {toolGetUnitDollar(inPPP = TRUE)}"),
    "GDPpc"      = toolGetUnitDollar(inPPP = TRUE),
    "Population" = "million",
    "Urban"      = "share of population",
    "Labour"     = "million"
  )

  list(x = harmonizedData$x, weight = NULL, unit = unit, description = harmonizedData$description)
}
