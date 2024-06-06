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
#' @param naming A string giving the naming scheme of the data dimension. Can be either:
#' \itemize{
#'   \item "indicator_scenario" (default): Returns names of the type "gdp_SSP2", or "pop_SSP2".
#'   \item "indicator.scenario": Returns names of the type "gdp.SSP2", or "pop.SSP2".
#'   \item "scenario": Returns names of the type "SSP2".
#' }
#' Set naming to "scenario" when you want to operate on SSP2 gdp and population data for instance, and not have to
#' worry about the conflicting names.
#'
#' @param popAsWeight If TRUE, then population data of the same scenario is used as weight.
#'
#' @inheritDotParams calcScenarioConstructor
#' @inherit madrat::calcOutput return
#' @inherit calcHarmonizedData seealso
#' @keywords internal
calcDriver <- function(driver,
                       scenario,
                       popAsWeight = FALSE,
                       naming = "indicator_scenario",
                       extension2150 = "bezier",
                       ...) {
  # Load ... arguments into function environment
  list2env(list(...), environment())

  # Temporary warning left as information. Remove in next release.
  if ("FiveYearSteps" %in% ls()) {
    warning("FiveYearSteps is deprecated and will throw an error in the next release.")
    rm("FiveYearSteps")
  }

  # If the pastData, futureData and harmonization arguments are not in ..., then query them using "scenario" and
  # load them into the function environment.
  if (!any(c("pastData", "futureData", "harmonization") %in% ls())) {
    list2env(toolGetScenarioDefinition(driver, scenario, aslist = TRUE), environment())
  } else {
    scenario <- "-"
  }

  # Create a list of all the arguments
  l <- as.list(environment())

  # Call ScenarioConstructor function the appropriate number of times (map) and combine (reduce)
  # !! Keep formula syntax for madrat caching to work
  purrr::pmap(l, ~calcOutput("ScenarioConstructor", aggregate = FALSE, supplementary = TRUE, ...)) %>%
    toolReduce()
}

#' ScenarioConstructor
#'
#' @details # Combining data sources with "-"
#' Data sources can be combined with "-" and passed to both the pastData and futureData arguments, i.e. "WDI-MI". This
#' signifies that WDI data will be taken first, but missing data will be then be filled in with data from MI.
#'
#' @param harmonization A string designating the harmonization function.
#' @param pastData A string passed to the calc'Driver'Past function, e.g. [calcGDPPast()] or [calcPopulationPast()].
#' @param futureData A string passed to the calc'Driver'Future function, e.g. [calcGDPFuture()] or
#'   [calcPopulationFuture()].
#' @param ... Arguments passed on to the 'driver'Past, 'driver'Future and driver'Harmonization' functions.
#' @inheritParams calcDriver
#' @inherit calcHarmonizedData seealso
#' @inherit madrat::calcOutput return
#' @keywords internal
calcScenarioConstructor <- function(driver,
                                    scenario,
                                    pastData,
                                    futureData,
                                    harmonization,
                                    extension2150,
                                    naming,
                                    popAsWeight,
                                    ...) {

  harmonizedData <- calcOutput("HarmonizedData",
                               driver = driver,
                               scenario = scenario,
                               pastData = pastData,
                               futureData = futureData,
                               harmonization = harmonization,
                               aggregate = FALSE,
                               supplementary = TRUE,
                               ...)

  harmonizedData$x <- toolFinishingTouches(x = harmonizedData$x, extension2150 = extension2150, naming = naming)

  weight <- NULL
  description <- harmonizedData$description
  if (popAsWeight) {
    weight <- calcOutput("Population",
                         scenario = scenario,
                         extension2150 = extension2150,
                         aggregate = FALSE,
                         supplementary = TRUE)
    # Give weight same names as data, so that aggregate doesn't mess up data dim
    getNames(weight$x) <- getNames(harmonizedData$x)
    # Make sure weight and harmonizedData have the same yearly resolution.
    # Sometimes weght has more years than x, thus the intersect operation.
    weight$x <- weight$x[, intersect(getYears(harmonizedData$x), getYears(weight$x)), ]
    # If x has more years than weight, add these years and interpolate
    missingYears <- getYears(harmonizedData$x)[! getYears(harmonizedData$x) %in% getYears(weight$x)]
    weight$x <- add_columns(weight$x, missingYears, dim = 2, fill = 0)
    weight$x <- weight$x[, sort(getYears(weight$x)), ]
    weight$x <- toolInterpolateAndExtrapolate(weight$x, extrapolate = FALSE)

    description <- glue("{description} Associated {weight$description}")
  }

  if (extension2150 == "bezier") {
    description <- glue("{description} Extended from 2100 to 2150 using bezier curves, resulting in a smooth \\
                        flattening of the scenario (the slope in 2150 is equal to half of that in 2100).")
  } else if (extension2150 == "constant") {
    description <- glue("{description} Extended from 2100 to 2150 using the constant 2100 value.")
  }

  list(x = harmonizedData$x,
       weight = weight$x,
       unit = harmonizedData$unit,
       description = glue("{driver} {scenario} scenarios: {description}"))
}


#' Get Harmonized Data
#'
#' @param ... Arguments passed on to harmonization functions
#' @inheritParams calcScenarioConstructor
#' @inherit madrat::calcOutput return
#' @seealso \itemize{
#'   \item [calcGDPPast()], [calcGDPFuture()], [calcGDPpcPast()] and [calcGDPpcFuture()] for the builiding blocks of
#'     the GDP and GDPpc scenarios.
#'   \item [calcPopulationPast()], [calcPopulationFuture()], [calcLabourPast()], [calcLabourFuture()], [calcUrbanPast()]
#'     and [calcUrbanFuture()] for the builiding blocks of the Population, Labor and Urban scenarios.
#' }
#' @keywords internal
calcHarmonizedData <- function(driver, scenario, pastData, futureData, harmonization, ...) {
  # Depending on the setup, the scenario construction either requires 'past' and 'future' scenarios, or not!
  # For example, many GDP scenarios are actually constructed as GDPpc scenarios, and then simply multiplied with
  # population scenarios.
  past <- if (pastData != "-") {
    calcOutput("PastData", driver = driver, pastData = pastData, aggregate = FALSE, supplementary = TRUE, ...)
  } else NULL
  future <- if (futureData != "-") {
    calcOutput("FutureData", driver = driver, futureData = futureData, aggregate = FALSE, supplementary = TRUE, ...)
  } else NULL

  switch(
    driver,
    "Population" = calcOutput("PopulationHarmonized",
                              harmonization = harmonization,
                              past = past,
                              future = future,
                              aggregate = FALSE,
                              supplementary = TRUE,
                              ...),
    "GDP"        = calcOutput("GDPHarmonized",
                              harmonization = harmonization,
                              past = past,
                              future = future,
                              scenario = scenario,
                              aggregate = FALSE,
                              supplementary = TRUE,
                              ...),
    "GDPpc"      = calcOutput("GDPpcHarmonized",harmonization = harmonization,
                              past = past,
                              future = future,
                              scenario = scenario,
                              aggregate = FALSE,
                              supplementary = TRUE,
                              ...),
    "Labour"     = calcOutput("LabourHarmonized",harmonization = harmonization,
                              past = past,
                              future = future,
                              aggregate = FALSE,
                              supplementary = TRUE,
                              ...),
    "Urban"      = calcOutput("UrbanHarmonized",harmonization = harmonization,
                              past = past,
                              future = future,
                              aggregate = FALSE,
                              supplementary = TRUE,
                              ...)
  )
}

#' Get Past Data Building Block
#'
#' @inheritParams calcScenarioConstructor
#' @inherit madrat::calcOutput return
#' @inherit calcHarmonizedData seealso
#' @keywords internal
calcPastData <- function(driver, pastData, unit) {
  switch(
    driver,
    "GDP"        = calcOutput("GDPPast",
                              GDPPast = pastData,
                              unit = unit,
                              aggregate = FALSE,
                              supplementary = TRUE),
    "GDPpc"      = calcOutput("GDPpcPast",
                              GDPpcPast = pastData,
                              unit = unit,
                              aggregate = FALSE,
                              supplementary = TRUE),
    "Population" = calcOutput("PopulationPast",
                              PopulationPast = pastData,
                              aggregate = FALSE,
                              supplementary = TRUE),
    "Urban"      = calcOutput("UrbanPast",
                              UrbanPast = pastData,
                              aggregate = FALSE,
                              supplementary = TRUE),
    "Labour"     = calcOutput("LabourPast",
                              LabourPast = pastData,
                              aggregate = FALSE,
                              supplementary = TRUE)
  )
}

#' Get Future Data Building Block
#'
#' @inheritParams calcScenarioConstructor
#' @inherit madrat::calcOutput return
#' @inherit calcHarmonizedData seealso
#' @keywords internal
calcFutureData <- function(driver, futureData, unit) {
  switch(
    driver,
    "Population" = calcOutput("PopulationFuture",
                              PopulationFuture = futureData,
                              aggregate = FALSE,
                              supplementary = TRUE),
    "GDP"        = calcOutput("GDPFuture",
                              GDPFuture = futureData,
                              unit = unit,
                              aggregate = FALSE,
                              supplementary = TRUE),
    "GDPpc"      = calcOutput("GDPpcFuture",
                              GDPpcFuture = futureData,
                              unit = unit,
                              aggregate = FALSE,
                              supplementary = TRUE),
    "Labour"     = calcOutput("LabourFuture",
                              LabourFuture = futureData,
                              aggregate = FALSE,
                              supplementary = TRUE),
    "Urban"      = calcOutput("UrbanFuture",
                              UrbanFuture = futureData,
                              aggregate = FALSE,
                              supplementary = TRUE)
  )
}
