#' Get GDP and GDP per capita scenarios
#'
#' @description
#' Like all scenarios in mrdrivers, the GDP and GDP per capita scenarios are the result of a harmonization exercise
#' between past data and future projections. Together with the corresponding population scenarios
#' (see [calcPopulation()]) they comprise a consistent set of scenarios.
#'
#' By default the following scenarios are returned:
#'  \itemize{
#'    \item the SSPs, i.e. SSP1-5
#'    \item the SDPs, i.e. SDP, SDP_EI, SDP_RC, and SDP_MC
#'  }
#'
#' See the vignette: \code{vignette("scenarios")} for scenario options, definitions and references.
#'
#' @inheritParams calcDriver
#'
#' @param unit A string specifying the unit of GDP. Can be either:
#' \itemize{
#'   \item "`r toolGetUnitDollar(inPPP = TRUE)`" (default):
#'          Scenarios are constructed in `r toolGetUnitDollar(inPPP = TRUE)`.
#'   \item "`r toolGetUnitDollar()`": Scenarios are constructed in `r toolGetUnitDollar()` and then converted with
#'   [GDPuc::toolConvertGDP()].
#' }
#' In all cases, GDP is returned in millions.
#'
#' @param average2020 If TRUE (default), then the 2020 value is replaced by the 2018-2022 average. To be consistent,
#' the yearly resolution is decreased to 5 year intervals.
#'
#' @param ... Arguments passed on to [calcDriver()], of which "extension2150" and "naming" are most often of interest.
#'
#' @inherit madrat::calcOutput return
#' @seealso  \itemize{
#'   \item [toolGetScenarioDefinition()] for scenario options and definitions.
#'   \item [madrat::calcOutput()] for how to return supplementary information and other control options.
#'   \item [calcDriver()], [calcScenarioConstructor()] and [calcHarmonizedData()] for how to create new scenarios
#'     (for developers).
#' }
#'
#' @examples \dontrun{
#' # Return default scenarios
#' calcOutput("GDP")
#' calcOutput("GDPpc")
#'
#' # Return only the SSP2 GDP scenario
#' calcOutput("GDP", scenario = "SSP2")
#' }
#'
calcGDP <- function(scenario = c("SSPs", "SDPs", "SSP2EU"),
                    unit = toolGetUnitDollar(inPPP = TRUE),
                    average2020 = TRUE,
                    ...) {
  # Check user input
  toolCheckUserInput(driver = "GDP", args = c(list(...), as.list(environment())))

  # GDP scenarios are constructed in 2017 Int$PPP, and converted, if necessary, at the end.
  gdp <- calcOutput("Driver",
                    driver = "GDP",
                    scenario = scenario,
                    aggregate = FALSE,
                    supplementary = TRUE,
                    ...)

  if (average2020) {
    # For REMIND, the consensus is to average the 2020 value so as to dampen the effect of the COVID shock. (The
    # reasoning being that REMIND uses 5-year time steps, and that the year-in-itself should represent the 2,5 years
    # before and after.)
    xNew2020 <- (gdp$x[, 2018, ] + gdp$x[, 2019, ] + gdp$x[, 2020, ] + gdp$x[, 2021, ] + gdp$x[, 2022, ]) / 5
    getYears(xNew2020) <- 2020
    getSets(xNew2020) <- getSets(gdp$x)
    gdp$x[, 2020, ] <- xNew2020
    gdp$description <- paste(gdp$description, "|| 2020 value averaged over 2018-2022 time period.")
    # Return only 5 year time steps, since the yearly data around 2020 is not connected to the 2020 value anymore.
    years5ts <- getYears(gdp$x, as.integer = TRUE)[getYears(gdp$x, as.integer = TRUE) %% 5 == 0 &
                                                     getYears(gdp$x, as.integer = TRUE) != 1960]
    gdp$x <- gdp$x[, years5ts, ]
    gdp$weight <- gdp$weight[, years5ts, ]
    gdp$description <- paste(gdp$description, "5 year time steps only.")
    message("The 2020 value is an an avergae over the 2018-2022 time period!! Only returning 5 year time steps.")
  }

  # Convert to US$MER if required
  if (grepl("US\\$MER", unit)) {
    # Convert by interpolating and extrapolating missing conversion factors when possible.
    gdp$x <- GDPuc::toolConvertGDP(gdp$x,
                                   unit_in = toolGetUnitDollar(inPPP = TRUE),
                                   unit_out = toolGetUnitDollar(inPPP = FALSE),
                                   replace_NAs = c("linear", "no_conversion"))
  }

  if (!grepl(toolGetUnitDollar(returnOnlyBase = TRUE), unit)) {
    warning(glue("Shifting to non-default base-year prices, using only the US deflator for all countries, and \\
                  neglecting any changes in PPPs or MERs. Standard conversion would not guarantee that the \\
                  relationships between countries (an important charateristic of the scenario design) remain the \\
                  same."))
    gdp$x <- gdp$x * GDPuc::toolConvertSingle(1, "USA", unit_in = toolGetUnitDollar(inPPP = TRUE), unit_out = unit)
  }

  list(x = gdp$x, weight = gdp$weight, unit = glue("mil. {unit}"), description = gdp$description)
}


#' @rdname calcGDP
#' @examples \dontrun{
#' calcOutput("GDPpc")
#' }
#'
calcGDPpc <- function(scenario = c("SSPs", "SDPs", "SSP2EU"),
                      unit = toolGetUnitDollar(inPPP = TRUE),
                      average2020 = TRUE,
                      ...) {
  # Check user input
  toolCheckUserInput(driver = "GDPpc", args = c(list(...), as.list(environment())))

  # GDPpc scenarios are constructed in 2017 Int$PPP, and converted, if necessary, at the end.
  gdppc <- calcOutput("Driver",
                      driver = "GDPpc",
                      scenario = scenario,
                      popAsWeight = TRUE,
                      aggregate = FALSE,
                      supplementary = TRUE,
                      ...)

  if (average2020) {
    # For REMIND, the consensus is to average the 2020 value so as to dampen the effect of the COVID shock. (The
    # reasoning being that REMIND uses 5-year time steps, and that the year-in-itself should represent the 2,5 years
    # before and after.)
    # The dampening is supposed to take place on GDP. So for GDP per capita in 2020 to be consistent with the dampened
    # GDP, it has to calculated from GDP and population. (In other words we can't just use the same formula as for GDP,
    # since it would lead to inconsistency at the end.) This is a bit hacky...
    gdp2020 <- calcOutput("GDP",
                          scenario = scenario,
                          naming = "scenario",
                          extension2150 = "none",
                          aggregate = FALSE,
                          years = 2020)
    pop2020 <- calcOutput("Population",
                          scenario = scenario,
                          naming = "scenario",
                          extension2150 = "none",
                          aggregate = FALSE,
                          years = 2020)
    gdppc2020 <- gdp2020 / pop2020
    gdppc2020[is.nan(gdppc2020)] <- 0
    getNames(gdppc2020) <- getNames(gdppc$x)
    gdppc$x[, 2020, ] <- gdppc2020
    gdppc$description <- paste(gdppc$description, "|| 2020 value averaged over 2018-2022 time period.")
    # Return only 5 year time steps, since the yearly data around 2020 is not connected to the 2020 value anymore.
    years5ts <- getYears(gdppc$x, as.integer = TRUE)[getYears(gdppc$x, as.integer = TRUE) %% 5 == 0 &
                                                       getYears(gdppc$x, as.integer = TRUE) != 1960]
    gdppc$x <- gdppc$x[, years5ts, ]
    gdppc$weight <- gdppc$weight[, years5ts, ]
    gdppc$description <- paste(gdppc$description, "5 year time steps only.")
    message("The 2020 value is an an avergae over the 2018-2022 time period!!")
  }

  # Convert to US$MER if required
  if (grepl("US\\$MER", unit)) {
    # Convert by interpolating and extrapolating missing conversion factors when possible.
    gdppc$x <- GDPuc::toolConvertGDP(gdppc$x,
                                     unit_in = toolGetUnitDollar(inPPP = TRUE),
                                     unit_out = toolGetUnitDollar(inPPP = FALSE),
                                     replace_NAs = c("linear", "no_conversion"))
  }

  if (!grepl(toolGetUnitDollar(returnOnlyBase = TRUE), unit)) {
    warning(glue("Shifting to non-default base-year prices, using only the US deflator for all countries, and \\
                  neglecting any changes in PPPs or MERs. Standard conversion would not guarantee that the \\
                  relationships between countries (an important charateristic of the scenario design) remain the \\
                  same."))
    gdppc$x <- gdppc$x * GDPuc::toolConvertSingle(1, "USA", unit_in = toolGetUnitDollar(inPPP = TRUE), unit_out = unit)
  }

  list(x = gdppc$x, weight = gdppc$weight, unit = unit, description = gdppc$description)
}
