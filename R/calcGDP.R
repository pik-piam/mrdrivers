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
#'    \item SSP2EU
#'  }
#'
#' See the vignette: \code{vignette("scenarios")} for scenario options, definitions and references.
#'
#' @inheritParams calcDriver
#'
#' @param unit A string specifying the unit of GDP. Can be either:
#' \itemize{
#'   \item "constant 2017 Int$PPP" (default): Scenarios are constructed in constant 2017 Int$PPP.
#'   \item "constant 2017 US$MER": Scenarios are constructed in constant 2017 Int$PPP and then converted with
#'   [GDPuc::toolConvertGDP()].
#' }
#' In all cases, GDP is returned in millions.
#'
#' @param average2020 If TRUE (default), then the 2020 value is replaced by the 2018-2022 average. To be consistent,
#' the yearly resolution is decreased to 5 year intervals.
#'
#' @param ... Arguments passed on to [calcDriver()], of which "extension2150" and "naming" are most often of interest.
#' Other [calcDriver()] arguments are used for scenario fine-tuning and by package developers.
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
#' # Return only the SSP2EU GDP scenario
#' calcOutput("GDP", scenario = "SSP2EU")
#' }
#'
calcGDP <- function(scenario = c("SSPs", "SDPs", "SSP2EU"),
                    unit = "constant 2017 Int$PPP",
                    average2020 = TRUE,
                    ...) {
  # Check user input
  toolCheckUserInput(driver = "GDP", args = c(list(...), as.list(environment())))

  # GDP scenarios are constructed in 2017 Int$PPP, and converted, if necessary, at the end.
  gdp <- calcOutput("Driver",
                    driver = "GDP",
                    scenario = scenario,
                    unit = "constant 2017 Int$PPP",
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
                               unit_in = "constant 2017 Int$PPP",
                               unit_out = "constant 2017 US$MER",
                               replace_NAs = c("linear", "no_conversion"))
  }
  # Temporary shifting to 2005 prices, using only the US deflator for all countries, and neglecting any changes in
  # PPPs or MERs.
  if (grepl("2005", unit)) gdp$x <- gdp$x * 0.8121123

  list(x = gdp$x, weight = gdp$weight, unit = glue("mil. {unit}"), description = gdp$description)
}
