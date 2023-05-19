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
#'   \item "constant 2005 Int$PPP" (default): Scenarios are constructed in constant 2005 Int$PPP.
#'   \item "constant 2005 US$MER": Scenarios are constructed in constant 2005 Int$PPP and converted before being
#'                                 returned with [GDPuc::convertGDP()].
#'   \item "constant 2017 Int$PPP": Scenarios are constructed in constant 2017 Int$PPP.
#'   \item "constant 2017 US$MER": Scenarios are constructed in constant 2017 Int$PPP and converted before being
#'                                 returned with [GDPuc::convertGDP()].
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
#' library(mrdrivers)
#'
#' # Return default scenarios
#' calcOutput("GDP")
#'
#' # Return only the SSP2EU GDP scenario
#' calcOutput("GDP", scenario = "SSP2EU")
#'
#' # Return the now-outdated GDP scenarios used before summer 2021,
#' calcOutput("GDP", scenario = "SSPsOld", extension2150 = "constant", average2020 = FALSE)
#' }
#'
calcGDP <- function(scenario = c("SSPs", "SDPs", "SSP2EU"),
                    unit = "constant 2005 Int$PPP",
                    average2020 = TRUE,
                    ...) {
  # Check user input
  toolCheckUserInput(driver = "GDP", args = c(list(...), as.list(environment())))

  # GDP scenarios are constructed in PPPs. If MERs are desired, scenarios with the
  # same base year but in PPPs are constructed, and converted to MERs at the end.
  constructUnit <- unit
  if (grepl("^constant .* US\\$MER$", unit)) {
    constructUnit <- paste0("constant ",  substr(unit, 10, 13), " Int$PPP")
  }

  gdp <- calcOutput("Driver",
                    driver = "GDP",
                    scenario = scenario,
                    unit = constructUnit,
                    aggregate = FALSE,
                    supplementary = TRUE,
                    ...)

  if (average2020 && any(grepl("SSPsOld", scenario))) {
    warning("Average 2020 is not compatible with SSPsOld. Setting to FALSE.")
    average2020 <- FALSE
  }
  if (average2020) {
    # For REMIND, the concensus is to avergae the 2020 value so as to dampen the effect of the COVID shock. (The
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

  if (constructUnit != unit) {
    # Convert by interpolating and extrapolating missing conversion factors when possible.
    gdp$x <- GDPuc::convertGDP(gdp$x, constructUnit, unit, replace_NAs = c("linear", "no_conversion"))
  }

  list(x = gdp$x, weight = gdp$weight, unit = glue("mil. {unit}"), description = gdp$description)
}
