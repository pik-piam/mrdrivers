#' Get population and labour scenarios
#'
#' @description
#' Like all scenarios in mrdrivers, the Population, Labour and Urban population share scenarios are the result of a
#' harmonization exercise between past data and future projections.
#'
#' The `scenario` argument is used to designate the scenario(s) to be returned. Currently available Population
#' scenarios are: `r toolGetScenarioDefinition(driver = "Population")$scenario`. See the vignette:
#' \code{vignette("scenarios")} and/or [toolGetScenarioDefinition()] for more information, scenario options,
#' definitions and references.
#'
#' @inheritParams calcDriver
#' @inheritDotParams calcDriver extension2150
#' @inherit calcGDP return
#' @inherit calcGDP seealso
#'
#' @examples \dontrun{
#' # Return all SSP scenarios
#' calcOutput("Population", scenario = "SSPs")
#'
#' # Return only the SSP2 GDP scenario
#' calcOutput("Population", scenario = "SSP2")
#'
#' # Return the SSP and SDP Labour scenarios
#' calcOutput("Population", scenario = c("SSPs", "SDPs"))
#' }
#' @order 1
calcPopulation <- function(scenario, ...) {
  toolCheckUserInput(driver = "Population", args = c(list(...), as.list(environment())))
  calcOutput("Driver", driver = "Population", scenario = scenario, aggregate = FALSE, supplementary = TRUE, ...)
}

#' @rdname calcPopulation
#' @order 2
#' @examples \dontrun{
#' calcOutput("Labour", scenario = "SSPs")
#' }
calcLabour <- function(scenario, ...) {
  toolCheckUserInput(driver = "Labour", args = c(list(...), as.list(environment())))
  calcOutput("Driver", driver = "Labour", scenario = scenario, aggregate = FALSE, supplementary = TRUE, ...)
}

#' @rdname calcPopulation
#' @order 3
#' @param asShare If TRUE (default) urban population shares are returned. If FALSE, then urban population in millions is
#' returned.
#' @examples \dontrun{
#' calcOutput("Urban", scenario = "SSPs")
#' }
#'
calcUrban <- function(scenario, asShare = TRUE, ...) {
  toolCheckUserInput(driver = "Urban", args = c(list(...), as.list(environment())))

  urb <- calcOutput("Driver",
                    driver = "Urban",
                    scenario = scenario,
                    popAsWeight = TRUE,
                    aggregate = FALSE,
                    supplementary = TRUE,
                    ...)

  # Cap urban share at 99%.
  urb$x[urb$x > 0.99] <- 0.99

  if (!asShare) {
    urb$x <- urb$x * urb$weight
    urb$weight <- NULL
    urb$unit <- "million"
  }

  list(x = urb$x, weight = urb$weight, unit = urb$unit, description = urb$description)
}
