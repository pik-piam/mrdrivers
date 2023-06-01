#' Get population and labour scenarios
#'
#' @description
#' Like all scenarios in mrdrivers, the Population, Labour and Urban population share scenarios are the result of a
#' harmonization exercise between past data and future projections.
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
#' @param ... Arguments passed on to [calcDriver()], of which "extension2150" and "naming" are most often of interest.
#' Other [calcDriver()] arguments are used for scenario fine-tuning and by package developers.
#' @inherit calcGDP return
#' @inherit calcGDP seealso
#'
#' @examples \dontrun{
#' # Return the default scenarios
#' calcOutput("Population")
#'
#' # Return the SSP2EU scenario
#' calcOutput("Population", scenario = "SSP2EU")
#'
#' # Return the ISIMIP SSP scenarios
#' calcOutput("Population", scenario = "ISIMIP", extension2150 = "none", aggregate = FALSE)
#' }
#' @order 1
calcPopulation <- function(scenario = c("SSPs", "SDPs", "SSP2EU"), ...) {
  toolCheckUserInput(driver = "Population", args = c(list(...), as.list(environment())))
  calcOutput("Driver", driver = "Population", scenario = scenario, aggregate = FALSE, supplementary = TRUE, ...)
}
