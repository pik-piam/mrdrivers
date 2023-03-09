#' @rdname calcPopulation
#' @examples \dontrun{
#' library(mrdrivers)
#' calcLabour()
#' }
#'
calcLabour <- function(scenario = c("SSPs", "SDPs", "SSP2EU"), ...) {
  toolCheckUserInput(driver = "Labour", args = c(list(...), as.list(environment())))
  calcOutput("Driver", driver = "Labour", scenario = scenario, aggregate = FALSE, supplementary = TRUE, ...)
}
