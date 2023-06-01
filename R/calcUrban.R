#' @rdname calcPopulation
#' @param asShare If TRUE (default) urban population shares are returned. If FALSE, then urban population in millions is
#' returned.
#' @examples \dontrun{
#' calcOutput("Urban")
#' }
#'
calcUrban <- function(scenario = c("SSPs", "SDPs", "SSP2EU"), asShare = TRUE, ...) {
  toolCheckUserInput(driver = "Urban", args = c(list(...), as.list(environment())))

  urb <- calcOutput("Driver",
                    driver = "Urban",
                    scenario = scenario,
                    popAsWeight = TRUE,
                    aggregate = FALSE,
                    supplementary = TRUE,
                    ...)

  if (!asShare) {
    urb$x <- urb$x * urb$weight
    urb$weight <- NULL
    urb$unit <- "million"
  }

  list(x = urb$x, weight = urb$weight, unit = urb$unit, description = urb$description)
}
