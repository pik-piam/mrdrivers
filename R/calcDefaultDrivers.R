#' Collect Default Model Drivers
#'
#' @param drivers Vector of strings.
#' @inherit calcGDP return
#' @inheritSection calcGDP Return supplementary information
#' @inheritSection calcGDP Vectorization of arguments
#'
#' @seealso [madrat::calcOutput()]
#'
#' @examples \dontrun{
#' library(mrdrivers)
#' calcOutput("DefaultDrivers")
#' }
#'
calcDefaultDrivers <- function(drivers = c("Population", "GDP", "UrbanPop")) {
  # Check user input
  if (!all(drivers %in% c("Population", "GDP", "UrbanPop"))) {
     stop("Bad input for DefaultDrivers. Invalid 'drivers' argument.")
  }

  # Call calcInternalDefaultDrivers function the appropriate number of times (map) and combine (reduce)
  # !! Keep formula syntax for madrat caching to work
  purrr::pmap(list(drivers = drivers),
              ~calcOutput("InternalDefaultDrivers", aggregate = FALSE, supplementary = TRUE, ...)) %>%
    toolReduce()
}

######################################################################################
# Internal Function
######################################################################################
calcInternalDefaultDrivers <- function(drivers) {

  d <- switch(
    drivers,
    "Population" = calcOutput("Population", aggregate = FALSE, supplementary = TRUE),
    "GDP"        = calcOutput("GDP",        aggregate = FALSE, supplementary = TRUE),
    "UrbanPop"   = calcOutput("UrbanPop",   aggregate = FALSE, supplementary = TRUE)
  )

  list(x = d$x, weight = NULL, unit = d$unit, description = d$description)
}
