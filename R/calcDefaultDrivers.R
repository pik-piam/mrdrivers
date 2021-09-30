#' Collect Default Model Drivers
#' 
#' @param drivers Vector of strings.
#' @inherit calcGDP return
#' 
#' @seealso [madrat::calcOutput]
#' 
#' @examples \dontrun{
#' library(mrdrivers)
#' calcOutput("DefaultDrivers")}
#' 
calcDefaultDrivers <- function(drivers = c("Population", "GDP", "UrbanPop")) {
  
  if (!all(drivers %in% c("Population", "GDP", "UrbanPop"))) {
     stop("Bad input for DefaultDrivers. Invalid 'drivers' argument.")
  }
  
  toolInternalCalc("DefaultDrivers", list(drivers))
}

######################################################################################
# Internal Function
######################################################################################
internal_calcDefaultDrivers <- function (drivers) {

  d <- switch(
    drivers, 
    "Population" = calcOutput("Population", aggregate = FALSE, supplementary = TRUE),
    "GDP"        = calcOutput("GDP",        aggregate = FALSE, supplementary = TRUE),
    "UrbanPop"   = calcOutput("UrbanPop",   aggregate = FALSE, supplementary = TRUE)
  )

  list(x = d$x, weight = NULL, unit = d$unit, description = d$description)
}
