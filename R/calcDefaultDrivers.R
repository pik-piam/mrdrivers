#' Collect Default Model Drivers
#' 
#' @param drivers Vector of strings.
#' @return A magpie object.
calcDefaultDrivers <- function(drivers = c("Population", "Urban", "GDP", "GDPpc")) {
  
  if (!all(drivers %in% c("Population", "Urban", "GDP", "GDPpc"))) {
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
    "Urban"      = calcOutput("Urban",      aggregate = FALSE, supplementary = TRUE),
    "GDP"        = calcOutput("GDP",        aggregate = FALSE, supplementary = TRUE),
    "GDPpc"      = calcOutput("GDPpc",      aggregate = FALSE, supplementary = TRUE)
  )

  return(list(x = d$x,
              weight = NULL,
              unit = d$unit,
              description = d$description))
}
