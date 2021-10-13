#' calcGDPpcPast
#'
#' @inheritParams calcGDPpc
#' @inherit calcGDPpc return
#' 
#' @seealso [madrat::calcOutput()]
#' @family GDPpc functions
#' 
#' @examples \dontrun{
#' library(mrdrivers)
#' calcOutput("GDPpcPast")}
#' 
calcGDPpcPast <- function(GDPpcPast = "WDI-MI", unit = "constant 2005 Int$PPP") {
  # Call appropriate calcGDPPast function.
  data <- switch(GDPpcPast,
                 "WDI"    = cGDPpcFromGDPAndPop(GDPpcPast, unit),
                 "WDI-MI" = cGDPpcFromGDPAndPop(GDPpcPast, unit),
                 "MI"     = cGDPpcFromGDPAndPop(GDPpcPast, unit),
                 stop("Bad input for calcGDPpcPast. Invalid 'GDPpcPast' argument."))

  weight <- calcOutput("PopulationPast", 
                       PopulationPast = GDPpcPast,
                       aggregate = FALSE)

  list(x = data, weight = weight, unit = unit, description = glue("GDPpc data from {GDPpcPast}."))
}


######################################################################################
# Functions
######################################################################################
cGDPpcFromGDPAndPop <- function(GDPpcPast, unit) {
  gdp <- calcOutput("GDPPast", GDPPast = GDPpcPast, unit = unit, aggregate = FALSE)
  pop <- calcOutput("PopulationPast", PopulationPast = GDPpcPast, aggregate = FALSE)
  years <- intersect(getYears(gdp), getYears(pop))

  data <- gdp[, years,] / pop[, years,]
  data <- setNames(data, glue("gdppc_{GDPpcPast}"))
  data[is.nan(data) | data == Inf] <- 0
  data
}
