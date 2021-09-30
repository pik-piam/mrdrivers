#' calcGDPpcPast
#'
#' @inheritParams calcGDPpc
#' @inherit calcGDPpc return
#' 
#' @seealso [madrat::calcOutput]
#' @family GDPpc functions
#' 
#' @examples \dontrun{
#' library(mrdrivers)
#' calcOutput("GDPpcPast")}
#' 
calcGDPpcPast <- function(GDPpcPast = "WDI",
                          unit = "constant 2005 Int$PPP",
                          useMIData = TRUE) {

  # Call appropriate calcGDPPast function.
  data <- switch(GDPpcPast,
                 "WDI" = cGDPpcPastWDI(unit, useMIData),
                 stop("Bad input for calcGDPpcPast. Invalid 'GDPpcPast' argument."))

  weight <- calcOutput("PopulationPast", 
                       PopulationPast = GDPpcPast,
                       useMIData = useMIData,
                       aggregate = FALSE)

  list(x = data, weight = weight, unit = unit, description = glue("GDPpc data from {GDPpcPast}."))
}


######################################################################################
# Functions
######################################################################################
cGDPpcPastWDI <- function(unit, useMIData) {
  gdp <- calcOutput("GDPPast", GDPPast = "WDI", unit = unit, useMIData = useMIData, aggregate = FALSE)
  pop <- calcOutput("PopulationPast", PopulationPast = "WDI", useMIData = useMIData, aggregate = FALSE)
  years <- intersect(getYears(gdp), getYears(pop))

  data <- gdp[, years,] / pop[, years,]
  data <- setNames(data, "gdppc_WDI")
  data[is.nan(data) | data == Inf] <- 0
  data
}
