#' calcUrbanPast
#' 
#' Calculates a time series of urban shares
#' 
#' @inheritParams calcUrban
#' @inherit calcUrban return
#' 
#' @seealso [madrat::calcOutput()]
#' @family Urban functions
#' 
#' @examples \dontrun{
#' library(mrdrivers)
#' calcOutput("UrbanPast")}
#' 
calcUrbanPast <- function(UrbanPast = "WDI") {

  data <- switch(
    UrbanPast,
    "WDI" = readSource("WDI", "SP.URB.TOTL.IN.ZS") / 100,
    stop("Bad input for UrbanPast. Invalid 'UrbanPast' argument.")
  )

  getNames(data) <- "urbanPop"
  data <- toolFinishingTouches(data)

  wp <- calcOutput("PopulationPast", PopulationPast = UrbanPast, aggregate = FALSE)

  list(x = data, weight = wp, unit = "per 1", description = glue("Urbanisation data from {UrbanPast}"))
}
