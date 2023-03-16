#' MER over PPP ratio
#'
#' Get a conversion factor to convert GDP in constant 2005 Int$PPP into constant 2005 US$MER.
#' Use the when argument to switch the year of the conversion factor.
#'
#' @param from A string indicating the source. Can be either "WDI" (default) or "OECD".
#' @param when An integer (defaults to 2005) specifying the year of the PPP2MER factor.
#' @inherit madrat::calcOutput return
#' @seealso [madrat::calcOutput()]
#' @examples \dontrun{
#' library(mrdrivers)
#' calcOutput("RatioPPP2MER")
#' }
#'
calcRatioPPP2MER <- function(from = "WDI", when = 2005) {

  if (from == "WDI") {
    data <- readSource("WDI", "PA.NUS.PPPC.RF")[, when, ]
    # Replace 0s with 1s. This was done previously. Other solutions here should be taken into consideration.
    data[data == 0] <- 1
  } else if (from == "OECD") {
    data <- readSource("OECD", subtype = "ratioPM")
  } else {
    stop("Bad input for calcRatioPPP2MER. Invalid 'from' argument.")
  }

  weight <- calcOutput("GDPPast", aggregate = FALSE)[, when, ]

  # TMP: have to use old sets and names for now, to not break interfaces
  getSets(data) <- c("Region", "year", "d3")
  getNames(data) <- NULL
  getYears(data) <- NULL

  list(x = data,
       weight = weight,
       unit = glue::glue("constant {when} US$MER / constant {when} Int$PPP"),
       description = glue::glue("Ratio of GDP in constant {when} US$MER over GDP in constant {when} Int$PPP (source: \\
                                {from}). Can be used to convert between GDP at constant {when} Int$PPP and GDP at \\
                                constant {when} US$MER."))
}
