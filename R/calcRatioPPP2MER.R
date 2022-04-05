#' Get PPP to MER ratio
#'
#' @param from A string indicating the source
#' @param when An integer with the year for which the PPP2MER ratio is to
#'   be returned.
#'
#' @seealso [madrat::calcOutput()]
#'
#' @examples \dontrun{
#' library(mrdrivers)
#' calcOutput("calcfrom")
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
    stop("Bad input for calcfrom. Invalid 'from' argument.")
  }

  weight <- calcOutput("GDPPast", aggregate = FALSE)[, when, ]

  # TMP: have to use old sets and names for now, to not break interfaces
  getSets(data) <- c("Region", "year", "d3")
  getNames(data) <- NULL
  getYears(data) <- NULL

  list(x = data,
       weight = weight,
       unit = "constant 2005 US$MER / constant 2005 Int$PPP",
       description = glue::glue("Ratio of GDP in constant 2005 US$MER over GDP in constant 2005 Int$PPP (source: \\
                                {from}). Can be used to convert between GDP at constant 2005 Int$PPP and GDP at \\
                                constant 2005 US$MER."))
}
