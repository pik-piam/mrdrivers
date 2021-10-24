#' Get PPP to MER ratio
#' 
#' @param from A string indicating the source
#' 
#' @seealso [madrat::calcOutput()]
#' 
#' @examples \dontrun{
#' library(mrdrivers)
#' calcOutput("calcfrom")}
#' 
calcRatioPPP2MER <- function(from = "WDI", when = 2005) {

  if (from == "WDI") {
    data <- readSource("WDI", "PA.NUS.PPPC.RF")[, when, ]
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

  return(list(x = data,
              weight = weight,
              unit = "-",
              description = glue::glue("Ratio of GDP in constant 2005 Int$PPP over GDP \\
                                        in constant 2005 US$MER (source: {from}).")))
}
