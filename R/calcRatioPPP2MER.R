#' MER over PPP ratio
#'
#' Get a conversion factor to convert GDP in constant 2017 Int$PPP into constant 2017 US$MER.
#' Use the "when" argument to switch the year of the conversion factor. Source = WDI. Countries with missing data are
#' filled in with 1. Regional aggregation is weighed by GDP from WDI-MI in the year set by "when".
#'
#' @param when An integer (defaults to 2017) specifying the year of the PPP2MER factor.
#' @inherit madrat::calcOutput return
#' @seealso [madrat::calcOutput()]
#' @examples \dontrun{
#' calcOutput("RatioPPP2MER")
#' }
#'
calcRatioPPP2MER <- function(when = 2017) {

  data <- readSource("WDI", "PA.NUS.PPPC.RF")[, when, ]
  # Replace 0s with 1s. This was done previously. Other solutions here should be taken into consideration.
  data[data == 0] <- 1

  weight <- calcOutput("GDPPast", aggregate = FALSE, years = when)

  list(x = data,
       weight = weight,
       unit = glue::glue("constant {when} US$MER / constant {when} Int$PPP"),
       description = glue::glue("Ratio of GDP in constant {when} US$MER over GDP in constant {when} Int$PPP (source: \\
                                WDI). Can be used to convert between GDP at constant {when} Int$PPP and GDP at \\
                                constant {when} US$MER."))
}
