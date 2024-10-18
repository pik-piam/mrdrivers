#' Get monetary unit
#'
#' toolGetUnitDollar returns unit used for monetary values.
#'
#' @param returnOnlyBase TRUE or FALSE (default). If true only the base year is returned (as string).
#' @param inPPP TRUE or FALSE (default). If TRUE the the string ends in 'Int$PPP', instead of 'Int$MER'.
#'
#' @return A string with the monetary unit: `r toolGetUnitDollar()`.
#' @export
#'
#' @examples
#' toolGetUnitDollar()
#' toolGetUnitDollar(inPPP = TRUE)
#' toolGetUnitDollar(returnOnlyBase = TRUE)
#'
toolGetUnitDollar <- function(returnOnlyBase = FALSE, inPPP = FALSE) {
  base <- "2017"
  if (returnOnlyBase) {
    return(base)
  }
  pppOrMer <- if (inPPP) "Int$PPP" else "US$MER"
  paste("constant", base, pppOrMer)
}
