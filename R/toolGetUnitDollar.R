toolGetUnitDollar <- function(returnOnlyBase = FALSE, inPPPP = FALSE) {
  base <- 2017
  if (returnOnlyBase) {
    return(base)
  }
  pppOrMer <- if (inPPPP) "Int$PPP" else "US$MER"
  paste("constant", base, pppOrMer)
}
