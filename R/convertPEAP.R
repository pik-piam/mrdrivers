#' Convert Population Estimates And Projections from the World Bank
#' 
#' Convert data from the World Bank's Population Estimates And Projections
#' 
#' @return MAgPIE object 
#' @param x MAgPIE object
convertPEAP <- function(x) {
  x <- x[!is.na(getCells(x)),,]
  x <- x["ANT",,,invert=TRUE]
  x <- clean_magpie(x)
  x <- toolCountryFill(x, fill = 0)
  x[is.na(x)] <- 0
  x <- x[,sort(getYears(x)),]
}
