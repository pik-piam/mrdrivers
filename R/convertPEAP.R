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
  x <- suppressWarnings(toolCountryFill(x, fill = 0))
  # The warning that is being suppressed above concerns the removal of countries
  # from the data, that are not required so to say.
  x[is.na(x)] <- 0
  x <- x[, sort(getYears(x)), ]
}
