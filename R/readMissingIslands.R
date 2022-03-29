#' Read in the "Missing Islands" dataset
#'
#' @description Read in core data for minor islands which are not included in big inventories but have a countrycode
#'
#' @param subtype pop for population or gdp for gdp
#'
#' @seealso [madrat::readSource()]
#' @seealso [downloadMissingIslands()]
#'
#' @return Magpie object
#' @examples \dontrun{
#' readSource("MissingIslands", subtype = "pop", convert = FALSE)
#' }
#'
readMissingIslands <- function(subtype) {

  files <- c(pop = "pop_past_missing.csv", gdp = "gdp_past_missing.csv")
  file <- toolSubtypeSelect(subtype = subtype, files = files)

  x <- utils::read.csv(file, header = TRUE)
  names(x) <- substring(names(x), 1, 5)
  as.magpie(x)
}

#' @rdname readMissingIslands
#' @param x MAgPIE object returned by readMissingIslands
convertMissingIslands <- function(x) {
  toolGeneralConvert(x, note = FALSE)
}
