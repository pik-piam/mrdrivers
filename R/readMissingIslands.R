#' @title readMissingIslands
#' @description Read in core data for minor islands which are not included in big inventories but have a countrycode
#' 
#' @param subtype pop for population or gdp for gdp
#' 
#' @seealso [madrat::readSource()]
#' @family "Past" population functions
#' @family "Past" GDP functions
#' @family MissingIslands functions
#' 
#' @return Magpie object
#' @examples \dontrun{ 
#' readSource("MissingIslands", subtype = "pop", convert = FALSE)}
#'
readMissingIslands <- function(subtype) {
  
  files <- c(pop = "pop_past_missing.csv", gdp = "gdp_past_missing.csv")
  file <- toolSubtypeSelect(subtype = subtype, files = files)
  
  x <- utils::read.csv(file, header = TRUE)
  names(x) <- substring(names(x), 1, 5)
  x <- as.magpie(x)
  
  x
}  
