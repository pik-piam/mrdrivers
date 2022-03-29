#' Read James
#' 
#' @description
#' `r lifecycle::badge('deprecated')`  
#' 
#' Read-in GDP per-capita data from the publication James, Spencer L., Paul
#' Gubbins, Christopher JL Murray, and Emmanuela Gakidou. 2012. "Developing a
#' Comprehensive Time Series of GDP per Capita for 210 Countries from 1950 to
#' 2015." Population Health Metrics 10 (1): 12. doi:10.1186/1478-7954-10-12.
#' from a .csv file to a magclass object
#' 
#' @param subtype String indicating the data series
#' @return GDP per capita in USD05 in PPP or MER as magpie object
#' @seealso [madrat::readSource()]
#' @examples \dontrun{ 
#' readSource("James", subtype = "IHME_USD05_PPP_pc")}
#' @keywords internal
readJames <- function(subtype) {
  utils::read.csv("james.csv", sep = ";", dec = ",") %>%
    `[`(, c("ISO3", "Year", subtype)) %>%
    as.magpie(spatial = 1, temporal = 2)    
}  

#' @describeIn readJames convert function
#' @param x MAgPIE object returned by readJames
convertJames <- function(x, subtype) {
  x <- x[c("ANT", "SUN"), , , invert = TRUE]
  toolGeneralConvert(x[, , subtype], useDefaultSetNames = FALSE)
}
