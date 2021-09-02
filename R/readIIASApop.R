#' Read IIASApop
#' 
#' Read-in an population data csv file as magclass object
#' 
#' @return magpie object of the population data
#' @seealso \code{\link{readSource}}
#' @examples
#' \dontrun{ a <- readSource(type="IIASApop")}
readIIASApop <- function() {
  data <- utils::read.csv("Data A1 Country total population-SSPs.csv")
  data <- data[, -ncol(data)]
  data[,"cc"] <- countrycode::countrycode(data[,"cc"], origin = "iso3n", destination = "iso3c", 
                                          custom_match = c("530" = "ANT", "736" = "SDN", "830" = "XAA"))
  data <- data[!is.na(data[, c("cc")]),]
  x <- as.magpie(data, datacol = 3)
  getNames(x) <- paste0("pop_", gsub("ssp", "SSP", getNames(x)))
  x
}  
