#' Read TWN Population projections.
#'
#' @description
#' `r lifecycle::badge('deprecated')`
#'
#' Read-in Population projections for Taiwan
#'
#' @param subtype A string. Variant of population projection.
#'
#' @return A magpie object of the TWN population data
#'
#' @seealso [madrat::readSource()]
#'
#' @examples \dontrun{ 
#' library(mrdrivers)
#' readSource("PopulationTWN", subtype = "medium")}
#' @keywords internal
readPopulationTWN <- function(subtype) {
  files <- c(medium = "A1. Population Projections - Medium Variant.xlsx",
             high = "A2. Population Projections  - High Variant.xlsx",
             low = "A3. Population Projections  - Low Variant.xlsx")
  
  file <- toolSubtypeSelect(subtype, files)
  
  twn <- as.data.frame(suppressMessages(readxl::read_excel(file, sheet = "M3", skip = 1)))
  twn <- twn[!is.na(twn[[2]]),]
    
  names(twn)[3]  <- paste(names(twn)[2], twn[1, 3],  sep="_")
  names(twn)[4]  <- paste(names(twn)[2], twn[1, 4],  sep="_")
  names(twn)[2]  <- paste(names(twn)[2], twn[1, 2],  sep="_")
  names(twn)[6]  <- paste(names(twn)[5], twn[1, 6],  sep="_")
  names(twn)[7]  <- paste(names(twn)[5], twn[1, 7],  sep="_")
  names(twn)[5]  <- paste(names(twn)[5], twn[1, 5],  sep="_")
  names(twn)[9]  <- paste(names(twn)[8], twn[1, 9],  sep="_")
  names(twn)[10] <- paste(names(twn)[8], twn[1, 10], sep="_")
  names(twn)[8]  <- paste(names(twn)[8], twn[1, 8],  sep="_")
  
  twn <- twn[-1, ]
  twn <- tidyr::pivot_longer(twn, -"Year", names_to = "variable")
  twn$value <- as.numeric(twn$value)
  twn$variable <- gsub(" +", "_", twn$variable)
  twn$variable <- gsub("_years", " years", twn$variable, fixed = TRUE)
  
    
  x <- as.magpie(twn)
  x
}
