#' Convert PWT data
#' 
#' Convert PWT data on ISO country level.
#' 
#' @param x MAgPIE object containing PWT data country-region resolution
#' @family PWT functions
#' @return PWT data as MAgPIE object aggregated to country level
convertPWT <- function(x) {
  # fill all missing countries with 0
  toolCountryFill(x, fill = 0)
}  
