#' Convert PWT data
#' 
#' Convert PWT data on ISO country level.
#' 
#' @param x MAgPIE object returned by readPWT
#' @inherit readPWT return
#' @family PWT functions
convertPWT <- function(x) {
  toolGeneralConvert(x, useDefaultSetNames = FALSE, NASubstituteWith = NA)
}  
