#' Convert James data
#' 
#' Convert James data on ISO country level.
#' 
#' @param x MAgPIE object containing James data region resolution
#' @param subtype subtype of GDP indicator to be selected
#' @return Krausmann data as MAgPIE object aggregated/disaggregated to country
#' level
#' @seealso \code{\link{readJames}}
#' @examples
#' 
#' \dontrun{ a <- convertJames(x,"IHME_USD05_PPP_pc")
#' }
convertJames <- function(x, subtype) {
  x <- x[c("ANT", "SUN"),,, invert = TRUE]
  y <- toolCountryFill(x[,, subtype], fill = 0) 
  return(y)
}  
