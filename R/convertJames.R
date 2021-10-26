#' Convert James data
#'
#' Convert James data on ISO country level.
#'
#' @param x MAgPIE object returned by readJames
#' @inheritParams readJames
#' @inherit readJames return
#' @family James functions
convertJames <- function(x, subtype) {
  x <- x[c("ANT", "SUN"), , , invert = TRUE]
  toolGeneralConvert(x[, , subtype], useDefaultSetNames = FALSE)
}
