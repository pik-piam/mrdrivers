#' Convert MissingIslands
#'
#' Convert data from the MissingIslands dataset
#'
#' @param x MAgPIE object returned by readMissingIslands
#' @inherit readMissingIslands return
#' @family MissingIslands functions
convertMissingIslands <- function(x) {
  toolGeneralConvert(x, note = FALSE)
}
