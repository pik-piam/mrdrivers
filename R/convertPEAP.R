#' Convert Population Estimates And Projections from the World Bank
#'
#' Convert data from the World Bank's Population Estimates And Projections
#'
#' @param x MAgPIE object returned by readPEAP
#' @inherit readPEAP return
#' @family PEAP functions
convertPEAP <- function(x) {
  toolGeneralConvert(x, warn = FALSE, note = FALSE)
}
