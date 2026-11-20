#' Bind magpie objects with different temporal dimensions
#'
#' Bind magpie objects even if they differ in the temporal dimension, in which any missing years are added and filled
#' with NAs.
#'
#' @param x A magpie object
#' @param y A magpie object
#'
#' @returns A magpie object
#' @keywords internal
toolMbindAddMissingYears <- function(x, y) {
  if (is.null(x)) {
    return(NULL)
  }

  cy <- commonYears(x, y)
  xnew <- add_columns(x, dim = 2, addnm = getYears(y)[!getYears(y) %in% cy]) %>% magpiesort()
  ynew <- add_columns(y, dim = 2, addnm = getYears(x)[!getYears(x) %in% cy]) %>% magpiesort()
  mbind(xnew, ynew)
}
