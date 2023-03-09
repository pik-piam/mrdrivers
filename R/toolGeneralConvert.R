#' Tool used to consolidate the most common "convert" operations
#'
#' The most important and common "convert" operations are:
#' \itemize{
#'   \item removing undefined countries,
#'   \item using default set names "iso3c", "year", and "variable",
#'   \item substituting NAs, see the "substituteNAsWith" argument,
#'   \item fill in countries,
#'   \item sort in chronological order.
#' }
#'
#' @param x A magpie object.
#' @param useDefaultSetNames TRUE or FALSE.
#' @param countryFillWith 0.
#' @param substituteNAsWith 0.
#' @param warn TRUE or FALSE.
#' @param note TRUE or FALSE.
#' @param ... Arguments passed on to [madrat::toolCountryFill()]
#'
#' @return A magpie object.
toolGeneralConvert <- function(x,
                               useDefaultSetNames = TRUE,
                               countryFillWith = 0,
                               substituteNAsWith = 0,
                               warn = TRUE,
                               note = TRUE,
                               ...) {

  # Remove any NA-countries
  x <- x[!is.na(getCells(x)), , ]

  # Remove years which only contain NAs
  x <- x[, !apply(x, 2, function(y) all(is.na(y))), ]

  # Use default setNames
  if (useDefaultSetNames) getSets(x) <- c("iso3c", "year", "variable")

  # Substitute NAs
  x[is.na(x)] <- substituteNAsWith

  # Check whether the country list agrees with the list of countries in the madrat library
  # and remove unrequired data, add missing data
  if (warn && note) {
    x <- toolCountryFill(x, fill = countryFillWith, ...)
  } else if (!warn && note) {
    x <- toolCountryFill(x, fill = countryFillWith, ...) %>%
      suppressWarnings()
  } else if (warn && !note) {
    x <- toolCountryFill(x, fill = countryFillWith, ...) %>%
      suppressMessages()
  } else {
    x <- toolCountryFill(x, fill = countryFillWith, ...) %>%
      suppressWarnings() %>%
      suppressMessages()
  }

  # Sort by year
  x <- x[, sort(getYears(x)), ]
}
