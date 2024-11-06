#' Tool used to consolidate the most common "convert" operations
#'
#' The most important and common "convert" operations are:
#' \itemize{
#'   \item removing undefined countries,
#'   \item substituting NAs, see the "substituteNAsWith" argument,
#'   \item using default set names "iso3c", "year", and "variable",
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
#' @export
#'
#' @examples \dontrun{
#' toolGeneralConvert(x)
#' }
toolGeneralConvert <- function(x,
                               useDefaultSetNames = TRUE,
                               countryFillWith = 0,
                               substituteNAsWith = 0,
                               warn = TRUE,
                               note = TRUE,
                               ...) {

  # Remove any NA-countries
  x <- x[!is.na(getCells(x)), , ]

  # Substitute NAs
  x[is.na(x)] <- substituteNAsWith

  # Remove years which only 0s or substituteNAsWith
  x <- x[, purrr::map_lgl(getYears(x), ~!all(x[, .x, ] == substituteNAsWith) & !all(x[, .x, ] == 0)), ]

  # Use default setNames
  if (useDefaultSetNames) getSets(x) <- c("iso3c", "year", "variable")

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
  x[, sort(getYears(x)), ]
}
