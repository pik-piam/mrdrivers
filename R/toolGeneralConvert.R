toolGeneralConvert <- function(x,
                               useDefaultSetNames = TRUE,
                               countryFillWith = 0,
                               substituteNAsWith = 0,
                               warn = TRUE,
                               note = TRUE,
                               ...) {

  # Remove any NA-countries
  x <- x[!is.na(getCells(x)), , ]

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
