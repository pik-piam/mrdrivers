toolGeneralConvert <- function(x,
                               useDefaultSetNames = TRUE,
                               countryFillWith = 0,
                               NASubstituteWith = 0,
                               warn = TRUE,
                               no_remove_warning = NULL) {

  # Remove any NA-countries
  x <- x[!is.na(getCells(x)),,]
  
  # Use default setNames
  if (useDefaultSetNames) getSets(x) <- c("iso3c", "year", "variable")
    
  # Substitute NAs
  x[is.na(x)] <- NASubstituteWith    

  # Check whether the country list agrees with the list of countries in the madrat library
  # and remove unrequired data, add missing data 
  if (warn) {
    x <- toolCountryFill(x, fill = countryFillWith, no_remove_warning = no_remove_warning)
  } else {
    x <- toolCountryFill(x, fill = countryFillWith , no_remove_warning = no_remove_warning) %>%
      suppressWarnings()
  }

  # Sort by year
  x <- x[, sort(getYears(x)), ]
}