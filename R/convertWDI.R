#' Convert WDI
#'
#' Convert WDI converts data from readWDI() to ISO country level. Adds Taiwan
#' as difference from global total.
#'
#' @param x MAgPIE object containing WDI data
#' @inheritParams readWDI
#' @inherit readWDI return
#' 
#' @family WDI functions
#' 
convertWDI <- function(x, subtype){
  
  if (subtype %in% c("SP_POP_TOTL",
                     "NY_GDP_MKTP_PP_KD",
                     "NY_GDP_MKTP_PP_CD",
                     "NY_GDP_MKTP_CD", 
                     "NY_GDP_MKTP_CN", 
                     "NY_GDP_MKTP_KD",
                     "NY_GDP_MKTP_KN", 
                     "NV_AGR_TOTL_KD", 
                     "NV_AGR_TOTL_CD")) {
    # Change scale of indicators
    x <- x / 1e+6
    # Add Kosovo to Serbia
    x["RS",,] <- dimSums(x[c("RS", "XK"),,], dim = 1, na.rm = TRUE)
  } else {
    vcat("Warning: Kosovo left out of conversion and has differing population values from FAO", verbosity=2)
  } 

  getCells(x) <- countrycode::countrycode(getCells(x), 
                                          "iso2c", 
                                          "iso3c", 
                                          custom_match = c("JG" = "JEY"),
                                          warn = FALSE)
  # Above warnings are ignored consciously. Non-country entites turn into NAs and must
  # therefore be removed
  x <- x[!is.na(getCells(x)),,]
  getSets(x)[1] <- "iso3c"
  
  # Remove Antarctica, fill in missing countries and replace NAs with 0
  x <- x["ANT",,, invert = TRUE]
  x <- toolCountryFill(x, fill = 0)
  x[is.na(x)] <- 0

  # Rsemove years which only contain 0s as entries
  x <- x[,!apply(x, 2, function(y) all(y == 0)), ]
  
  # UNSURE if necessary
  x <- clean_magpie(x)
  x <- x[, sort(getYears(x)), ]

  getSets(x)[3] <- "variable"
  x
}
