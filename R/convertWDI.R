#' Convert WDI
#'
#' Convert WDI converts data from readWDI() to ISO country level. Adds Taiwan
#' as difference from global total.
#'
#' @param x MAgPIE object containing WDI data
#' @inheritParams readWDI
#' @inherit readWDI return
#' @family WDI functions
convertWDI <- function(x, subtype) {

  if (subtype %in% c("SP.POP.TOTL",
                     "NY.GDP.MKTP.PP.KD",
                     "NY.GDP.MKTP.PP.CD",
                     "NY.GDP.MKTP.CD",
                     "NY.GDP.MKTP.CN",
                     "NY.GDP.MKTP.KD",
                     "NY.GDP.MKTP.KN",
                     "NV.AGR.TOTL.KD",
                     "NV.AGR.TOTL.CD")) {
    # Change scale of indicators
    x <- x / 1e+6
    # Add Kosovo to Serbia
    x["RS", , ] <- dimSums(x[c("RS", "XK"), , ], dim = 1, na.rm = TRUE)
  } else {
    vcat("Warning: Kosovo left out of conversion and has differing population values from FAO", verbosity = 2)
  }

  getCells(x) <- countrycode::countrycode(getCells(x),
                                          "iso2c",
                                          "iso3c",
                                          custom_match = c("JG" = "JEY"),
                                          warn = FALSE)

  x <- toolGeneralConvert(x)

  # Remove years which only contain 0s as entries
  x <- x[, !apply(x, 2, function(y) all(y == 0)), ]
  x
}
