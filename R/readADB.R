#' Read ADB data
#'
#' Read-in an ADB data as magclass object
#'
#' @inherit madrat::readSource return
#' @seealso [madrat::readSource()]
#' @examples \dontrun{
#' readSource("ADB", subtype = "gdppc")
#' }
#' @order 1
readADB <- function() {
  baselineCase <- readxl::read_xlsx("GDP growth projections.xlsx",
                                    sheet = "Summary Sheet",
                                    range = "G3:J20",
                                    col_types = rep.int("numeric", 4),
                                    progress = FALSE) %>%
    dplyr::select(3:4) %>%
    dplyr::mutate("scenario" = "ADBbaseline", "year" = seq(2020, 2100, 5), "iso3c" = "IND") %>%
    tidyr::pivot_longer(1:2, names_to = "variable")

  optimisticCase <- readxl::read_xlsx("GDP growth projections.xlsx",
                                      sheet = "Summary Sheet",
                                      range = "K3:N20",
                                      col_types = rep.int("numeric", 4),
                                      progress = FALSE) %>%
    dplyr::select(3:4) %>%
    dplyr::mutate("scenario" = "ADBoptimistic", "year" = seq(2020, 2100, 5), "iso3c" = "IND") %>%
    tidyr::pivot_longer(1:2, names_to = "variable")

  dplyr::bind_rows(baselineCase, optimisticCase) %>%
    as.magpie(spatial = "iso3c", temporal = "year", tidy = TRUE, filter = FALSE)
}

#' @rdname readADB
#' @order 2
#' @param x MAgPIE object returned from readADB
#' @param subtype A string, either "all", "gdppc", "pop"
convertADB <- function(x, subtype = "all") {
  if (!subtype %in% c("all", "gdppc", "pop")) {
    stop("Bad input for readADB. Invalid 'subtype' argument. Available subtypes are 'all', 'gdppc', and 'pop'.")
  }

  # Filter for subtype in the convert Function to use common read cache
  if (subtype == "gdppc") {
    x <- mselect(x,
                 scenario = c("ADBoptimistic", "ADBbaseline"),
                 variable = "Per Capita GDP Level (in thousands 2017PPP$)")
    # Convert from thousands to $
    x <- x * 1e3
  }
  if (subtype == "pop") {
    x <- mselect(x,
                 scenario = c("ADBoptimistic", "ADBbaseline"),
                 variable = "Population (in million)")
  }

  # Reduce dimension by summation when possible
  if (subtype != "all") x <- dimSums(x, dim = c("variable"))

  toolGeneralConvert(x, useDefaultSetNames = subtype != "all", note = FALSE)
}

#' @rdname readADB
#' @order 3
downloadADB <- function() {
  stop("Manual download of ADB data required!")
  # Compose meta data
  list(url           = "",
       doi           = "-",
       title         = "ADB projections",
       description   = "ADB projections",
       unit          = "-",
       author        = "ADB",
       release_date  = "2024",
       license       = "-",
       comment       = "Manual download required! Accessed on the 11.09.2024.")
}
