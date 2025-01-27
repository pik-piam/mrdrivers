#' Read data from India's Department of Economic Affairs (DEA)
#'
#' Read-in an DEA data as magclass object
#'
#' @inherit madrat::readSource return
#' @seealso [madrat::readSource()]
#' @examples \dontrun{
#' readSource("IndiaDEA", subtype = "gdppc")
#' }
#' @order 1
readIndiaDEA <- function() {
  baselineCase <- readxl::read_xlsx("GDP growth projections.xlsx",
                                    sheet = "Summary Sheet",
                                    range = "G3:J20",
                                    col_types = rep.int("numeric", 4),
                                    progress = FALSE) %>%
    dplyr::select(3:4) %>%
    dplyr::mutate("scenario" = "baseline", "year" = seq(2020, 2100, 5), "iso3c" = "IND") %>%
    tidyr::pivot_longer(1:2, names_to = "variable")

  optimisticCase <- readxl::read_xlsx("GDP growth projections.xlsx",
                                      sheet = "Summary Sheet",
                                      range = "K3:N20",
                                      col_types = rep.int("numeric", 4),
                                      progress = FALSE) %>%
    dplyr::select(3:4) %>%
    dplyr::mutate("scenario" = "optimistic", "year" = seq(2020, 2100, 5), "iso3c" = "IND") %>%
    tidyr::pivot_longer(1:2, names_to = "variable")

  dplyr::bind_rows(baselineCase, optimisticCase) %>%
    as.magpie(spatial = "iso3c", temporal = "year", tidy = TRUE, filter = FALSE)
}

#' @rdname readIndiaDEA
#' @order 2
#' @param x MAgPIE object returned from readIndiaDEA
#' @param subtype A string, either "all", "gdppc", "pop"
#' @param subset A vector of strings designating the scenarios. Defaults to c("IndiaMedium", "IndiaHigh").
convertIndiaDEA <- function(x, subtype = "all", subset = c("IndiaMedium", "IndiaHigh")) {
  if (!subtype %in% c("all", "gdppc", "pop")) {
    stop("Bad input for readDEA. Invalid 'subtype' argument. Available subtypes are 'all', 'gdppc', and 'pop'.")
  }
  # Rename scenarios to IndiaMedium and IndiaHigh
  getNames(x, dim = "scenario") <- c("IndiaMedium", "IndiaHigh")

  # Filter scenario
  x <- mselect(x, scenario = subset)

  # Filter for subtype in the convert Function to use common read cache
  if (subtype == "gdppc") {
    x <- mselect(x, variable = "Per Capita GDP Level (in thousands 2017PPP$)")
    # Convert from thousands to $
    x <- x * 1e3
  }
  if (subtype == "pop") x <- mselect(x, variable = "Population (in million)")

  # Reduce dimension by summation when possible
  if (subtype != "all") x <- dimSums(x, dim = c("variable"))

  toolGeneralConvert(x, useDefaultSetNames = subtype != "all", note = FALSE)
}

#' @rdname readIndiaDEA
#' @order 3
downloadIndiaDEA <- function() {
  stop("Manual download of DEA data required!")
  # Compose meta data
  list(url           = "",
       doi           = "-",
       title         = "DEA projections",
       description   = "DEA projections",
       unit          = "-",
       author        = "DEA",
       release_date  = "2024",
       license       = "-",
       comment       = "Manual download required! Accessed on the 11.09.2024.")
}
