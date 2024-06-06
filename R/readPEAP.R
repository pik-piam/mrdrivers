#' Read Population Estimates And Projections from the World Bank
#'
#' Read-in xlsx file from the World Bank's Population Estimates And Projections (PEAP)
#' The PEAP data can't seemed to be accessed by the WDI::WDI package nor the World Bank's API directly.
#' Manual download required from https://databank.worldbank.org/source/population-estimates-and-projections#
#'
#' @inherit madrat::readSource return
#' @seealso [madrat::readSource()] and [madrat::downloadSource()]
#' @order 2
readPEAP <- function() {
  file <- "Data_Extract_From_Population_estimates_and_projections_15_04_2024.csv"
  myColTypes <- readr::cols(.default = "d",
                            "Country Name" = "_",
                            "Country Code" = "c",
                            "Series Name" = "_",
                            "Series Code" = "_")
  readr::read_csv(file, col_types = myColTypes) %>%
    suppressWarnings() %>%
    # The warnings that are being suppressed above, come from character strings that can't be converted to numeric, and
    # are thus returned as NA.
    dplyr::filter(!is.na(.data$`Country Code`)) %>%
    tidyr::pivot_longer(cols = tidyselect::starts_with(c("1", "2")), names_to = "year") %>%
    dplyr::select("iso3c" = "Country Code", "year", "value") %>%
    dplyr::mutate(variable = "population", year = sub(" .*", "", .data$year)) %>%
    dplyr::relocate("value", .after = tidyselect::last_col()) %>%
    as.magpie()
}

#' @rdname readPEAP
#' @param x MAgPIE object returned by readPEAP
#' @order 3
convertPEAP <- function(x) {
  toolGeneralConvert(x, warn = FALSE, note = FALSE)
}

#' @rdname readPEAP
#' @order 1
downloadPEAP  <- function() {
 stop("Manual download of PEAP data required!")
 # Compose meta data
  list(url           = "https://databank.worldbank.org/source/population-estimates-and-projections#",
       doi           = "-",
       title         = "Population Estimates and Projections",
       description   = "Population Estimates and Projections by the World Bank 1960-2050",
       unit          = "-",
       author        = "World Bank",
       release_date  = "2024",
       license       = "-",
       comment       = "Manual download required! Accessed on the 15.04.2024.")
}
