# These countries are the ones (essential the EU-27) that receive special treatment
# in the SSP2EU scenario.
toolGetEUcountries <- function() {
  x <- toolGetMapping("regionmappingH12.csv", type = "regional", where = "mappingfolder") %>%
    tibble::as_tibble() %>%
    dplyr::filter(.data$RegionCode == "EUR", .data$CountryCode != "GBR") %>%
    dplyr::pull(.data$CountryCode)

  x
}
