# These countries are the ones (essentiall the EU-27) that receive special treatment
# in the SSP2EU scenario.
toolGetEUcountries <- function(onlyWithARIADNEgdpData = FALSE) {
  x <- toolGetMapping("regionmappingH12.csv", type = "regional", where = "mappingfolder") %>%
    tibble::as_tibble() %>%
    dplyr::filter(.data$RegionCode == "EUR", .data$CountryCode != "GBR") %>%
    dplyr::pull(.data$CountryCode)

  if (onlyWithARIADNEgdpData) {
     x <- x[x %in% where(readSource("ARIADNE", "gdp_corona") != 0)$true$regions]
  }
  x
}
