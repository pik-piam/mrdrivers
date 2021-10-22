# These countries are the ones (essentiall the EU-27) that receive special treatment
# in the SSP2EU scenario. 
toolGetEUcountries <- function(only_countries_with_ARIADNE_gdp_data = FALSE) {
  x <- toolGetMapping("regionmappingH12.csv") %>% 
    tibble::as_tibble() %>% 
    dplyr::filter(.data$RegionCode == "EUR", .data$CountryCode != "GBR") %>% 
    dplyr::pull(.data$CountryCode)

  if (only_countries_with_ARIADNE_gdp_data) {
     x <- x[x %in% where(readSource("ARIADNE", "gdp_corona") != 0 )$true$regions]
  }
  x
}
