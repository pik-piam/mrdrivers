# These countries are the ones (essentiall the EU-27) that receive special treatment
# in the SSP2EU scenario. 
toolGetEURcountries <- function() {
  toolGetMapping("regionmappingH12.csv") %>% 
    tibble::as_tibble() %>% 
    dplyr::filter(.data$RegionCode == "EUR", .data$CountryCode != "GBR") %>% 
    dplyr::pull(.data$CountryCode)
}
