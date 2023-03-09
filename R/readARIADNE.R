#' Read ARIADNE Reference Scenario
#'
#' Read ARIADNE Reference Scenario data from various .xls files as magpie object
#'
#' @param subtype data subtype. Either "population", "gdp", or "gdp_corona"
#'
#' @seealso [madrat::readSource()]
#'
#' @return magpie object of ARIADNE reference scenario data by country
#' @order 1
readARIADNE <- function(subtype) {

  switch(subtype,
         "population" = rARIADNEPopulation(),
         "gdp" = rARIADNEGDP(corona = FALSE),
         "gdp_corona" = rARIADNEGDP(corona = TRUE),
         stop("Bad input for readARIADNE. Invalid 'subtype' argument."))
}


######################################################################################
# Functions
######################################################################################
rARIADNEPopulation <- function() {
  readxl::read_excel("POP_EU-27_Eurostat.xlsx", range = "B12:T46", sheet = "Pop_Total") %>%
    suppressMessages() %>%
    dplyr::rename("eurostat" = "...1") %>%
    dplyr::slice(-c(28:30)) %>%
    dplyr::mutate(eurostat = c(.data$eurostat[1:27], "IS", "LI", "NO", "CH")) %>%
    tidyr::pivot_longer(-1, names_to = "year") %>%
    dplyr::mutate(value = .data$value * 1e-6,
                  variable = factor("Population (million)"),
                  .before = "value") %>%
    as.magpie(spatial = 1)
}

rARIADNEGDP <- function(corona) {
  gdpSheet <- suppressMessages(readxl::read_excel("GDP_Base_Corona_EU-28_V02.xlsx", range = "A2:AL30")) %>%
    # Drop columns with only NAs
    dplyr::select(tidyselect::vars_select_helpers$where(~ !all(is.na(.x)))) %>%
    # Pivot columns
    tidyr::pivot_longer(cols = tidyselect::starts_with("2"), names_to = "year") %>%
    dplyr::rename("eurostat" = "Regions")

  # Split off years that apply to both the base and corona scenarios, and duplicate
  gdp1 <- gdpSheet %>%
    dplyr::filter(nchar(.data$year) == 4) %>%
    dplyr::rename("base" = "value") %>%
    dplyr::mutate("corona" = .data$base, year = as.integer(.data$year))

  # Identify base and corona scenarios
  gdp2 <- gdpSheet %>%
    dplyr::filter(!nchar(.data$year) == 4) %>%
    tidyr::separate(.data$year, c("year", "scen")) %>%
    dplyr::mutate(year = as.integer(.data$year),
                  scen = ifelse(dplyr::row_number() %% 2 == 0, "corona", "base")) %>%
    tidyr::pivot_wider(names_from = "scen")

  # Combine and add variable description column
  gdp <- dplyr::bind_rows(gdp1, gdp2) %>%
    dplyr::arrange(.data$eurostat, .data$year) %>%
    dplyr::mutate(variable = "GDP|MER (million euro 2005/yr)") %>%
    dplyr::relocate("variable", .after = "year")

  # Choose which scenario
  gdp <- if (corona) dplyr::select(gdp, -"base") else dplyr::select(gdp, -"corona")

  as.magpie(gdp, spatial = "eurostat", temporal = "year", tidy = TRUE)
}


#' @rdname readARIADNE
#' @order 2
#' @param x MAgPIE object returned by readARIADNE
convertARIADNE <- function(x, subtype) {

  # Convert region codons from Eurostat to iso3c
  getItems(x, 1) <- countrycode::countrycode(getItems(x, 1), "eurostat", "iso3c")
  getSets(x)[1] <- "iso3c"

  if (subtype %in% c("gdp", "gdp_corona")) {
    # Convert currency:
    # First we use the country-sepcific defltors to change the base year, and then we
    # divide by 0.8041, a fixed value sourced from the World Bank's WDI
    # (1 US$2005 = 0.8041 €2005) to get the values in 2005 US$MER.
    # In a second step we convert from US$MER to Int$PPP
    # The reason we can't do the conversin at once here is that x is not in LCU but in €.
    x <- GDPuc::convertGDP(x, "constant 2010 LCU", "constant 2005 LCU") / 0.8041
    x <- GDPuc::convertGDP(x, "constant 2005 US$MER", "constant 2005 Int$PPP")

    # Rename
    getNames(x) <- "GDP|PPP (million US$2005/yr)"
  }

  x <- toolGeneralConvert(x, note = FALSE, countryFillWith = NA, substituteNAsWith = NA)
  # fill smaller EU-countries with 0s to allow for aggregation of EU-region
  x[c("ALA", "FRO", "GIB", "GGY", "IMN", "JEY"), , ] <- 0

  return(x)
}




#' @rdname readARIADNE
readARIADNE_ReferenceScenario <- function(subtype) { # nolint
  readSource("ARIADNE", subtype = subtype)
}
