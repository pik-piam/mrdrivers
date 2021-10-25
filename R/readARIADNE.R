#' Read ARIADNE Reference Scenario
#' 
#' Read ARIADNE Reference Scenario data from various .xls files as magpie object
#' 
#' @param subtype data subtype. Either "population", "gdp", or "gdp_corona"
#' 
#' @seealso [madrat::readSource()]
#' @family "Future" GDP functions
#' @family "Future" Population functions
#' @family ARIADNE functions
#' 
#' @return magpie object of ARIADNE reference scenario data by country
readARIADNE <- function(subtype){

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
  populationSheet <- suppressMessages(
    readxl::read_excel('POP_EU-27_Eurostat.xlsx', range='B12:T46', sheet='Pop_Total')
  )
  populationSheet <- populationSheet[c(seq(1,27), seq(31,34)),]
  colnames(populationSheet)[1] <- 'eurostat'
  populationSheet$eurostat <- c(populationSheet$eurostat[1:27], 'IS', 'LI', 'NO', 'CH')
  populationSheet[,seq(2,19)] <- sapply(populationSheet[,seq(2,19)], as.numeric)
  populationSheet[,seq(2,19)] <- populationSheet[,seq(2,19)] / 1000000
  populationSheet <- tidyr::pivot_longer(populationSheet, -1)
  populationSheet <- cbind(c('Population (million)'), populationSheet)
  colnames(populationSheet) <- c('variable', 'eurostat','year','value')
  as.magpie(populationSheet, spatial = 2, temporal = 3, datacol = 4)
}

rARIADNEGDP <- function(corona) {
  gdpSheet <- suppressMessages(readxl::read_excel('GDP_Base_Corona_EU-28_V02.xlsx', range = 'A2:AL30')) %>% 
    # Drop columns with only NAs
    dplyr::select(tidyselect::vars_select_helpers$where(~!all(is.na(.x)))) %>% 
    # Pivot columns
    tidyr::pivot_longer(cols = tidyselect::starts_with("2"), names_to = "year") %>% 
    dplyr::rename("eurostat" = .data$Regions) 

  # Split off years that apply to both the base and corona scenarios, and duplicate
  gdp_1 <- gdpSheet %>% 
    dplyr::filter(nchar(.data$year) == 4) %>% 
    dplyr::rename("base" = .data$value) %>% 
    dplyr::mutate("corona" = .data$base, year = as.integer(.data$year))

  # Identify base and corona scenarios
  gdp_2 <- gdpSheet %>% 
    dplyr::filter(!nchar(.data$year) == 4) %>% 
    tidyr::separate(.data$year, c("year", "scen")) %>% 
    dplyr::mutate(year = as.integer(.data$year), 
                  scen = ifelse(dplyr::row_number() %% 2 == 0, "corona", "base")) %>% 
    tidyr::pivot_wider(names_from = .data$scen)

  # Combine and add variable description column
  gdp <- dplyr::bind_rows(gdp_1, gdp_2) %>% 
    dplyr::arrange(.data$eurostat, .data$year) %>% 
    dplyr::mutate(variable = "GDP|MER (million euro 2005/yr)") %>% 
    dplyr::relocate(.data$variable, .after = "year")

  # Choose which scenario
  gdp <- if(corona) dplyr::select(gdp, -.data$base) else dplyr::select(gdp, -.data$corona)

  as.magpie(gdp, spatial = "eurostat", temporal = "year", tidy = TRUE)
}

#' Read ARIADNE Reference Scenario
#' 
#' Alias to [readARIADNE].
#' 
#' @param subtype data subtype. Either "population", "gdp", or "gdp_corona"
#' 
#' @seealso [madrat::readSource()]
#' @family ARIADNE functions
#' 
#' @return magpie object of ARIADNE reference scenario data by country
readARIADNE_ReferenceScenario <- function(subtype){
  readSource("ARIADNE", subtype = subtype)
}
