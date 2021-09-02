#' Read ARIADNE Reference Scenario
#' 
#' Read ARIADNE Reference Scenario data from various .xls files as magpie object
#' 
#' @param subtype data subtype. Either "population", "gdp", or "gdp_corona"
#' @return magpie object of ARIADNE reference scenario data by country
readARIADNE_ReferenceScenario <- function(subtype){

  switch(subtype,
         "population" = readARIADNEPopulation(),
         "gdp" = readARIADNEGDP(corona = FALSE),
         "gdp_corona" = readARIADNEGDP(corona = TRUE),
         stop("Bad input for readARIADNE_ReferenceScenario. Invalid 'subtype' argument."))
}
  

######################################################################################
# Functions
######################################################################################
readARIADNEPopulation <- function() {
  populationSheet <- suppressMessages(
    readxl::read_excel('POP_EU-27_Eurostat.xlsx', range='B12:T46', sheet='Pop_Total')
  )
  populationSheet <- populationSheet[c(seq(1,27), seq(31,34)),]
  colnames(populationSheet)[1] <- 'Region'
  populationSheet$Region <- c(populationSheet$Region[1:8], 'GR', populationSheet$Region[10:27], 'IS', 'LI', 'NO', 'CH')
  populationSheet[,seq(2,19)] <- sapply(populationSheet[,seq(2,19)], as.numeric)
  populationSheet[,seq(2,19)] <- populationSheet[,seq(2,19)] / 1000000
  populationSheet <- reshape2::melt(populationSheet, id.vars=1)
  populationSheet <- cbind(c('Population (million)'), populationSheet)
  colnames(populationSheet) <- c('variable', 'region','period','value')
  as.magpie(populationSheet, spatial = 2, temporal = 3, datacol = 4)
}

readARIADNEGDP <- function(corona) {
  gdpSheet <- suppressMessages(readxl::read_excel('GDP_Base_Corona_EU-28_V02.xlsx', range = 'A2:AL30')) %>% 
    # Drop columns with only NAs
    dplyr::select(tidyselect::vars_select_helpers$where(~!all(is.na(.x)))) %>% 
    # Pivot columns
    tidyr::pivot_longer(cols = tidyselect::starts_with("2"), names_to = "period") %>% 
    dplyr::rename("region" = .data$Regions) 

  # Split off years that apply to both the base and corona scenarios, and duplicate
  gdp_1 <- gdpSheet %>% 
    dplyr::filter(nchar(.data$period) == 4) %>% 
    dplyr::rename("base" = .data$value) %>% 
    dplyr::mutate("corona" = .data$base, period = as.integer(.data$period))

  # Identify base and corona scenarios
  gdp_2 <- gdpSheet %>% 
    dplyr::filter(!nchar(.data$period) == 4) %>% 
    tidyr::separate(.data$period, c("period", "scen")) %>% 
    dplyr::mutate(period = as.integer(.data$period), 
                  scen = ifelse(dplyr::row_number() %% 2 == 0, "corona", "base")) %>% 
    tidyr::pivot_wider(names_from = .data$scen)

  # Combine and add variable description column
  gdp <- dplyr::bind_rows(gdp_1, gdp_2) %>% 
    dplyr::arrange(.data$region, .data$period) %>% 
    dplyr::mutate(variable = "GDP|MER (million euro 2005/yr)") %>% 
    dplyr::relocate(.data$variable, .after = "period")

  # Choose which scenario
  gdp <- if(corona) dplyr::select(gdp, -.data$base) else dplyr::select(gdp, -.data$corona)

  as.magpie(gdp, spatial = "region", temporal = "period", tidy = TRUE)
}
