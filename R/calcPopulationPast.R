#' calcPopulationPast
#' 
#' Calculates a time series of Population. Different sources are available:
#' \itemize{ 
#'   \item \code{WDI}: Source: Worldbank. Taiwan was estimated as the
#'         difference between all countries and the global total.
#'   \item \code{UN_PopDiv}: UN Population Division data. Taiwan is estimated
#'         from "other, non-specified areas". Missing countries have their 
#'         values set to zero.
#' }
#' 
#' @inheritParams calcPopulation
#' @return Population in millions.
calcPopulationPast <- function(PopulationPast = "WDI", useMIData = TRUE) {

  data <- switch(
    PopulationPast,
    "WDI"          = readSource("WDI", "SP.POP.TOTL"),
    "UN_PopDiv"    = readSource("UN_PopDiv"),
    "Eurostat_WDI" = calcPopulationPastEurostatWDI(),
    stop("Bad input for PopulationPast. Invalid 'PopulationPast' argument.")
  )

  if (useMIData) {
    fill <- readSource("MissingIslands", subtype = "pop", convert = FALSE)
    data <- completeData(data, fill)
  }

  getNames(data) <- "population"
  data <- finishingTouches(data)

  return(list(x = data,
              weight = NULL,
              unit = "million",
              description = paste0("Population data from ", PopulationPast)))
}



######################################################################################
# Functions
######################################################################################
calcPopulationPastEurostatWDI <- function() {
  data_eurostat <- readSource("Eurostat", "population") / 1e+6
  data_wdi <- readSource("WDI", "SP.POP.TOTL")

  # Get EUR countries. 
  EUR_countries <- toolGetMapping("regionmappingH12.csv") %>% 
    tibble::as_tibble() %>% 
    dplyr::filter(.data$RegionCode == "EUR") %>% 
    dplyr::pull(.data$CountryCode)
  
  # Use WDI data for everything but the EUR_countries. Use Eurostat stat for those.
  data <- data_wdi
  data[EUR_countries,,] <- data_eurostat[EUR_countries,,]

  data
}
