#' calcPopulationPast
#' 
#' Calculates a time series of Population. Different sources are available and
#' can be selected in the madrat config(getConfig()$calc$PopulationPast):
#' \itemize{ 
#'   \item \code{WDI}: Source: Worldbank. Taiwan was estimated as the
#'         difference between all countries and the global total.
#'   \item \code{UN_PopDiv}: UN Population Division data. Taiwan is estimated
#'         from "other, non-specified areas". Missing countries have their 
#'         values set to zero.
#' }
#' 
#' @param PopulationPast population past data source
#' @return Population in millions.
#' @author Lavinia Baumstark, Benjamin Bodirsky, Johannes Koch

calcPopulationPast <- function(PopulationPast = "WDI_completed") {
  
  # Check input argument  
  valid_inputs <- c(
    "WDI", 
    "WDI_completed",
    "UN_PopDiv",
    "Eurostat_WDI",
    "Eurostat_WDI_completed"
  )
  if (!PopulationPast %in% valid_inputs) {
    stop("Bad input for PopulationPast. Invalid 'PopulationPast' argument.")
  }

  # Look for "_completed" tag. Remove if found.
  if (grepl("_completed$", PopulationPast)) {
    PopulationPast <- gsub("_completed", "", PopulationPast)
    complete <- TRUE
  } else {
    complete <- FALSE
  }

  data <- switch(PopulationPast,
                 "WDI" = calcPopulationPastWDI(complete),
                 "UN_PopDiv" = readSource("UN_PopDiv"),
                 "Eurostat_WDI" = calcPopulationPastEurostatWDI(complete))

  getNames(data) <- "population"
  data <- clean_magpie(data)

  return(list(x = data,
              weight = NULL,
              unit = "million",
              description = paste0("Population data based on ", PopulationPast)))
}



######################################################################################
# Functions
######################################################################################
calcPopulationPastWDI <- function(complete) {
  data <- readSource("WDI", "SP.POP.TOTL")
  
  if (complete) {
    fill <- readSource("MissingIslands", subtype = "pop", convert = FALSE)
    data <- completeData(data, fill)
  }

  data
}

calcPopulationPastEurostatWDI <- function(complete) {
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

  if (complete) {
    fill <- readSource("MissingIslands", subtype = "pop", convert = FALSE)
    data <- completeData(data, fill)
  }

  data
}
