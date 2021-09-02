#' calcPopulation
#' 
#' Merges time series of population for the past and present. See
#' \code{\link{calcPopulationPast}} for past datasets, and
#' \code{\link{calcPopulationFuture}} for future datasets. The time series are
#' merged via the growth rates. The first year of the future scenarios
#' determines the merging point. All data is calibrated either to the "past" or
#' the "future" dataset as specified by PopulationCalib.
#' 
#' @param PopulationCalib to what should be calibrated? past, future or a transition?
#' @param PopulationPast population past data source
#' @param PopulationFuture population future data source
#' @inheritParams calcGDP
#' 
#' @return Population in millions.
#' @seealso \code{\link{calcPopulationPast}}, \code{\link{calcPopulationFuture}}
calcPopulation <- function(PopulationCalib  = c("past_grPEAP_grFuture", "past_grPEAP_grFuture", "Ariadne"),
                           PopulationPast   = c("WDI",                  "WDI",                  "Eurostat_WDI"), 
                           PopulationFuture = c("SSPs",                 "SDPs",                 "SSP2Ariadne"), 
                           useMIData = TRUE,
                           extension2150 = "bezier",
                           FiveYearSteps = TRUE, 
                           naming = "indicator_scenario") {
  # Check user input
  toolCheckUserInput("Population", as.list(environment()))
  # Call internal_calcPop function the appropriate number of times              
  toolInternalCalc("Population", as.list(environment()))
}

######################################################################################
# Internal Function
######################################################################################
internal_calcPopulation <- function(PopulationCalib, 
                                    PopulationPast, 
                                    PopulationFuture, 
                                    useMIData,
                                    extension2150,
                                    FiveYearSteps, 
                                    naming){

  # Compute "past" and "future" time series.
  past <- calcOutput("PopulationPast", 
                     PopulationPast = PopulationPast, 
                     useMIData = useMIData, 
                     aggregate = FALSE)
  future <- calcOutput("PopulationFuture", 
                       PopulationFuture = PopulationFuture, 
                       useMIData = useMIData, 
                       extension2150 = "none", 
                       aggregate = FALSE)

  # Combine "past" and "future" time series.
  combined <- switch(
    PopulationCalib,
    "past"                 = popHarmonizePast(past, future),
    "future"               = harmonizeFuture(past, future),
    "transition"           = harmonizeTransition(past, future, yEnd = 2020),
    "past_transition"      = harmonizePastTransition(past, future, yEnd = 2050),
    "past_grFuture"        = harmonizePastGrFuture(past, future),
    "past_grPEAP_grFuture" = harmonizePastGrPEAPGrFuture(past, future),
    "Ariadne"              = harmonizeAriadne(past, future),
    stop("Bad input for calcPopulation. Invalid 'PopulationCalib' argument.")
  )

  # Get description of harmonization function.
  datasettype <- switch(
    PopulationCalib,
    "past"                 = PopulationPast,
    "future"               = PopulationFuture,
    "transition"           = glue("transition between {PopulationPast} and {PopulationFuture} \\
                                        with a transition period until 2020"),
    "past_transition"      = glue("use past data and afterwards transition between \\
                                        {PopulationPast} and {PopulationFuture} with a transition \\
                                        period until 2050"),
    "past_grFuture"        = glue("use past data from {PopulationPast} and then the growth rates \\
                                        from {PopulationFuture}."),
    "past_grPEAP_grFuture" = glue("use past data from {PopulationPast}, then the growth rates \\
                                         from the Wolrld Bank's PEAP until 2025, and then the growth \\
                                         rates from {PopulationFuture}."),
    "Ariadne"              = glue("use past data from {PopulationPast}, then the growth rates \\
                                        from the Wolrld Bank's PEAP until 2025, and then the growth \\
                                        rates from {PopulationFuture}. For EUR/ARIADNE countries, \\
                                        just glue past with future.")
  )

  # Apply finishing touches to combined time-series
  combined <- finishingTouches(combined, extension2150, FiveYearSteps, naming)
  
  return(list(x = combined,
              weight = NULL,
              unit = "million",
              description = glue("Population data. Datasource for the Past: {PopulationPast}. \\
                                        Datasource for the Future: {PopulationFuture}. Calibrated \\
                                        to {datasettype}")))

}

######################################################################################
# Population Harmonization Functions
######################################################################################
popHarmonizePast <- function(past, future) {
  firstyear <- min(getYears(future, as.integer = TRUE))
  tmp <- future / setYears(future[,firstyear,], NULL)
  tmp[is.nan(tmp)] <- 1
  tmp <- dimSums(tmp * setYears(past[,firstyear,], NULL), dim = 3.2)
  if (firstyear>min(getYears(past,as.integer = TRUE))) {                                                 
    years_past <- getYears(past)[which(getYears(past,as.integer = TRUE)<firstyear)]
    tmp2       <- setNames(past[,years_past,rep(1,ndata(future))],getNames(future))
    combined   <- mbind(tmp2,tmp)
  } else {
    combined <- tmp
  }
}

harmonizePastGrPEAPGrFuture <- function(past, future) {
  # Get PEAP data and fill in missing islands
  short_term <- readSource("PEAP")
  fill <- readSource("MissingIslands", subtype = "pop", convert = FALSE)
  short_term <- completeData(short_term, fill)
  
  # Use PEAP growth rates until 2025
  tmp <- harmonizePastGrFuture(past, short_term[,getYears(short_term, as.integer = TRUE) <= 2025,])
  # Use future growth rates after that
  combined <- harmonizePastGrFuture(tmp, future)
  combined
}

harmonizeAriadne <- function(past, future) {
  combined <- harmonizePastGrPEAPGrFuture(past, future)

  # For SSP2-Ariadne: simply glue past (until 2019) with future (starting 2020)
  # Get EUR countries. 
  EUR_countries <- toolGetMapping("regionmappingH12.csv") %>% 
    tibble::as_tibble() %>% 
    dplyr::filter(.data$RegionCode == "EUR") %>% 
    dplyr::pull(.data$CountryCode)

  fut_years <- getYears(future)[getYears(future) >= 2020]
  combined[EUR_countries, fut_years,] <- future[EUR_countries, fut_years,]

  combined
}
