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
#' 
#' @inheritParams calcGDP
#' @inherit calcGDP return
#' 
#' @seealso [madrat::calcOutput()]
#' @family Population functions
#' @family Combined scenario functions
#' 
#' @examples \dontrun{
#' library(mrdrivers)
#' calcOutput("GDPpc")}
#' 
calcPopulation <- function(PopulationCalib  = c("calibSSPs", "calibSDPs", "calibSSP2EU"),
                           PopulationPast   = c("WDI-MI",    "WDI-MI",    "Eurostat-WDI-MI"), 
                           PopulationFuture = c("SSPs-MI",   "SDPs-MI",   "SSP2EU-MI"), 
                           extension2150 = "bezier",
                           FiveYearSteps = TRUE, 
                           naming = "indicator_scenario") {
  # Check user input
  toolCheckUserInput("Population", as.list(environment()))
  # Call internal_calcPopulation function the appropriate number of times              
  toolInternalCalc("Population", as.list(environment()))
}

######################################################################################
# Internal Function
######################################################################################
internal_calcPopulation <- function(PopulationCalib, 
                                    PopulationPast, 
                                    PopulationFuture, 
                                    extension2150,
                                    FiveYearSteps, 
                                    naming){

  # Compute "past" and "future" time series.
  past <- calcOutput("PopulationPast", 
                     PopulationPast = PopulationPast, 
                     aggregate = FALSE)
  future <- calcOutput("PopulationFuture", 
                       PopulationFuture = PopulationFuture, 
                       extension2150 = "none", 
                       aggregate = FALSE)

  # Combine "past" and "future" time series.
  combined <- switch(
    PopulationCalib,
    "calibSSPs"       = harmonizeSSPsSDPs(past, future),
    "calibSDPs"       = harmonizeSSPsSDPs(past, future),
    "calibSSP2EU"     = harmonizeSSP2EU(past, future),
    "calibUN_PopDiv"  = harmonizeUN_PopDiv(past, future),
    # Deprecated?
    "past"            = popHarmonizePast(past, future),
    "future"          = toolHarmonizeFuture(past, future),
    "transition"      = toolHarmonizeTransition(past, future, yEnd = 2020),
    "past_transition" = toolHarmonizePastTransition(past, future, yEnd = 2050),
    "past_grFuture"   = toolHarmonizePastGrFuture(past, future),
    stop("Bad input for calcPopulation. Invalid 'PopulationCalib' argument.")
  )

  # Get description of harmonization function.
  datasettype <- switch(
    PopulationCalib,
    "calibSSPs"       = glue("use past data from {PopulationPast}, then the growth rates \\
                             from the Wolrld Bank's PEAP until 2025, and then the growth \\
                             rates from {PopulationFuture}."),
    "calibSDPs"       = glue("use past data from {PopulationPast}, then the growth rates \\
                             from the Wolrld Bank's PEAP until 2025, and then the growth \\
                             rates from {PopulationFuture}."),
    "calibSSP2EU"     = glue("use past data from {PopulationPast}, then the growth rates \\
                             from the Wolrld Bank's PEAP until 2025, and then the growth \\
                             rates from {PopulationFuture}. For European countries, \\
                             just glue past with future."),
    "calibUN_PopDiv"            = glue("use past data from {PopulationPast} and then future data from \\
                              {PopulationFuture}."),
    "past"            = PopulationPast,
    "future"          = PopulationFuture,
    "transition"      = glue("transition between {PopulationPast} and {PopulationFuture} \\
                             with a transition period until 2020"),
    "past_transition" = glue("use past data and afterwards transition between \\
                             {PopulationPast} and {PopulationFuture} with a transition \\
                             period until 2050"),
    "past_grFuture"   = glue("use past data from {PopulationPast} and then the growth rates \\
                             from {PopulationFuture}."),
  )

  # Apply finishing touches to combined time-series
  combined <- toolFinishingTouches(combined, extension2150, FiveYearSteps, naming)
  
  list(x = combined,
       weight = NULL,
       unit = "million",
       description = glue("Population data. Datasource for the Past: {PopulationPast}. \\
                          Datasource for the Future: {PopulationFuture}. Calibrated \\
                          to {datasettype}"))

}

######################################################################################
# Population Harmonization Functions
######################################################################################
harmonizeSSPsSDPs <- function(past, future) {
  # Get PEAP data and fill in missing islands
  short_term <- readSource("PEAP")
  fill <- readSource("MissingIslands", subtype = "pop", convert = FALSE)
  short_term <- short_term %>% 
    toolFillWith(fill) %>%
    toolInterpolateAndExtrapolate()
  
  # Use PEAP growth rates until 2025
  tmp <- toolHarmonizePastGrFuture(past, short_term[,getYears(short_term, as.integer = TRUE) <= 2025,])
  # Use future growth rates after that
  combined <- toolHarmonizePastGrFuture(tmp, future)
  combined
}

harmonizeSSP2EU <- function(past, future) {
  combined <- harmonizeSSPsSDPs(past, future)

  # For SSP2EU: simply glue past (until 2019) with future (starting 2020)
  # Get EUR countries. 
  EUR_countries <- toolGetEURcountries()

  fut_years <- getYears(future)[getYears(future, as.integer = TRUE) >= 2020]
  combined[EUR_countries, fut_years,] <- future[EUR_countries, fut_years,]

  combined
}

harmonizeUN_PopDiv <- function(past, future) {
  # Glue future to past
  year <- max(intersect(getYears(past, as.integer = TRUE),
                        getYears(future, as.integer = TRUE)))
  fut_years <- getYears(future)[getYears(future, as.integer = TRUE) > year]
  getNames(past) <- getNames(future)
  mbind(past, future[, fut_years, ])
}

# Legacy?
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
  combined[combined == Inf] <- 0
  combined
}
