#' calcGDP
#'
#' Merges time series of GDP in Purchase Power Parity (PPP) of International
#' Dollars of the year 2005. See \code{\link{calcGDPPast}} for past datasets, and
#' \code{\link{calcGDPFuture}} for future datasets. The time series are
#' merged via the growth rates. The first year of the future scenarios
#' determines the merging point. All data is calibrated specified by GDPCalib.
#' The extension comes after the combination of past and future, in case the
#' calibration method affected the future extension
#'
#' @param GDPCalib to what should be calibrated? past, future or a transition?
#' @param GDPPast GDP past data source
#' @param GDPFuture GDP future data source
#' @param unit A string. Either 'constant 2005 Int$PPP', 'constant 2005 US$MER',
#'   'constant 2017 Int$PPP' or 'constant 2017 US$MER'.
#' @param extension2150 string, either "bezier", "constant" or "none"
#' @param FiveYearSteps `r lifecycle::badge("deprecated")` `FiveYearSteps = TRUE` is no
#'   longer supported; use the calcOutput argument `years`  instead, to retrieve
#'   specific years.
#' @param naming naming scheme
#'
#' @return A magpie object with sets "iso3c", "year" and "variable".
#' 
#' @seealso [madrat::calcOutput()]
#' @family GDP functions
#' @family Combined scenario functions
#'
#' @examples \dontrun{
#' library(mrdrivers)
#' calcOutput("GDP")}
#'
calcGDP <- function(GDPCalib  = c("calibSSPs", "calibSDPs", "calibSSP2EU"),
                    GDPPast   = c("WDI-MI",    "WDI-MI",    "Eurostat-WDI-MI"),
                    GDPFuture = c("SSPs-MI",   "SDPs-MI",   "SSP2EU-MI"),
                    unit = "constant 2005 Int$PPP",
                    extension2150 = "bezier",
                    FiveYearSteps = TRUE,
                    naming = "indicator_scenario") {
  # Check user input
  toolCheckUserInput("GDP", as.list(environment()))
  # Call internalCalcGDP function the appropriate number of times
  toolInternalCalc("GDP", as.list(environment()))
}

######################################################################################
# Internal Function
######################################################################################
internalCalcGDP <- function(GDPCalib,
                            GDPPast,
                            GDPFuture,
                            unit,
                            extension2150,
                            FiveYearSteps,
                            naming){

  # Depending on the chose GDPCalib, the harmonization function either requires 'past' and
  # 'future' GDP scenarios, OR NOT, which is the case for "calibSSPs" for example, where
  # the underlying computations are done on the GDPpc level, meaning that 'past' and 'future'
  # GDPpc (not GDP) are actually required. The harmonization on the GDP level, simply takes
  # the combined GDPpc scenario and multiplies it with the population sceanario.

  if (GDPCalib %in% c("calibSSPs", "calibSDPs")) {
    # Save arguments as list.
     args <- as.list(environment())
  } else {
    # Compute "past" and "future" GDP time series.
    past <- calcOutput("GDPPast",
                       GDPPast = GDPPast,
                       unit = unit,
                       aggregate = FALSE)
    future <- calcOutput("GDPFuture",
                         GDPFuture = GDPFuture,
                         unit = unit,
                         extension2150 = "none",
                         aggregate = FALSE)
  }

  # Combine "past" and "future" time series.
  combined <- switch(
    GDPCalib,
    "calibSSPs"       = gdpHarmonizeSSPsSPDs(args),
    "calibSDPs"       = gdpHarmonizeSSPsSPDs(args),
    "calibSSP2EU"     = gdpHarmonizeSSP2EU(past, future, unit),
    # Deprecated?
    "past"            = toolHarmonizePast(past, future),
    "future"          = toolHarmonizeFuture(past, future),
    "transition"      = toolHarmonizeTransition(past, future, yEnd = 2020),
    "past_transition" = toolHarmonizePastTransition(past, future, yEnd = 2050),    
    stop("Bad input for calcGDP. Invalid 'GDPCalib' argument.")
  )

  # Get description of harmonization function.
  description <- switch(
    GDPCalib,
    "past"            = GDPPast,
    "future"          = GDPFuture,
    "transition"      = glue("transition between {GDPPast} and {GDPFuture} with a transition period \\
                             until 2020"),
    "past_transition" = glue("use past data and afterwards transition between {GDPPast} and \\
                             {GDPFuture} with a transition period until 2050"),
    "calibSSPs"    = glue("use past data, short term growth rates from IMF and \\
                           afterwards transition between {GDPPast} and {GDPFuture} \\
                           with a transition period until 2100"),
    "calibSDPs"    = glue("use past data, short term growth rates from IMF and \\
                            afterwards transition between {GDPPast} and {GDPFuture} \\
                            with a transition period until 2100"),
    "calibSSP2EU"     = glue("use past data, short term growth rates from IMF and afterwards transition \\
                              between {GDPPast} and {GDPFuture} with a transition period until 2100. For \\
                              European countries, just glue past with future and after 2070 converge \\
                              to 2150 SSP2 values.")
  )

  # Apply finishing touches to combined time-series
  combined <- toolFinishingTouches(combined, extension2150, FiveYearSteps, naming)

  list(x = combined,
       weight = NULL,
       unit = unit,
       description = glue("Datasource for the Past: {GDPPast}. Datasource for the Future: \\
                                 {GDPFuture}. Calibrated to {description}."))
}


######################################################################################
# GDP Harmonization Functions
######################################################################################
gdpHarmonizeSSPsSPDs <- function(args) {
  gdppc <- calcOutput("GDPpc",
                      GDPpcCalib = args$GDPCalib,
                      GDPpcPast = args$GDPPast,
                      GDPpcFuture = args$GDPFuture,
                      unit = args$unit,
                      extension2150 = "none",
                      FiveYearSteps = FALSE,
                      aggregate = FALSE)

  pop <- calcOutput("Population",
                    PopulationCalib = args$GDPCalib,
                    PopulationPast = args$GDPPast,
                    PopulationFuture = args$GDPFuture,
                    extension2150 = "none",
                    FiveYearSteps = FALSE,
                    aggregate = FALSE)

  getNames(gdppc) <- getNames(pop) <- gsub("pop", "gdp", getNames(pop))
  gdp <- gdppc * pop
}



gdpHarmonizeSSP2EU <- function(past, future, unit) {
  # We explicitly use the bezier Extension for SSP2 here, but only for harmonization purposes.
  # We return only up until 2100.
  ssp2_data <- calcOutput("GDP",
                          GDPCalib = "calibSSPs",
                          GDPPast = "WDI-MI",
                          GDPFuture = "SSPs-MI",
                          unit = unit,
                          extension2150 = "bezier",
                          aggregate = FALSE,
                          FiveYearSteps = FALSE) %>%
  `[`(,, "gdp_SSP2")

  # For SSP2EU: simply glue past (until 2019) with future (starting 2020)
  # Get EUR countries.
  euCountries <- toolGetEUcountries(onlyWithARIADNEgdpData = TRUE)
  fut_years <- getYears(future)[getYears(future, as.integer = TRUE) >= max(getYears(past, as.integer = TRUE))]

  SSP2EU_data <- ssp2_data
  SSP2EU_data[euCountries, getYears(past),] <- past[euCountries,,]
  SSP2EU_data[euCountries, fut_years,] <- future[euCountries, fut_years,]

  # After 2070, transition to SSP2 values by 2150
  past_years <- getYears(future)[getYears(future, as.integer = TRUE) <= 2070]
  combined_SSP2EU <- toolHarmonizePastTransition(SSP2EU_data[euCountries, past_years,],
                                              ssp2_data[euCountries,,],
                                              2150)

  combined <- ssp2_data
  combined[euCountries, getYears(combined_SSP2EU),]  <- combined_SSP2EU[euCountries,,]
  getNames(combined) <- "gdp_SSP2EU"

  combined[, getYears(combined)[getYears(combined, as.integer = TRUE) <= 2100], ]
}
