#' calcGDP
#'
#' @description
#' Get GDP scenarios. By default the following scenarios are returned: 
#' \itemize{
#'   \item the SSPs, i.e. SSP1-5 and SSP2EU
#'   \item the SDPs, i.e. SDP, SDP_EI, SDP_RC, and SDP_MC
#' }
#'
#' The scenarios are created by harmonizing future projections onto historical data.
#'
#' @details # Return supplementary information
#'  Set the `supplementary` argument of [madrat::calcOutput()] to `TRUE` to return a list with the scenarios and
#'  additional information on the unit, the data sources and the harmonization function.
#'
#' @details # Vectorization of arguments
#'   The function accepts vectors for the harmonization function and past and future data sources.
#'   If given a vector, different combinations are created and returned all at once. If more than one
#'   argument is vectorised, the arguments have to have the same length. Which time series are created can be
#'   illustrated with the following example. Let's say the harmonization function and past data source are vectors of
#'   length 3. Then there will be in total 3 time series that are produced: the first time series is the result of
#'   combining the first harmonization function with the first past data source, the second time series the result of
#'   combining the second harmonization function with the second past data source, and the third time series the result
#'   of  using the respective third entry. The future data source used in each case is the same, since in this example
#'   only one future data source is provided.
#'
#' @param GDPCalib to what should be calibrated? past, future or a transition?
#' @param GDPPast GDP past data source
#' @param GDPFuture GDP future data source
#' @param unit A string. Either 'constant 2005 Int$PPP', 'constant 2005 US$MER',
#'   'constant 2017 Int$PPP' or 'constant 2017 US$MER'. In either case, the scenario
#'   construction is done in Int$PPP, with the conversion to US$MER coming afterward.
#' @param extension2150 string, either "bezier", "constant" or "none"
#' @param FiveYearSteps `r lifecycle::badge("deprecated")` `FiveYearSteps = TRUE` is no
#'   longer supported; use the calcOutput argument `years`  instead, to retrieve
#'   specific years.
#' @param average2020 TRUE or FALSE. If TRUE, then the 2020 value is replaced by the 2018-2022 average.
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
#' calcOutput("GDP")
#' # or, for the now-outdated GDP scenarios used before summer 2021,
#' calcOutput("GDP",
#'            GDPCalib = "past_transition",
#'            GDPPast = "IHME_USD05_PPP_pc-MI",
#'            GDPFuture = "SSPs-MI",
#'            extension2150 = "none",
#'            average2020 = FALSE,
#'            aggregate = FALSE,
#'            FiveYearSteps = FALSE)    
#' }
#'
calcGDP <- function(GDPCalib  = c("calibSSPs", "calibSDPs", "calibSSP2EU"),
                    GDPPast   = c("WDI-MI",    "WDI-MI",    "Eurostat-WDI-MI"),
                    GDPFuture = c("SSPs-MI",   "SDPs-MI",   "SSP2EU-MI"),
                    unit = "constant 2005 Int$PPP",
                    extension2150 = "bezier",
                    FiveYearSteps = TRUE,
                    average2020 = TRUE,
                    naming = "indicator_scenario") {
  # Check user input
  toolCheckUserInput("GDP", as.list(environment()))
  # Call calcInternalGDP function the appropriate number of times (map) and combine (reduce)
  # !! Keep formula syntax for madrat caching to work
  purrr::pmap(as.list(environment()), ~calcOutput("InternalGDP", aggregate = FALSE, supplementary = TRUE, ...)) %>%
    toolReduce()
}

######################################################################################
# Internal Function
######################################################################################
calcInternalGDP <- function(GDPCalib,
                            GDPPast,
                            GDPFuture,
                            unit,
                            extension2150,
                            FiveYearSteps,
                            average2020,
                            naming) {
  # GDP scenarios are constructed in PPPs. If MERs are desired, scenarios with the
  # same base year but in PPPs are constructed, and converted to MERs at the end.
  if (grepl("^constant .* US\\$MER$", unit)) {
    constructUnit <- paste0("constant ",  substr(unit, 10, 13), " Int$PPP")
  } else {
    constructUnit <- unit
  }

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
    past <- calcOutput("GDPPast", GDPPast = GDPPast, unit = constructUnit, aggregate = FALSE)
    future <- calcOutput("GDPFuture",
                         GDPFuture = GDPFuture,
                         unit = constructUnit,
                         extension2150 = "none",
                         aggregate = FALSE)
  }

  # Combine "past" and "future" time series.
  combined <- switch(
    GDPCalib,
    "calibSSPs"       = toolGDPHarmonizeSSPsSPDs(args),
    "calibSDPs"       = toolGDPHarmonizeSSPsSPDs(args),
    "calibSSP2EU"     = toolGDPHarmonizeSSP2EU(past, future, constructUnit),
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
    "calibSSPs"       = glue("use past data, short term growth rates from IMF and \\
                             afterwards transition between {GDPPast} and {GDPFuture} \\
                             with a transition period until 2100"),
    "calibSDPs"       = glue("use past data, short term growth rates from IMF and \\
                             afterwards transition between {GDPPast} and {GDPFuture} \\
                             with a transition period until 2100"),
    "calibSSP2EU"     = glue("use past data, short term growth rates from IMF and afterwards transition \\
                              between {GDPPast} and {GDPFuture} with a transition period until 2100. For \\
                              European countries, just glue past with future and after 2070 converge \\
                              to 2150 SSP2 values.")
  )

  # Apply finishing touches to combined time-series
  combined <- toolFinishingTouches(combined, extension2150, FiveYearSteps, naming, unit, constructUnit, average2020)

  list(x = combined,
       weight = NULL,
       unit = glue("mil. {unit}"),
       description = glue("Datasource for the Past: {GDPPast}. Datasource for the Future: \\
                           {GDPFuture}. Calibrated to {description}."))
}


######################################################################################
# GDP Harmonization Functions
######################################################################################
toolGDPHarmonizeSSPsSPDs <- function(args) {
  gdppc <- calcOutput("GDPpc",
                      GDPpcCalib = args$GDPCalib,
                      GDPpcPast = args$GDPPast,
                      GDPpcFuture = args$GDPFuture,
                      unit = args$constructUnit,
                      extension2150 = "none",
                      FiveYearSteps = FALSE,
                      average2020 = FALSE,
                      aggregate = FALSE)

  if (grepl("-MI$", args$GDPPast)) {
    h1 <- sub("-MI", "-UN_PopDiv-MI", args$GDPPast)
  } else {
    h1 <- args$GDPPast
  }
  if (grepl("-MI$", args$GDPFuture)) {
    h2 <- sub("-MI", "-UN_PopDiv-MI", args$GDPFuture)
  } else {
    h2 <- args$GDPFuture
  }
  pop <- calcOutput("Population",
                    PopulationCalib = args$GDPCalib,
                    PopulationPast = h1,
                    PopulationFuture = h2,
                    extension2150 = "none",
                    FiveYearSteps = FALSE,
                    aggregate = FALSE)

  getNames(gdppc) <- getNames(pop) <- gsub("pop", "gdp", getNames(pop))
  gdp <- gdppc * pop
}



toolGDPHarmonizeSSP2EU <- function(past, future, unit) {
  # We explicitly use the bezier Extension for SSP2 here, but only for harmonization purposes.
  # We return only up until 2100.
  ssp2_data <- calcOutput("GDP",
                          GDPCalib = "calibSSPs",
                          GDPPast = "WDI-MI",
                          GDPFuture = "SSPs-MI",
                          unit = unit,
                          extension2150 = "bezier",
                          FiveYearSteps = FALSE,
                          average2020 = FALSE,
                          aggregate = FALSE) %>%
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
