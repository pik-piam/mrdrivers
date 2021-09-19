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
#' @param useMIData logical
#' @param extension2150 string, either "bezier", "constant" or "none"
#' @param FiveYearSteps Only five year steps if TRUE, FALSE returns years from source data
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
calcGDP <- function(GDPCalib  = c("past_IMF_SSP", "past_IMF_SDP", "Ariadne"),
                    GDPPast   = c("WDI",          "WDI",          "Eurostat_WDI"),
                    GDPFuture = c("SSPs",         "SDPs",         "SSP2Ariadne"),
                    unit = "constant 2005 Int$PPP",
                    useMIData = TRUE,
                    extension2150 = "bezier",
                    FiveYearSteps = TRUE,
                    naming = "indicator_scenario") {
  # Check user input
  toolCheckUserInput("GDP", as.list(environment()))
  # Call internal_calcGDP function the appropriate number of times
  toolInternalCalc("GDP", as.list(environment()))
}

######################################################################################
# Internal Function
######################################################################################
internal_calcGDP <- function(GDPCalib,
                             GDPPast,
                             GDPFuture,
                             unit,
                             useMIData,
                             extension2150,
                             FiveYearSteps,
                             naming){

  # Depending on the chose GDPCalib, the harmonization function either requires 'past' and
  # 'future' GDP scenarios, OR NOT, which is the case for "past_IMF_SSP" for example, where
  # the underlying computations are done on the GDPpc level, meaning that 'past' and 'future'
  # GDPpc (not GDP) are actually required. The harmonization on the GDP level, simply takes
  # the combined GDPpc scenario and multiplies it with the population sceanario.

  if (GDPCalib %in% c("past_IMF_SSP", "past_IMF_SDP")) {
    # Save arguments as list.
     args <- as.list(environment())
  } else {
    # Compute "past" and "future" GDP time series.
    past <- calcOutput("GDPPast",
                       GDPPast = GDPPast,
                       unit = unit,
                       useMIData = useMIData,
                       aggregate = FALSE)
    future <- calcOutput("GDPFuture",
                         GDPFuture = GDPFuture,
                         unit = unit,
                         useMIData = useMIData,
                         extension2150 = "none",
                         aggregate = FALSE)
  }

  # Combine "past" and "future" time series.
  combined <- switch(
    GDPCalib,
    "past"            = harmonizePast(past, future),
    "future"          = harmonizeFuture(past, future),
    "transition"      = harmonizeTransition(past, future, yEnd = 2020),
    "past_transition" = harmonizePastTransition(past, future, yEnd = 2050),
    "past_IMF_SSP"    = harmonizePastIMFSSPSDP(args),
    "past_IMF_SDP"    = harmonizePastIMFSSPSDP(args),
    "Ariadne"         = harmonizeAriadneGDP(past, future),
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
    "past_IMF_SSP"    = glue("use past data, short term growth rates from IMF and \\
                              afterwards transition between {GDPPast} and {GDPFuture} \\
                              with a transition period until 2100"),
    "past_IMF_SDP"    = glue("use past data, short term growth rates from IMF and \\
                              afterwards transition between {GDPPast} and {GDPFuture} \\
                              with a transition period until 2100"),
    "Ariadne"         = glue("use past data, short term growth rates from IMF and afterwards transition \\
                              between {GDPPast} and {GDPFuture} with a transition period until 2100. For \\
                              EUR/ARIADNE countries, just glue past with future and after 2070 converge \\
                              to 2150 SSP2 values.")
  )

  # Apply finishing touches to combined time-series
  combined <- finishingTouches(combined, extension2150, FiveYearSteps, naming)

  list(x = combined,
       weight = NULL,
       unit = unit,
       description = glue("Datasource for the Past: {GDPPast}. Datasource for the Future: \\
                                 {GDPFuture}. Calibrated to {description}."))
}


######################################################################################
# GDP Harmonization Functions
######################################################################################
harmonizePastIMFSSPSDP <- function(args) {
  gdppc <- calcOutput("GDPpc",
                      GDPpcCalib = args$GDPCalib,
                      GDPpcPast = args$GDPPast,
                      GDPpcFuture = args$GDPFuture,
                      unit = args$unit,
                      useMIData = args$useMIData,
                      extension2150 = "none",
                      FiveYearSteps = FALSE,
                      aggregate = FALSE)

  pop <- calcOutput("Population",
                    PopulationCalib = "past_grPEAP_grFuture",
                    PopulationPast = args$GDPPast,
                    PopulationFuture = args$GDPFuture,
                    extension2150 = "none",
                    FiveYearSteps = FALSE,
                    aggregate = FALSE)

  getNames(gdppc) <- getNames(pop) <- gsub("pop", "gdp", getNames(pop))
  gdp <- gdppc * pop
}



harmonizeAriadneGDP <- function(past, future) {
  # We explicitly use the bezier Extension for SSP2 here, but only for harmonization purposes.
  # We return only up until 2100.
  ssp2_data <- calcOutput("GDP",
                          GDPCalib = "past_IMF_SSP",
                          GDPPast = "WDI",
                          GDPFuture = "SSPs",
                          extension2150 = "bezier",
                          aggregate = FALSE,
                          FiveYearSteps = FALSE) %>%
  `[`(,, "gdp_SSP2")

  # For SSP2-Ariadne: simply glue past (until 2019) with future (starting 2020)
  # Get EUR countries.
  EUR_countries <- where(readSource("ARIADNE", "gdp_corona") != 0 )$true$regions
  fut_years <- getYears(future)[getYears(future, as.integer = TRUE) >= max(getYears(past, as.integer = TRUE))]

  ssp2Ariadne_data <- ssp2_data
  ssp2Ariadne_data[EUR_countries, getYears(past),] <- past[EUR_countries,,]
  ssp2Ariadne_data[EUR_countries, fut_years,] <- future[EUR_countries, fut_years,]

  # After 2070, transition to SSP2 values by 2150
  past_years <- getYears(future)[getYears(future, as.integer = TRUE) <= 2070]
  combined_Ariadne <- harmonizePastTransition(ssp2Ariadne_data[EUR_countries, past_years,],
                                              ssp2_data[EUR_countries,,],
                                              2150)

  combined <- ssp2_data
  combined[EUR_countries, getYears(combined_Ariadne),]  <- combined_Ariadne[EUR_countries,,]
  getNames(combined) <- "gdp_SSP2Ariadne"

  combined[, getYears(combined)[getYears(combined, as.integer = TRUE) <= 2100], ]
}
