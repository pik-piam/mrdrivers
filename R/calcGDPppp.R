#' calcGDPppp
#' 
#' Merges time series of GDP in Purchase Power Parity (PPP) of International
#' Dollars of the year 2005.  The source is selected in the config file. See
#' \code{\link{calcGDPpppPast}} for past datasets, and
#' \code{\link{calcGDPpppFuture}} for future datasets. The time series are
#' merged via the growth rates. The first year of the future scenarios
#' determines the merging point. All data is calibrated either to the "past" or
#' the "future" dataset as specified in "getConfig()$calc$PopulationCalib".
#' \itemize{ \item \code{WDI}: The PPP estimate from the World Developmnet
#' Indicators (WDI) are supplemented by values for Argentina, Syria and Somalia
#' which are missing in the database. The values were taken from World Bank.
#' 2014. Purchasing Power Parities and the Real Size of World Economies: A
#' Comprehensive Report of the 2011 International Comparison Program. The World
#' Bank. http://elibrary.worldbank.org/doi/book/10.1596/978-1-4648-0329-1.
#' table 2.13 Then, the 2011 estimate is extrapolated with the GDP projection
#' in local currency units (LCU), as these growth rates should be approximately
#' what the growth models predict. The price index from 2011 was transformed
#' into 2005 equivalents using the inflation rate of the United States(US), as
#' the PPPs are in USDollar equvialents. \item \code{PWP}: Penn World Tables }
#' 
#' @param GDPpppCalib to what should be calibrated? past, future or a transition?
#' @param GDPpppPast GDPppp past data source
#' @param GDPpppFuture GDPppp future data source
#' @param FiveYearSteps Only five year steps if TRUE, FALSE returns years from source data
#' @param naming naming scheme
#' 
#' @return GDP PPP(ICP11) in million USD05 equivalents
#' @author Lavinia Baumstark, Benjamin Bodirsky, Johannes Koch
#' @seealso \code{\link{calcGDPpppPast}}, \code{\link{calcGDPpppFuture}}

calcGDPppp <- function(GDPpppCalib = c("fixHist_IMFgr_return2SSP", "Ariadne"),
                       GDPpppPast = c("WDI_completed", "Eurostat_WDI_completed"), 
                       GDPpppFuture = c("SSP_bezierOut_completed", "SSP2Ariadne_completed_bezierOut"),
                       FiveYearSteps = TRUE,
                       naming = "indicator_scenario") {
  if (!(length(GDPpppCalib) == length(GDPpppPast) && length(GDPpppCalib) == length(GDPpppFuture))) {
     stop("Arguments GDPpppCalib, GDPpppPast and GDPpppFuture are not the same length.")
  }

  purrr::pmap(list(GDPpppCalib, GDPpppPast, GDPpppFuture, FiveYearSteps, naming),
              internal_calcGDPppp) %>% 
  purrr::reduce(~ list(x = mbind(.x$x, .y$x),
                       weight = NULL,
                       unit = "million",
                       description = glue::glue("{.x$description} \n {.y$description}")))

}


internal_calcGDPppp <- function(GDPpppCalib, 
                                GDPpppPast, 
                                GDPpppFuture, 
                                FiveYearSteps, 
                                naming){     
  
  past <- calcOutput("GDPpppPast", GDPpppPast = GDPpppPast, aggregate = FALSE)
  future <- calcOutput("GDPpppFuture", GDPpppFuture = GDPpppFuture, aggregate = FALSE)
  
  # The harmonization functions can be found in the file "helperFunctionsGDPandPopulation"
  combined <- switch(GDPpppCalib,
                     "past" = harmonizePast(past, future),
                     "future" = harmonizeFuture(past, future),
                     "transition" = harmonizeTransition(past, future, yEnd = 2020),
                     "past_transition" = harmonizePastTransition(past, future, yEnd = 2050),
                     "fixHist_IMFgr_return2SSP" = harmonizeFixHistIMFSSP(past, future, yEnd = 2100),
                     "Ariadne" = harmonizeAriadneGDP(past, future),
                     stop("Bad input for calcGDPppp. Invalid 'GDPpppCalib' argument."))

  datasettype <- switch(GDPpppCalib,
                        "past" = GDPpppPast,
                        "future" = GDPpppFuture,
                        "transition" = glue::glue("transition between {GDPpppPast} and {GDPpppFuture} \\
                                                   with a transition period until 2020"),
                        "past_transition" = glue::glue("use past data and afterwards transition between \\
                                                       {GDPpppPast} and {GDPpppFuture} with a transition \\
                                                       period until 2050"),
                        "fixHist_IMFgr_return2SSP" = glue::glue("use past data, short term growth rates from IMF and \\
                                                                afterwards transition between {GDPpppPast} and \\
                                                                {GDPpppFuture} with a transition period until 2100"),
                        "Ariadne" = glue::glue("use past data, short term growth rates from IMF and \\
                                                afterwards transition between {GDPpppPast} and \\
                                                {GDPpppFuture} with a transition period until 2100. \\
                                                For EUR/ARIADNE countries, just glue past with future and \\
                                                after 2070 converge to 2150 SSP2 values."))

  # The function "finishingTouches" can be found in the file "helperFunctionsGDPandPopulation"
  combined <- finishingTouches(combined, future, FiveYearSteps, naming)
       
  # Add SDP, SDP_EI, SDP_RC and SDP_MC scenarios as copy of SSP1
  if("gdp_SSP1" %in% getNames(combined) && !("gdp_SDP" %in% getNames(combined))){
    combined_SDP <- combined[,, "gdp_SSP1"]
    for  (i in c("SDP", "SDP_EI", "SDP_RC", "SDP_MC")) {
      getNames(combined_SDP) <- gsub("SSP1", i, getNames(combined[,, "gdp_SSP1"]))
      combined <- mbind(combined, combined_SDP) 
    }
  }  

  return(list(x = combined,
              weight = NULL,
              unit = "Million US Dollar in currency units of the calibration dataset",
              description = glue::glue("Million US Dollar in currency units of the calibration dataset. \\
                                        Datasource for the Past: {GDPpppPast}. Datasource for the Future: \\
                                        {GDPpppFuture}. Calibrated to {datasettype}.")))
}
