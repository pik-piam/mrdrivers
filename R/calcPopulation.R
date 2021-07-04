#' calcPopulation
#' 
#' Merges time series of population for the past and present. Different sources
#' are available and can be selected in the madrat config. See
#' \code{\link{calcPopulationPast}} for past datasets, and
#' \code{\link{calcPopulationFuture}} for future datasets. The time series are
#' merged via the growth rates. The first year of the future scenarios
#' determines the merging point. All data is calibrated either to the "past" or
#' the "future" dataset as specified in "getConfig()$calc$PopulationCalib".
#' 
#' @param PopulationCalib to what should be calibrated? past, future or a transition?
#' @param PopulationPast population past data source
#' @param PopulationFuture population future data source
#' @param FiveYearSteps Only five year steps if TRUE, FALSE returns years from source data
#' @param naming naming scheme
#' 
#' @return Population in millions.
#' @author Lavinia Baumstark, Benjamin Bodirsky, Johannes Koch
#' @seealso \code{\link{calcPopulationPast}},\code{\link{calcPopulationFuture}}

calcPopulation <- function(PopulationCalib = c("past_grPEAP_grFuture", "Ariadne"),
                           PopulationPast = c("WDI_completed", "Eurostat_WDI_completed"), 
                           PopulationFuture = c("SSP2018Update_completed_bezierOut", "SSP2Ariadne_completed_bezierOut"), 
                           FiveYearSteps = TRUE, 
                           naming = "indicator_scenario") {
  if (!(length(PopulationCalib) == length(PopulationPast) && length(PopulationCalib) == length(PopulationFuture))) {
     stop("Arguments PopulationCalib, PopulationPast and PopulationFuture are not the same length.")
  }
  
  purrr::pmap(list(PopulationCalib, PopulationPast, PopulationFuture, FiveYearSteps, naming),
              internal_calcPopulation) %>% 
  purrr::reduce(~ list(x = mbind(.x$x, .y$x),
                       weight = NULL,
                       unit = "million",
                       description = glue::glue("{.x$description} \n {.y$description}")))
}


internal_calcPopulation <- function(PopulationCalib, 
                                    PopulationPast, 
                                    PopulationFuture, 
                                    FiveYearSteps, 
                                    naming){

  past <- calcOutput("PopulationPast", PopulationPast = PopulationPast, aggregate = FALSE)
  future <- calcOutput("PopulationFuture", PopulationFuture = PopulationFuture, aggregate = FALSE)

  # The harmonization functions can be found in the file "helperFunctionsGDPandPopulation"
  combined <- switch(PopulationCalib,
                     "past" = popHarmonizePast(past, future),
                     "future" = harmonizeFuture(past, future),
                     "transition" = harmonizeTransition(past, future, yEnd = 2020),
                     "past_transition" = harmonizePastTransition(past, future, yEnd = 2050),
                     "past_grFuture" = harmonizePastGrFuture(past, future),
                     "past_grPEAP_grFuture" =  harmonizePastGrPEAPGrFuture(past, future),
                     "Ariadne" =  harmonizeAriadne(past, future),
                     stop("Bad input for calcPopulation. Invalid 'PopulationCalib' argument."))

  datasettype <- switch(PopulationCalib,
                        "past" = PopulationPast,
                        "future" = PopulationFuture,
                        "transition" = glue::glue("transition between {PopulationPast} and {PopulationFuture} \\
                                                   with a transition period until 2020"),
                        "past_transition" = glue::glue("use past data and afterwards transition between \\
                                                       {PopulationPast} and {PopulationFuture} with a transition \\
                                                       period until 2050"),
                        "past_grFuture" = glue::glue("use past data from {PopulationPast} and then the growth rates \\
                                                      from {PopulationFuture}."),
                        "past_grPEAP_grFuture" = glue::glue("use past data from {PopulationPast}, then the growth rates \\
                                                             from the Wolrld Bank's PEAP until 2025, and then the growth \\
                                                             rates from {PopulationFuture}."),
                        "Ariadne" = glue::glue("use past data from {PopulationPast}, then the growth rates \\
                                                from the Wolrld Bank's PEAP until 2025, and then the growth \\
                                                rates from {PopulationFuture}. For EUR/ARIADNE countries, \\
                                                just glue past with future."))

  # The function "finishingTouches" can be found in the file "helperFunctionsGDPandPopulation"
  combined <- finishingTouches(combined, future, FiveYearSteps, naming)

  # Add SDP, SDP_EI, SDP_RC and SDP_MC scenarios as copy of SSP1
  if("pop_SSP1" %in% getNames(combined) && !("pop_SDP" %in% getNames(combined))){
    combined_SDP <- combined[,, "pop_SSP1"]
    for  (i in c("SDP", "SDP_EI", "SDP_RC", "SDP_MC")) {
      getNames(combined_SDP) <- gsub("SSP1", i, getNames(combined[,, "pop_SSP1"]))
      combined <- mbind(combined, combined_SDP) 
    }
  }  
  
  return(list(x = combined,
              weight = NULL,
              unit = "million",
              description = glue::glue("Population data. Datasource for the Past: {PopulationPast}.\\
                                        Datasource for the Future: {PopulationFuture}. Calibrated \\
                                        to {datasettype}")))

}
