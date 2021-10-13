#' calcUrbanPop
#' 
#' @inheritParams calcUrban
#' @inherit calcUrban return
#' 
#' @seealso [madrat::calcOutput()]
#' @family Urban functions
#' @family Combined scenario functions
#' 
#' @examples \dontrun{
#' library(mrdrivers)
#' calcOutput("UrbanPop")}
#' 
calcUrbanPop <- function(UrbanCalib = "past", 
                         UrbanPast = "WDI", 
                         UrbanFuture = c("SSPs", "SDPs", "SSP2EU"),
                         extension2150 = "constant",
                         FiveYearSteps = TRUE,
                         naming = "indicator_scenario") {
  # Check user input
  toolCheckUserInput("UrbanPop", as.list(environment()))
  # Call internal_calcUrban function the appropriate number of times              
  toolInternalCalc("UrbanPop", as.list(environment()))
}

######################################################################################
# Internal Function
######################################################################################
internal_calcUrbanPop <- function(UrbanCalib, 
                                  UrbanPast, 
                                  UrbanFuture, 
                                  FiveYearSteps, 
                                  extension2150, 
                                  naming){
  # Get urban shares
  us <- calcOutput("Urban",
                   UrbanCalib = UrbanCalib, 
                   UrbanPast = UrbanPast, 
                   UrbanFuture = UrbanFuture, 
                   FiveYearSteps = FiveYearSteps, 
                   extension2150 = extension2150, 
                   naming = naming,
                   aggregate = FALSE,
                   supplementary = TRUE)

  # Multiply
  getNames(us$x) <- getNames(us$weight) <- gsub("pop", "urbanpop", getNames(us$weight))
  x <- us$x * us$weight

  list(x = x, weight = NULL, unit = "million", description = us$description)
}
