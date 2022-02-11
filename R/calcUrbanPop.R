#' calcUrbanPop
#'
#' @inheritParams calcUrban
#' @inherit calcUrban return
#' @inheritSection calcGDP Return supplementary information
#' @inheritSection calcGDP Vectorization of arguments
#'
#' @seealso [madrat::calcOutput()]
#' @family Urban functions
#' @family Combined scenario functions
#'
#' @examples \dontrun{
#' library(mrdrivers)
#' calcOutput("UrbanPop")
#' }
#'
calcUrbanPop <- function(UrbanCalib = "past",                       # nolint
                         UrbanPast = "WDI",                         # nolint
                         UrbanFuture = c("SSPs", "SDPs", "SSP2EU"), # nolint
                         extension2150 = "constant",
                         FiveYearSteps = TRUE,                      # nolint
                         naming = "indicator_scenario") {
  # Check user input
  toolCheckUserInput("UrbanPop", as.list(environment()))
  # Call calcInternalUrbanPop function the appropriate number of times (map) and combine (reduce)
  # !! Keep formula syntax for madrat caching to work
  purrr::pmap(as.list(environment()),
              ~calcOutput("InternalUrbanPop", aggregate = FALSE, supplementary = TRUE, ...)) %>%
    toolReduce()
}

######################################################################################
# Internal Function
######################################################################################
calcInternalUrbanPop <- function(UrbanCalib,    # nolint
                                 UrbanPast,     # nolint
                                 UrbanFuture,   # nolint
                                 FiveYearSteps, # nolint
                                 extension2150,
                                 naming) {
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
