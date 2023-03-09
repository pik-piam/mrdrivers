#' @describeIn calcPopulationPast Get historic working-age population data
#'
#' @param LabourPast A string designating the source for the historical working-age population data.
#'   Available sources are:
#'   \itemize{
#'     \item "WDI": World development indicators from the World Bank
#'   }
#'   See the "Combining data sources with '-'" section below for how to combine data sources.
#'
#' @examples \dontrun{
#' library(mrdrivers)
#' calcOutput("LabourPast")
#' }
#'
calcLabourPast <- function(LabourPast = "WDI") { # nolint
  # Check user input
  toolCheckUserInput("LabourPast", as.list(environment()))
  # Call calcInternalPopulationFuture function the appropriate number of times (map) and combine (reduce)
  # !! Keep formula syntax for madrat caching to work
  purrr::pmap(list("LabourPast" = unlist(strsplit(LabourPast, "-"))),
              ~calcOutput("InternalLabourPast", aggregate = FALSE, supplementary = TRUE, ...)) %>%
    toolReduce(mbindOrFillWith = "fillWith")
}

calcInternalLabourPast <- function(LabourPast) { # nolint
  x <- switch(
    LabourPast,
    "WDI" = readSource("WDI", "SP.POP.1564.TO"),
    stop("Bad input for calcLabour. Invalid 'LabourPast' argument.")
  )

  # Apply finishing touches to combined time-series
  x <- toolFinishingTouches(x)

  # Hopefully temporary: rename lab scnearios pop. Necessary for REMIND to work.
  getNames(x) <- sub("lab_", "pop_", getNames(x))

  list(x = x,
       weight = NULL,
       unit = "million",
       description = glue("Working age population data."))
}