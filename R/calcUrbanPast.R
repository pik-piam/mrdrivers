#' @describeIn calcUrban Get historic urban population share data
#' @examples \dontrun{
#' library(mrdrivers)
#' calcOutput("UrbanPast")
#' }
#'
calcUrbanPast <- function(UrbanPast = "WDI") { # nolint

  data <- switch(
    UrbanPast,
    "WDI" = readSource("WDI", "SP.URB.TOTL.IN.ZS") / 100,
    stop("Bad input for UrbanPast. Invalid 'UrbanPast' argument.")
  )

  getNames(data) <- "urbanPop"
  data <- toolFinishingTouches(data)

  wp <- calcOutput("PopulationPast", PopulationPast = UrbanPast, aggregate = FALSE)

  list(x = data,
       weight = wp,
       unit = "share of population",
       description = glue("Urban population share data from {UrbanPast}"))
}
