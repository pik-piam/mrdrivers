#' calcGDPpcFuture
#' 
#' @inheritParams calcGDPpc
#' @inherit calcGDPpc return
#' 
#' @seealso [madrat::calcOutput]
#' @family GDPpc functions
#'
#' @examples \dontrun{
#' library(mrdrivers)
#' calcOutput("GDPpcFuture")}
#'
calcGDPpcFuture <- function(GDPpcFuture = "SSPs",
                            unit = "constant 2005 Int$PPP", 
                            useMIData = TRUE,
                            extension2150 = "none") {

  data <- switch(
    GDPpcFuture,
    "SSPs" = cGDPpcFutureSSPs(),
    "SDPs" = cGDPpcFutureSDPs(), 
    stop("Bad input for calcGDPFuture. Invalid 'GDPFuture' argument.")
  )

  # Fill in data with Missing Islands dataset
  if (useMIData) {  
    gdp <- readSource("MissingIslands", subtype = "gdp", convert = FALSE)
    pop <- readSource("MissingIslands", subtype = "pop", convert = FALSE)
    countries <- intersect(getRegions(gdp), getRegions(pop))
    fill <- gdp[countries,,] / pop[countries,,]
    data <- completeData(data, fill)
  }

  data <- finishingTouches(data, extension2150)

  list(x = data, weight = NULL, unit = unit, description = glue("GDPpc data from {GDPpcFuture}"))
}


######################################################################################
# Functions
######################################################################################
cGDPpcFutureSSPs <- function() {
  gdp <- calcOutput("GDPFuture", 
                    GDPFuture = "SSPs", 
                    useMIData = FALSE, 
                    extension2150 = "none", 
                    aggregate = FALSE)
  gdp <- setNames(gdp, c("gdppc_SSP1", "gdppc_SSP2", "gdppc_SSP3", "gdppc_SSP4", "gdppc_SSP5"))

  pop <- calcOutput("PopulationFuture", 
                    PopulationFuture = "SSPs",
                    useMIData = FALSE, 
                    extension2150 = "none", 
                    aggregate = FALSE)
  pop <- setNames(pop, c("gdppc_SSP1", "gdppc_SSP2", "gdppc_SSP3", "gdppc_SSP4", "gdppc_SSP5"))

  years <- intersect(getYears(gdp), getYears(pop))  
  data <- gdp[, years,] / pop[, years,]
  data[is.nan(data) | data == Inf] <- 0
  data 
}

cGDPpcFutureSDPs <- function() {
  data_SSP1 <- cGDPpcFutureSSPs()[,, "gdppc_SSP1"]

  data <- purrr::map(c("SDP", "SDP_EI", "SDP_RC", "SDP_MC"),
                     ~ setNames(data_SSP1, gsub("SSP1", .x, getNames(data_SSP1)))) %>%
    mbind()
}
