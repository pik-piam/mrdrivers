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
    "SSPs" = cGDPpcFutureSSPs(useMIData),
    "SDPs" = cGDPpcFutureSDPs(useMIData), 
    stop("Bad input for calcGDPFuture. Invalid 'GDPFuture' argument.")
  )

  data <- finishingTouches(data, extension2150)

  list(x = data, weight = NULL, unit = unit, description = glue("GDPpc data from {GDPpcFuture}"))
}


######################################################################################
# Functions
######################################################################################
cGDPpcFutureSSPs <- function(useMIData) {
  gdp <- calcOutput("GDPFuture", 
                    GDPFuture = "SSPs", 
                    useMIData = useMIData, 
                    extension2150 = "none", 
                    aggregate = FALSE)
  gdp <- setNames(gdp, c("gdppc_SSP1", "gdppc_SSP2", "gdppc_SSP3", "gdppc_SSP4", "gdppc_SSP5"))

  pop <- calcOutput("PopulationFuture", 
                    PopulationFuture = "SSPs_old",
                    useMIData = useMIData, 
                    extension2150 = "none", 
                    aggregate = FALSE)
  pop <- setNames(pop, c("gdppc_SSP1", "gdppc_SSP2", "gdppc_SSP3", "gdppc_SSP4", "gdppc_SSP5"))

  years <- intersect(getYears(gdp), getYears(pop))  
  data <- gdp[, years,] / pop[, years,]
  data[is.nan(data) | data == Inf] <- 0
  data 
}

cGDPpcFutureSDPs <- function(useMIData) {
  data_SSP1 <- cGDPpcFutureSSPs(useMIData)[,, "gdppc_SSP1"]

  data <- purrr::map(c("SDP", "SDP_EI", "SDP_RC", "SDP_MC"),
                     ~ setNames(data_SSP1, gsub("SSP1", .x, getNames(data_SSP1)))) %>%
    mbind()
}
