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
    "SSPs" = cGDPpcFutureSSPs(useMIData, unit),
    "SDPs" = cGDPpcFutureSDPs(useMIData, unit), 
    stop("Bad input for calcGDPFuture. Invalid 'GDPFuture' argument.")
  )

  data <- finishingTouches(data, extension2150)

  weight <- calcOutput("PopulationFuture", 
                       PopulationFuture = GDPpcFuture,
                       useMIData = useMIData,
                       aggregate = FALSE)
  # Give weight same names as data, so that aggregate doesn't mess up data dim
  getNames(weight) <- gsub("pop", "gdppc", getNames(weight))

  data <- data[, getYears(weight), ]

  list(x = data, weight = weight, unit = unit, description = glue("GDPpc data from {GDPpcFuture}"))
}


######################################################################################
# Functions
######################################################################################
cGDPpcFutureSSPs <- function(useMIData, unit) {
  gdp <- calcOutput("GDPFuture", 
                    GDPFuture = "SSPs", 
                    useMIData = useMIData, 
                    unit = unit,
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

cGDPpcFutureSDPs <- function(useMIData, unit) {
  data_SSP1 <- cGDPpcFutureSSPs(useMIData, unit)[,, "gdppc_SSP1"]

  data <- purrr::map(c("SDP", "SDP_EI", "SDP_RC", "SDP_MC"),
                     ~ setNames(data_SSP1, gsub("SSP1", .x, getNames(data_SSP1)))) %>%
    mbind()
}
