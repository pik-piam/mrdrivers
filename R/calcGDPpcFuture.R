#' calcGDPpcFuture
#'
#' @inheritParams calcGDPpc
#' @inherit calcGDPpc return
#'
#' @seealso [madrat::calcOutput()]
#' @family GDPpc functions
#'
#' @examples \dontrun{
#' library(mrdrivers)
#' calcOutput("GDPpcFuture")
#' }
#'
calcGDPpcFuture <- function(GDPpcFuture = "SSPs-MI", # nolint
                            unit = "constant 2005 Int$PPP",
                            extension2150 = "none") {

  data <- switch(
    GDPpcFuture,
    "SSPs"    = toolGDPpcFutureSSPs(unit),
    "SDPs"    = toolGDPpcFutureSDPs(unit),
    "SDPs-MI" = toolGDPpcFutureSDPs(unit, mi = TRUE),
    "SSPs-MI" = toolGDPpcFutureSSPs(unit, mi = TRUE),
    "MI"      = toolGDPpcMI(unit),
    stop("Bad input for calcGDPFuture. Invalid 'GDPFuture' argument.")
  )

  data <- toolFinishingTouches(data, extension2150)

  weight <- calcOutput("PopulationFuture",
                       PopulationFuture = GDPpcFuture,
                       aggregate = FALSE)
  # Give weight same names as data, so that aggregate doesn't mess up data dim
  getNames(weight) <- gsub("pop", "gdppc", getNames(weight))

  data <- data[, getYears(weight), ]

  list(x = data, weight = weight, unit = unit, description = glue("GDPpc data from {GDPpcFuture}"))
}


######################################################################################
# Functions
######################################################################################
toolGDPpcFutureSSPs <- function(unit, mi = FALSE) {
  h1 <- if (mi) "SSPs-MI" else "SSPs"
  h2 <- if (mi) "SSPs_old-MI" else "SSPs_old"

  gdp <- calcOutput("GDPFuture",
                    GDPFuture = h1,
                    unit = unit,
                    extension2150 = "none",
                    aggregate = FALSE)
  gdp <- setNames(gdp, c("gdppc_SSP1", "gdppc_SSP2", "gdppc_SSP3", "gdppc_SSP4", "gdppc_SSP5"))

  pop <- calcOutput("PopulationFuture",
                    PopulationFuture = h2,
                    extension2150 = "none",
                    aggregate = FALSE)
  pop <- setNames(pop, c("gdppc_SSP1", "gdppc_SSP2", "gdppc_SSP3", "gdppc_SSP4", "gdppc_SSP5"))

  years <- intersect(getYears(gdp), getYears(pop))
  data <- gdp[, years, ] / pop[, years, ]
  data[is.nan(data) | data == Inf] <- 0
  data
}

toolGDPpcFutureSDPs <- function(unit, mi = FALSE) {
  data_SSP1 <- toolGDPpcFutureSSPs(unit, mi)[, , "gdppc_SSP1"] # nolint

  purrr::map(c("SDP", "SDP_EI", "SDP_RC", "SDP_MC"),
             ~ setNames(data_SSP1, gsub("SSP1", .x, getNames(data_SSP1)))) %>%
    mbind()
}

toolGDPpcMI <- function(unit) {
  gdp <- calcOutput("GDPFuture", GDPFuture = "MI", unit = unit, aggregate = FALSE)
  pop <- calcOutput("PopulationFuture", PopulationFuture = "MI", aggregate = FALSE)
  years <- intersect(getYears(gdp), getYears(pop))

  data <- gdp[, years, ] / pop[, years, ]
  data <- setNames(data, "gdppc_MI")
  data[is.nan(data) | data == Inf] <- 0
  data
}
