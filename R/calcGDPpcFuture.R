#' @describeIn calcGDPPast Get future GDPpc projections
#'
#' @param GDPpcFuture A string designating the source for the future GDP data. Available sources are:
#'   \itemize{
#'     \item "SSPs":
#'     \item "SDPs": All SDP future projections are set equal to those of SSP1.
#'     \item "MI": Missing island dataset
#'   }
#'   See the "Combining data sources with '-'" section below for how to combine data sources.
calcGDPpcFuture <- function(GDPpcFuture = "SSPsOld-MI", # nolint
                            unit = "constant 2005 Int$PPP") {

  data <- switch(
    GDPpcFuture,
    "SSPsOld"    = toolGDPpcFutureSSPsOld(unit),
    "SDPs"       = toolGDPpcFutureSDPs(unit),
    "SDPs-MI"    = toolGDPpcFutureSDPs(unit, mi = TRUE),
    "SSPsOld-MI" = toolGDPpcFutureSSPsOld(unit, mi = TRUE),
    "MI"         = toolGDPpcMI(unit),
    stop("Bad input for calcGDPFuture. Invalid 'GDPFuture' argument.")
  )

  data <- toolFinishingTouches(data)

  weight <- calcOutput("PopulationFuture", PopulationFuture = GDPpcFuture, aggregate = FALSE)
  # Give weight same names as data, so that aggregate doesn't mess up data dim
  getNames(weight) <- gsub("pop", "gdppc", getNames(weight))

  data <- data[, getYears(weight), ]

  list(x = data, weight = weight, unit = unit, description = glue("{GDPpcFuture} projections"))
}

toolGDPpcFutureSSPsOld <- function(unit, mi = FALSE) {
  h1 <- if (mi) "SSPs-MI" else "SSPs"
  h2 <- if (mi) "SSPsOld-MI" else "SSPsOld"

  gdp <- calcOutput("GDPFuture", GDPFuture = h1, unit = unit, aggregate = FALSE)
  gdp <- setNames(gdp, c("gdppc_SSP1", "gdppc_SSP2", "gdppc_SSP3", "gdppc_SSP4", "gdppc_SSP5"))

  pop <- calcOutput("PopulationFuture", PopulationFuture = h2, aggregate = FALSE)
  pop <- setNames(pop, c("gdppc_SSP1", "gdppc_SSP2", "gdppc_SSP3", "gdppc_SSP4", "gdppc_SSP5"))

  years <- intersect(getYears(gdp), getYears(pop))
  data <- gdp[, years, ] / pop[, years, ]
  data[is.nan(data) | data == Inf] <- 0
  data
}

toolGDPpcFutureSDPs <- function(unit, mi = FALSE) {
  dataSSP1 <- toolGDPpcFutureSSPsOld(unit, mi)[, , "gdppc_SSP1"]

  purrr::map(c("SDP", "SDP_EI", "SDP_RC", "SDP_MC"),
             ~ setNames(dataSSP1, gsub("SSP1", .x, getNames(dataSSP1)))) %>%
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
