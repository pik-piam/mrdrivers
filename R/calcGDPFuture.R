#' @rdname calcGDPPast
#' @param futureData A string specifying the sources for future projections.
#' @order 3
calcGDPFuture <- function(futureData) {
  toolCheckUserInput("GDPFuture", as.list(environment()))
  # Map over components of futureData.
  purrr::map(unlist(strsplit(futureData, "-")),
             ~calcOutput("InternalGDPFuture", futureData = .x, aggregate = FALSE, supplementary = TRUE)) %>%
    toolListFillWith()
}

calcInternalGDPFuture <- function(futureData) {
  data <- switch(
    futureData,
    "SSPs"   = readSource("SSP", "gdp"),
    "SSP1"   = readSource("SSP", "gdp", "SSP1"),
    "SSP2"   = readSource("SSP", "gdp", "SSP2"),
    "SSP3"   = readSource("SSP", "gdp", "SSP3"),
    "SSP4"   = readSource("SSP", "gdp", "SSP4"),
    "SSP5"   = readSource("SSP", "gdp", "SSP5"),
    "SDPs"   = toolGDPFutureSDPs(),
    stop("Bad input for calcGDPFuture. Invalid 'futureData' argument.")
  )

  list(x = data,
       weight = NULL,
       unit = glue("mil. {toolGetUnitDollar(inPPP = TRUE)}"),
       description = glue("{futureData} projections"))
}

toolGDPFutureSDPs <- function(sdps = c("SDP", "SDP_EI", "SDP_MC", "SDP_RC")) {
  gdpSSP1 <- readSource("SSP", "gdp", "SSP1") # nolint: object_usage_linter.
  purrr::map(sdps, ~setNames(gdpSSP1, .x)) %>% mbind()
}

#' @rdname calcGDPPast
#' @order 4
calcGDPpcFuture <- function(scenario) {
  # We can not fill data sources as in GDP and Pop, as GDP and pop on their part are filled with MI, and the countries
  # which are filled in do not match. (WDI has pop data for some countries, but not GDP.) So to make sure that the
  # GDP per capita is consistent, we have to pass it on to the GDP and pop functions.
  gdpFutureData <- toolGetScenarioDefinition("GDPpc", scenario, aslist = TRUE)$futureData
  popFutureData <- toolGetScenarioDefinition("Population", scenario, aslist = TRUE)$futureData

  data <- switch(
    scenario,
    "SSP2IndiaDEAs" = toolFillWith(
      readSource("IndiaDEA", "gdppc"),
      toolGDPpcFutureFromGDPAndPop(sub("IndiaDEAs-", "", gdpFutureData), popFutureData)
    ),
    "SSP2IndiaMedium" = toolFillWith(
      readSource("IndiaDEA", "gdppc", "baseline"),
      toolGDPpcFutureFromGDPAndPop(sub("IndiaDEAbase-", "", gdpFutureData), popFutureData)
    ),
    "SSP2IndiaHigh" = toolFillWith(
      readSource("IndiaDEA", "gdppc", "optimistic"),
      toolGDPpcFutureFromGDPAndPop(sub("IndiaDEAopt-", "", gdpFutureData), popFutureData)
    ),
    toolGDPpcFutureFromGDPAndPop(gdpFutureData, popFutureData)
  )

  # The weight does not go into the weight of the final scenario. So exact matching with GDPpc not necessary.
  weight <- calcOutput("PopulationFuture", futureData = popFutureData, aggregate = FALSE)
  getNames(weight) <- getNames(data)

  list(x = data, weight = weight, unit = toolGetUnitDollar(inPPP = TRUE), description = glue("{scenario} projections"))
}


toolGDPpcFutureFromGDPAndPop <- function(gdpFutureData, popFutureData) {
  gdp <- calcOutput("GDPFuture", futureData = gdpFutureData, aggregate = FALSE)
  pop <- calcOutput("PopulationFuture", futureData = popFutureData, aggregate = FALSE)
  years <- intersect(getYears(gdp), getYears(pop))
  data <- gdp[, years, ] / pop[, years, ]
  data[is.nan(data) | data == Inf] <- 0
  data
}

