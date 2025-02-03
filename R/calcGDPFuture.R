#' @rdname calcGDPPast
#' @param futureData A string specifying the sources for future projections.
#' @order 3
calcGDPFuture <- function(futureData = "SSPs") {
  # Check user input
  toolCheckUserInput("GDPFuture", as.list(environment()))
  # Call calcInternalGDPFuture function the appropriate number of times (map) and combine (reduce)
  # !! Keep formula syntax for madrat caching to work
  purrr::pmap(list("futureData" = unlist(strsplit(futureData, "-"))),
              ~calcOutput("InternalGDPFuture", aggregate = FALSE, supplementary = TRUE, ...)) %>%
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
    "SSP2EU" = setNames(readSource("SSP", "gdp", "SSP2"), "SSP2EU"),
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
calcGDPpcFuture <- function(scenario = "SSPs") {
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

  # The weight does go into the weight of the final scenario. So exact matching with GDPpc not necessary...
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



# Legacy...
toolGDPFutureOtherUnit <- function(data, baseYear, baseYearDef = 2017, convergeBy = 2100) {
  if (baseYear == baseYearDef) {
    return(data)
  }

  # Construct SSP pathways in constant "baseYear" Int$PPP.
  # For the near future, convert using current conversion factors.
  # After that the scenarios are built by converting the US GDP, and building
  # the other countries in relation to the US so that by "convergeBy", they have the same ratio as in the "defUnit".
  data2 <- data

  # The near future is defined hear by the next 15 years, or until 10 years after the last imf prediction.
  c15 <- max(getYears(readSource("IMF", "GDPpc"), as.integer = TRUE)) + 10

  y1 <- getYears(data2)[getYears(data2, as.integer = TRUE) <= c15]
  dataNearFut <- data2[, y1, ] %>%
    GDPuc::toolConvertGDP(glue("constant {baseYearDef} Int$PPP"),
                          glue("constant {baseYear} Int$PPP"),
                          replace_NAs = c("linear", "no_conversion"))

  y2 <- getYears(data2)[getYears(data2, as.integer = TRUE) > c15 & getYears(data2, as.integer = TRUE) < 2100]
  dataFarFut <- data2[, y2, ] * NA

  # Convert to "baseYear" Int$PPP using the "baseYear" value of base baseYearDef GDP deflator
  # (in constant baseYear LCU per constant baseYearDef LCU) of the USA
  # LONGTERM: allow other PPP units
  dataEnd <- data2[, convergeBy, ] * GDPuc::toolConvertSingle(1, "USA", "2010",
                                                              glue("constant {baseYearDef} LCU"),
                                                              glue("constant {baseYear} LCU"))

  dataConverted <- mbind(dataNearFut, dataFarFut, dataEnd)

  ratio <- data2 / dataConverted
  # For interpolation to work, the last and first values have to be non-NA/non-NaN
  ratio[, convergeBy, ][is.na(ratio[, convergeBy, ])] <- 0
  # The first 2 years of the SSP data set are incomplete. For countries that only lack data in these first 2 years,
  # set NaN to 0.
  ratio[, 2000, ][is.nan(ratio[, 2000, ]) & !is.nan(ratio[, 2010, ])] <- 0

  ratio <- as.data.frame(ratio, rev = 2) %>%
    dplyr::rename("value" = ".value") %>%
    dplyr::arrange(.data$year) %>%
    dplyr::group_by(.data$iso3c, .data$variable) %>%
    dplyr::mutate(value = zoo::na.approx(.data$value)) %>%
    dplyr::ungroup() %>%
    as.magpie(tidy = TRUE)

  dataConverted <- data2 / ratio
  dataConverted[is.na(dataConverted)] <- data2[is.na(dataConverted)]
  # Above should probably be "<- 0"
  dataConverted
}
