#' @rdname calcGDPPast
calcPopulationFuture <- function(futureData) {
  # Check user input
  toolCheckUserInput("PopulationFuture", as.list(environment()))
  # Call calcInternalPopulationFuture function the appropriate number of times (map) and combine (reduce)
  # !! Keep formula syntax for madrat caching to work
  purrr::pmap(list("futureData" = unlist(strsplit(futureData, "-"))),
              ~calcOutput("InternalPopulationFuture", aggregate = FALSE, supplementary = TRUE, ...)) %>%
    toolListFillWith()
}

calcInternalPopulationFuture <- function(futureData) {
  data <- switch(
    futureData,
    "SSPs"         = readSource("SSP", "pop"),
    "SSP1"         = readSource("SSP", "pop", "SSP1"),
    "SSP2"         = readSource("SSP", "pop", "SSP2"),
    "SSP3"         = readSource("SSP", "pop", "SSP3"),
    "SSP4"         = readSource("SSP", "pop", "SSP4"),
    "SSP5"         = readSource("SSP", "pop", "SSP5"),
    "SDPs"         = toolPopulationFutureSDPs(),
    "UN_PopDiv"    = readSource("UN_PopDiv", "pop", "medium"),
    "IndiaDEAs"    = readSource("IndiaDEA", "pop"),
    "IndiaDEAbase" = readSource("IndiaDEA", "pop", "baseline"),
    "IndiaDEAopt"  = readSource("IndiaDEA", "pop", "optimistic"),
    stop("Bad input for calcPopulationFuture. Invalid 'futureData' argument.")
  )

  list(x = data, weight = NULL, unit = "million", description = glue("{futureData} projections"))
}

toolPopulationFutureSDPs <- function(sdps = c("SDP", "SDP_EI", "SDP_MC", "SDP_RC")) {
  popSSP1 <- readSource("SSP", "pop", "SSP1") # nolint: object_usage_linter.
  purrr::map(sdps, ~setNames(popSSP1, .x)) %>% mbind()
}


#' @rdname calcGDPPast
calcLabourFuture <- function(futureData) {
  # Check user input
  toolCheckUserInput("LabourFuture", as.list(environment()))
  # Call calcInternalPopulationFuture function the appropriate number of times (map) and combine (reduce)
  # !! Keep formula syntax for madrat caching to work
  purrr::pmap(list("futureData" = unlist(strsplit(futureData, "-"))),
              ~calcOutput("InternalLabourFuture", aggregate = FALSE, supplementary = TRUE, ...)) %>%
    toolListFillWith()
}

calcInternalLabourFuture <- function(futureData) {
  data <- switch(
    futureData,
    "SSPs"      = readSource("SSP", "lab"),
    "SSP1"      = readSource("SSP", "lab", "SSP1"),
    "SSP2"      = readSource("SSP", "lab", "SSP2"),
    "SSP3"      = readSource("SSP", "lab", "SSP3"),
    "SSP4"      = readSource("SSP", "lab", "SSP4"),
    "SSP5"      = readSource("SSP", "lab", "SSP5"),
    "SDPs"      = toolLabourFutureSDPs(),
    "UN_PopDiv" = readSource("UN_PopDiv", "lab", "medium"),
    stop("Bad input for calcLabour. Invalid 'futureData' argument.")
  )

  list(x = data, weight = NULL, unit = "million", description = glue("{futureData} projections"))
}

toolLabourFutureSDPs <- function(sdps = c("SDP", "SDP_EI", "SDP_MC", "SDP_RC")) {
  labSSP1 <- readSource("SSP", "lab", "SSP1") # nolint: object_usage_linter.
  purrr::map(sdps, ~setNames(labSSP1, .x)) %>% mbind()
}



#' @rdname calcGDPPast
calcUrbanFuture <- function(futureData) {
  data <- switch(
    futureData,
    "SSPs"   = toolUrbanFutureSSPs(),
    "SSP1"   = toolUrbanFutureSSPs("SSP1"),
    "SSP2"   = toolUrbanFutureSSPs("SSP2"),
    "SSP3"   = toolUrbanFutureSSPs("SSP3"),
    "SSP4"   = toolUrbanFutureSSPs("SSP4"),
    "SSP5"   = toolUrbanFutureSSPs("SSP5"),
    "SDPs"   = toolUrbanFutureSDPs(),
    stop("Bad input for calcUrbanFuture. Invalid 'futureData' argument.")
  )

  # Use population as weight. Give weight same names as data, so that aggregate does not mess up data dim.
  weight <- calcOutput("PopulationFuture", futureData = futureData, aggregate = FALSE)
  getNames(weight) <- getNames(data)

  list(x = data, weight = weight, unit = "share of population", description = glue("{futureData} projections"))
}

toolUrbanFutureSSPs <- function(ssps = c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5")) {
  data <- readSource("SSP", "urb", ssps)
  # Drop 2010 and 2015 values, and add missing years.
  data <- data[, c(2010, 2015), , invert = TRUE]
  time_interpolate(data, seq(2025, 2095, by = 10), integrate_interpolated_years = TRUE)
}

# The SDP urban population share scenarios are mapped from the SSP scenarios.
# The SDP, SDP_EI and SDP_MC have high urban pop share from SSP1. The SDP_RC scenario has low urban pop share from
# SSP3 for OECD countries, and medium urban pop share from SSP2 for non-OECD countries.
# The alternative scenario combinations SDP_LS and SDP_GS are not coded explicitly here.
# They will re-use urban population share settings: SDP_LS = SDP_MC (Green cities), SDP_GS = SDP_EI (Tech cities).
toolUrbanFutureSDPs <- function() {
  urbSSPs <- toolUrbanFutureSSPs()
  # SSP1 for the first 3 SDP scenarios
  urbSDPs <- purrr::map(c("SDP", "SDP_EI", "SDP_MC"), ~setNames(urbSSPs[, , "SSP1"], .x)) %>% mbind()
  # SSP2 and SSP3 for SDP_RC
  urbSDPrc <- setNames(urbSSPs[, , "SSP2"], "SDP_RC")
  oecdMapping <- toolGetMapping("regionmappingOECD.csv", type = "regional", where = "mappingfolder")
  oecdCountries <- oecdMapping[oecdMapping$RegionCode == "OECD", "CountryCode"]
  urbSDPrc[oecdCountries, , ] <- urbSSPs[oecdCountries, , "SSP3"]
  mbind(urbSDPs, urbSDPrc)
}
