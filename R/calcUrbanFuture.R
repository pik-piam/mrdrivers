#' @rdname calcPopulationPast
#' @param UrbanFuture A string designating the source for the future urban population-share data.
#'   Available sources are:
#'   \itemize{
#'     \item "SSPs":
#'     \item "SDPs":
#'     \item "SSP2EU":
#'   }
calcUrbanFuture <- function(UrbanFuture = "SSPs") { # nolint

  data <- switch(
    UrbanFuture,
    "SSPs"   = calcOutput("InternalUrbanFutureSSPs", aggregate = FALSE),
    "SSP2"   = calcOutput("InternalUrbanFutureSSPs", aggregate = FALSE)[, , "urb_SSP2"],
    "SDPs"   = calcOutput("InternalUrbanFutureSDPs", aggregate = FALSE),
    "SSP2EU" = calcOutput("InternalUrbanFutureSSP2EU", aggregate = FALSE),
    stop("Bad input for UrbanFuture. Invalid 'UrbanFuture' argument.")
  )

  data <- toolFinishingTouches(data)

  wp <- calcOutput("PopulationFuture", PopulationFuture = UrbanFuture, aggregate = FALSE)
  # Give weight same names as data, so that aggregate doesn't mess up data dim
  getNames(wp) <- gsub("pop", "urb", getNames(wp))

  data <- data[getItems(wp, 1), getYears(wp), ]

  list(x = data,
       weight = wp,
       unit = "share of population",
       description = paste0("Urban population share from {UrbanFuture}"))
}


######################################################################################
# Functions
######################################################################################
# Calculates a time series of urban shares, using SSP projections. Currently has some inconsistencies
# with WDI in 2010
calcInternalUrbanFutureSSPs <- function() {
  data <- readSource("SSP", "urb") / 100
  getNames(data) <- paste0("urb_", getNames(data))
  # Add some missing years
  data <- time_interpolate(data, seq(2025, 2095, by = 10), integrate_interpolated_years = TRUE)
  list(x = data, weight = NULL, unit = "per 1", description = "Urban data from SSP")
}

calcInternalUrbanFutureSDPs <- function() {
  # note on SHAPE SDPs:
  # SDP urban population share scenarios are mapped from SSP urban population share,
  # (in one case with different SSP choice for OECD and non-OECD countries)
  # The alternative scenario combinations are not coded explicitly here.
  # They will re-use urban population share settings:
  # SDP_LS = SDP_MC (Green cities), SDP_GS = SDP_EI (Tech cities)

  urbanMapping <- c("urb_SDP"    = "urb_SSP1",
                    "urb_SDP_EI" = "urb_SSP1", # = high urban pop share from SSPs for all countries
                    "urb_SDP_RC" = "urb_SSP3|urb_SSP2", # low urban pop share from SSPs for OECD, med for non-OECD
                    "urb_SDP_MC" = "urb_SSP1") # = high urban pop share from SSPs for all countries

  sspUrb <- calcOutput("UrbanFuture", UrbanFuture = "SSPs", aggregate = FALSE) # nolint

  data <- purrr::imap(urbanMapping, mapSHAPEurban, sspUrb = sspUrb) %>% mbind()
  list(x = data, weight = NULL, unit = "per 1", description = "Urban data from SDP")
}

mapSHAPEurban <- function(ssp, sdp, sspUrb) {
  if (grepl("|", ssp, fixed = TRUE)) {
    # distinguish between OECD and non-OECD
    ssp <- strsplit(ssp, split = "\\|")[[1]]
    oecdMapping <- toolGetMapping("regionmappingOECD.csv", type = "regional", where = "mappingfolder")
    oecd <- oecdMapping[oecdMapping$RegionCode == "OECD", "CountryCode"]
    nonOECD <- oecdMapping[oecdMapping$RegionCode == "Non-OECD", "CountryCode"]
    mbind(setNames(sspUrb[oecd, , ssp[1]], sdp),
          setNames(sspUrb[nonOECD, , ssp[2]], sdp))
  } else {
    # same for OECD and non-OECD
    setNames(sspUrb[, , ssp], sdp)
  }
}

calcInternalUrbanFutureSSP2EU <- function() {
  ssp2Urb <- calcOutput("UrbanFuture", UrbanFuture = "SSPs", aggregate = FALSE)[, , "urb_SSP2"]
  ssp2Urb <- setNames(ssp2Urb, sub("SSP2", "SSP2EU", getNames(ssp2Urb)))
  list(x = ssp2Urb, weight = NULL, unit = "per 1", description = "Urban data from SSP2EU")
}
