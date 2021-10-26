#' calcUrbanFuture
#'
#' Calculates a time series of urban shares, using SSP projections Currently,
#' SSP data does not differentiate between SSPs and has some unconsistencies
#' with WDI in 2010
#'
#' @inheritParams calcUrban
#' @inherit calcUrban return
#'
#' @seealso [madrat::calcOutput()]
#' @family Urban functions
#'
#' @examples \dontrun{
#' library(mrdrivers)
#' calcOutput("UrbanFuture")
#' }
#'
calcUrbanFuture <- function(UrbanFuture = "SSPs", extension2150 = "none") {

  data <- switch(
    UrbanFuture,
    "SSPs"   = cUrbanFutureSSPs(),
    "SDPs"   = cUrbanFutureSDPs(),
    "SSP2EU" = cUrbanFutureSSP2EU(),
    stop("Bad input for UrbanFuture. Invalid 'UrbanFuture' argument.")
  )

  data <- toolFinishingTouches(data, extension2150)

  wp <- calcOutput("PopulationFuture",
                   PopulationFuture = UrbanFuture,
                   extension2150 = extension2150,
                   aggregate = FALSE)
  # Give weight same names as data, so that aggregate doesn't mess up data dim
  getNames(wp) <- gsub("pop", "urb", getNames(wp))

  data <- data[getItems(wp, 1), getYears(wp), ]

  list(x = data, weight = wp, unit = "per 1", description = paste0("Urbanisation data from {UrbanFuture}"))
}


######################################################################################
# Functions
######################################################################################
cUrbanFutureSSPs <- function() {
  data <- collapseNames(readSource("SSP", "urb")) / 100
  getSets(data)[3] <- "variable"
  getNames(data) <- paste0("urb_", gsub("_v[[:alnum:],[:punct:]]*", "", getNames(data)))


  # Remove years which only contain 0s as entries
  data <- data[, !apply(data, 2, function(x) all(x == 0)), ]

  timeInter <- paste0("y", seq(2015, 2095, by = 10))
  data <- time_interpolate(data, timeInter, integrate_interpolated_years = TRUE)
}

cUrbanFutureSDPs <- function() {
  # note on SHAPE: the alternative scenario combinations are not coded explicitly here.
  # They will re-use urbanization settings:
  # SDP_LS = SDP_MC (Green cities), SDP_GS = SDP_EI (Tech cities)
  urbanizationMapping <- c("urb_SDP"    = "urb_SSP1",
                           "urb_SDP_EI" = "urb_SSP1", # = high urbanization from SSPs
                           "urb_SDP_RC" = "urb_SSP2", # = med urbanization from SSPs
                           "urb_SDP_MC" = "urb_SSP1") # = high urbanization from SSPs

  sspUrb <- calcOutput("UrbanFuture", UrbanFuture = "SSPs", aggregate = FALSE) # nolint

  purrr::imap(urbanizationMapping,
              ~ setNames(sspUrb[, , .x], .y)) %>%
    mbind()
}

cUrbanFutureSSP2EU <- function() {
  ssp2Urb <- calcOutput("UrbanFuture", UrbanFuture = "SSPs", aggregate = FALSE)[, , "urb_SSP2"]
  setNames(ssp2Urb, sub("SSP2", "SSP2EU", getNames(ssp2Urb)))
}
