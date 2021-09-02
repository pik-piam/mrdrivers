#' calcUrbanFuture
#' 
#' Calculates a time series of urban shares, using SSP projections Currently,
#' SSP data does not differentiate between SSPs and has some unconsistencies
#' with WDI in 2010
#' 
#' @inheritParams calcUrban
#' @return Urban shares
calcUrbanFuture <- function(UrbanFuture = "SSPs", extension2150 = "constant") {
  
  data <- switch(
    UrbanFuture,
    "SSPs" = calcUrbanFutureSSPs(),
    stop("Bad input for UrbanFuture. Invalid 'UrbanFuture' argument.")
  )

  data <- finishingTouches(data, extension2150)

  wp <- calcOutput("PopulationFuture", 
                   PopulationFuture = UrbanFuture, 
                   useMIData = FALSE, 
                   extension2150 = extension2150,
                   aggregate = FALSE)
  getNames(wp) <- gsub("(urb_SSP\\d).*", "\\1", getNames(wp))

  data <- data[getRegions(wp), getYears(wp),]
  

  return(list(x = data,
              weight = wp,
              unit = "per 1",
              description = paste0("Urbanisation data from ", UrbanFuture)))
}


######################################################################################
# Functions
######################################################################################
calcUrbanFutureSSPs <- function() {
  data <- collapseNames(readSource("SSP", subtype = "all")[,,"Population|Urban|Share"][,,"NCAR"]) / 100
  getNames(data) <- paste0("urb_", gsub("_v[[:alnum:],[:punct:]]*", "", getNames(data)))
  
  #remove years which only contain 0s as entries
  data <- data[, !apply(data, 2, function(x) return(all(x == 0))),]
  
  time_inter <- paste0("y", seq(2015, 2095, by = 10))
  data <- time_interpolate(data, time_inter, integrate_interpolated_years = TRUE)
}
