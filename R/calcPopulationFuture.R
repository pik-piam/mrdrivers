#' calcPopulationFuture
#' 
#' Calculates a time series of Pop. Different sources are available:
#' \itemize{ \item \code{"IIASApop"}: Source: IIASA? Lavinia?  \item
#' \code{"IIASApop"}: Source: Lavinia? }
#' 
#' @inheritParams calcPopulation
#' @inherit calcPopulation return
#' 
#' @seealso [madrat::calcOutput()]
#' @family Population functions
#'
#' @examples \dontrun{
#' library(mrdrivers)
#' calcOutput("PopulationFuture")}
#'
calcPopulationFuture <- function(PopulationFuture = "SSPs-MI",
                                 extension2150 = "none") {
  # Call internal_calcPopulationFuture function the appropriate number of times
  toolInternalCalc("PopulationFuture", 
                   list("PopulationFuture" = strsplit(PopulationFuture, "-")[[1]],
                        "extension2150" = extension2150),
                   mbind_or_fillWith = "fillWith")
}

######################################################################################
# Internal Function
######################################################################################
internal_calcPopulationFuture <- function(PopulationFuture, extension2150) {
  data <- switch(PopulationFuture,
                 "SSPs"      = cPopulationFutureSSPs(),
                 "SSP2EU"    = cPopulationFutureSSP2EU(),
                 "SDPs"      = cPopulationFutureSDPs(),
                 "UN_PopDiv" = cPopulationFutureUN_PopDiv(),
                 "MI"        = readSource("MissingIslands", "pop"),
                 "SSPs_old"  = cPopulationFutureSSPsOld(),
                 # Deprecated options ? 
                 "SRES"     = cPopulationFutureSRES(),
                 "IIASApop" = readSource("IIASApop") * 1e-6,
                 stop("Bad input for PopulationFuture. Invalid 'PopulationFuture' argument."))

  data <- toolFinishingTouches(data, extension2150)

  list(x = data,
       weight = NULL,
       unit = "million",
       description = paste0("Population data from ", PopulationFuture))
}



######################################################################################
# Functions
######################################################################################
cPopulationFutureSSPs <- function() {
  data <- readSource("SSP", "pop2018Update") * 1e-3
  getNames(data) <- paste0("pop_", getNames(data))
  data
}

cPopulationFutureSDPs <- function() {
  data_SSP1 <- cPopulationFutureSSPs()[,, "pop_SSP1"]

  data <- purrr::map(c("SDP", "SDP_EI", "SDP_RC", "SDP_MC"),
                     ~ setNames(data_SSP1, gsub("SSP1", .x, getNames(data_SSP1)))) %>%
    mbind()
}

cPopulationFutureSSP2EU <- function() {
  data_eurostat <- readSource("EurostatPopGDP", "population_projections") * 1e-6
  data_ssp2 <- cPopulationFutureSSPs()[,, "pop_SSP2"]

  # Get EUR countries - GBR. (Great Britatin still in EUR mapping, but no Eurostat projections exist.) 
  EUR_countries <- toolGetEUcountries()
  
  # Get common years
  cy <- intersect(getYears(data_ssp2),  getYears(data_eurostat))

  # Start with the SSP2 scenario until 2100. Change the name, and overwrite the EUR
  # countries with the Eurostat data.
  data <- data_ssp2[, getYears(data_ssp2)[getYears(data_ssp2, as.integer = TRUE) <= 2100], ] %>% 
    setNames("pop_SSP2EU")
  data[EUR_countries,,] <- 0
  data[EUR_countries, cy, ] <- data_eurostat[EUR_countries, cy,]
  data
}

cPopulationFutureSSPsOld <- function() {
  data <- readSource("SSP", "pop")
  
  # Refactor names
  data <- collapseNames(data) 
  getNames(data) <- paste0("pop_", gsub("_v[[:alnum:],[:punct:]]*", "", getNames(data)))
  getNames(data) <- sub("SSP4d", "SSP4", getNames(data))

  # Remove 2000 and 2005, because these years are not complete
  data <- data[,setdiff(getYears(data), c("y2000", "y2005")),]
}

cPopulationFutureUN_PopDiv <- function() {
  data <- readSource("UN_PopDiv", "WPP2019_medium") * 1e-3
  getNames(data) <- "pop_medium_variant"
  data
}

######################################################################################
# Legacy
######################################################################################
cPopulationFutureSRES <- function() {
  data <- NULL
  for (i in c("sres_a1_pop", "sres_a2_pop", "sres_b1_pop", "sres_b2_pop")) {
    data <- mbind(data, readSource("SRES", i))
  }
  getNames(data) <- paste0("pop_", substr(getNames(data), 6, 7))
  
  fill <- calcOutput("PopulationFuture", 
                     PopulationFuture = "SSPs", 
                     extension2150 = "none",
                     aggregate = FALSE)[,,"pop_SSP2"]
  data <- data %>% 
    toolFillWith(fill) %>% 
    toolInterpolateAndExtrapolate()
}
