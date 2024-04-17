#' @rdname calcPopulationPast
#' @param PopulationFuture A string designating the source for the future population data.
#'   Available sources are:
#'   \itemize{
#'     \item "SSPs": From the Wittgenstein Center [here](http://pure.iiasa.ac.at/id/eprint/17550/) and
#'                   [here](http://pure.iiasa.ac.at/id/eprint/16710/)
#'     \item "SSP2EU": Combined SSP2 and Eurostat (for the EU countries) source
#'     \item "SDPs":
#'     \item "UN_PopDiv": United Nations
#'     \item "MI": Missing island dataset
#'   }
calcPopulationFuture <- function(PopulationFuture = "SSPs-UN_PopDiv-MI") { # nolint
  # Check user input
  toolCheckUserInput("PopulationFuture", as.list(environment()))
  # Call calcInternalPopulationFuture function the appropriate number of times (map) and combine (reduce)
  # !! Keep formula syntax for madrat caching to work
  purrr::pmap(list("PopulationFuture" = unlist(strsplit(PopulationFuture, "-"))),
              ~calcOutput("InternalPopulationFuture", aggregate = FALSE, supplementary = TRUE, ...)) %>%
    toolReduce(mbindOrFillWith = "fillWith")
}

######################################################################################
# Internal Function
######################################################################################
calcInternalPopulationFuture <- function(PopulationFuture) { # nolint
  data <- switch(
    PopulationFuture,
    "SSPs"      = calcOutput("InternalPopulationFutureSSPs", aggregate = FALSE, supplementary = TRUE),
    "SSP2"      = calcOutput("InternalPopulationFutureSSP2", aggregate = FALSE, supplementary = TRUE),
    "SSP2EU"    = calcOutput("InternalPopulationFutureSSP2EU", aggregate = FALSE, supplementary = TRUE),
    "SDPs"      = calcOutput("InternalPopulationFutureSDPs", aggregate = FALSE, supplementary = TRUE),
    "UN_PopDiv" = calcOutput("InternalPopulationFutureUN_PopDiv", aggregate = FALSE, supplementary = TRUE),
    "MI"        = calcOutput("InternalPopMI", aggregate = FALSE, supplementary = TRUE),
    stop("Bad input for PopulationFuture. Invalid 'PopulationFuture' argument.")
  )

  data$x <- toolFinishingTouches(data$x)
  data
}



######################################################################################
# Functions
######################################################################################
calcInternalPopulationFutureSSPs <- function() { # nolint
  data <- readSource("SSP", "pop")
  getNames(data) <- paste0("pop_", getNames(data))
  list(x = data, weight = NULL, unit = "million", description = "SSP projections")
}

calcInternalPopulationFutureSSP2 <- function() { # nolint
  data <- calcOutput("InternalPopulationFutureSSPs", aggregate = FALSE)[, , "pop_SSP2"]
  list(x = data, weight = NULL, unit = "million", description = "SSP2 projections")
}

calcInternalPopulationFutureSDPs <- function() { # nolint
  data_SSP1 <- calcOutput("InternalPopulationFutureSSPs", aggregate = FALSE)[, , "pop_SSP1"] # nolint

  data <- purrr::map(c("SDP", "SDP_EI", "SDP_RC", "SDP_MC"),
                     ~ setNames(data_SSP1, gsub("SSP1", .x, getNames(data_SSP1)))) %>%
    mbind()
  list(x = data, weight = NULL, unit = "million", description = "SSP1 projections")
}

calcInternalPopulationFutureSSP2EU <- function() { # nolint
  dataEurostat <- readSource("EurostatPopGDP", "population_projections") * 1e-6
  dataSSP2 <- calcOutput("InternalPopulationFutureSSPs", aggregate = FALSE)[, , "pop_SSP2"]

  # Get EUR countries - GBR. (Great Britatin still in EUR mapping, but no Eurostat projections exist.)
  euCountries <- toolGetEUcountries()

  # Get common years
  cy <- intersect(getYears(dataSSP2),  getYears(dataEurostat))

  # Start with the SSP2 scenario until 2100. Change the name, and overwrite the EUR
  # countries with the Eurostat data.
  data <- dataSSP2[, getYears(dataSSP2)[getYears(dataSSP2, as.integer = TRUE) <= 2100], ] %>%
    setNames("pop_SSP2EU")
  data[euCountries, , ] <- 0
  data[euCountries, cy, ] <- dataEurostat[euCountries, cy, ]
  list(x = data,
       weight = NULL,
       unit = "million",
       description = "SSP2 projections for non-EU countries, and EUROSTAT projections for EU countries")
}

calcInternalPopulationFutureUN_PopDiv <- function() { # nolint
  data <- readSource("UN_PopDiv", "medium") * 1e-3
  getNames(data) <- "pop_medium_variant"
  list(x = data, weight = NULL, unit = "million", description = "UN_PopDiv projections")
}

calcInternalPopMI <- function() {
  data <- readSource("MissingIslands", "pop")
  list(x = data, weight = NULL, unit = "million", description = "MI projections")
}
