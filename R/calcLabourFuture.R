#' @rdname calcPopulationPast
#' @param LabourFuture A string designating the source for the future working-age population data.
#'   Available sources are:
#'   \itemize{
#'     \item "SSPs":
#'   }
calcLabourFuture <- function(LabourFuture = "SSPs") { # nolint
  # Check user input
  toolCheckUserInput("LabourFuture", as.list(environment()))
  # Call calcInternalPopulationFuture function the appropriate number of times (map) and combine (reduce)
  # !! Keep formula syntax for madrat caching to work
  purrr::pmap(list("LabourFuture" = unlist(strsplit(LabourFuture, "-"))),
              ~calcOutput("InternalLabourFuture", aggregate = FALSE, supplementary = TRUE, ...)) %>%
    toolReduce(mbindOrFillWith = "fillWith")
}

calcInternalLabourFuture <- function(LabourFuture) { # nolint
  x <- switch(
    LabourFuture,
    "SSPs"    = calcOutput("InternalLabourFutureSSPs", aggregate = FALSE),
    "SSP2"    = calcOutput("InternalLabourFutureSSPs", aggregate = FALSE)[, , "lab_SSP2"],
    "SDPs"    = calcOutput("InternalLabourFutureSDPs", aggregate = FALSE),
    "SSP2EU"  = calcOutput("InternalLabourFutureSSP2EU", aggregate = FALSE),
    stop("Bad input for calcLabour. Invalid 'LabourFuture' argument.")
  )

  # Apply finishing touches to combined time-series
  x <- toolFinishingTouches(x)

  # Hopefully temporary: rename lab scnearios pop. Necessary for REMIND to work.
  getNames(x) <- sub("lab_", "pop_", getNames(x))

  list(x = x,
       weight = NULL,
       unit = "million",
       description = glue("Working age population data."))
}

######################################################################################
# Functions
######################################################################################
calcInternalLabourFutureSSPs <- function() {
  x <- readSource("SSP", "lab")[, , c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5")]
  # Remove years which only contain 0
  x <- x[, !apply(x, 2, function(y) all(y == 0)), ]
  getNames(x) <- paste0("lab_", getNames(x))
  list(x = x, weight = NULL, unit = "million", description = "Labor from SSPs")
}

calcInternalLabourFutureSDPs <- function() {
  labSSP1 <- calcOutput("InternalLabourFutureSSPs", aggregate = FALSE)[, , "lab_SSP1"] # nolint

  data <- purrr::map(c("SDP", "SDP_EI", "SDP_RC", "SDP_MC"),
                     ~ setNames(labSSP1, gsub("SSP1", .x, getNames(labSSP1)))) %>%
    mbind()
  list(x = data, weight = NULL, unit = "million", description = "Labor from SDPs")
}

calcInternalLabourFutureSSP2EU <- function() {
  labSSP2 <- calcOutput("InternalLabourFutureSSPs", aggregate = FALSE)[, , "lab_SSP2"]
  popSSP2 <- calcOutput("Population",
                        harmonization = "withPEAPandFuture",
                        pastData = "WDI",
                        futureData = "SSPs",
                        extension2150 = "none",
                        aggregate = FALSE)[, , "pop_SSP2"]
  popSSP2EU <- calcOutput("Population",
                          harmonization = "calibSSP2EU",
                          pastData = "Eurostat-WDI",
                          futureData = "SSP2EU",
                          extension2150 = "none",
                          aggregate = FALSE)[, , "pop_SSP2EU"]

  y <- getYears(labSSP2)
  getNames(popSSP2) <- getNames(popSSP2EU) <- getNames(labSSP2) <- "lab_SSP2EU"
  labSSP2EU <- labSSP2 / popSSP2[, y, ] * popSSP2EU[, y, ]
  labSSP2EU[is.na(labSSP2EU)] <- 0
  list(x = labSSP2EU, weight = NULL, unit = "million", description = "Labor from SSP2EU")
}
