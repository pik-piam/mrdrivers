#' Get labour scenarios
#'
#' @description
#' Get labour scenarios. By default the following scenarios are returned:
#' \itemize{
#'   \item the SSPs, i.e. SSP1-5 and SSP2EU
#'   \item the SDPs, i.e. SDP, SDP_EI, SDP_RC, and SDP_MC
#' }
#'
#' @param LabourFuture Labour future data source
#' @inheritParams calcGDP
#' @inherit calcGDP return
#' @inheritSection calcGDP Return supplementary information
#' @inheritSection calcGDP Vectorization of arguments
#'
#' @seealso [madrat::calcOutput()]
#' @family Population functions
#'
#' @examples \dontrun{
#' library(mrdrivers)
#' calcLabour()
#' }
#'
calcLabour <- function(LabourFuture = c("SSPs", "SDPs", "SSP2EU"), # nolint
                       extension2150 = "bezier") {
  # Check user input
  toolCheckUserInput("Labour", as.list(environment()))
  # Call calcInternalLabour function the appropriate number of times (map) and combine (reduce)
  # !! Keep formula syntax for madrat caching to work
  purrr::pmap(as.list(environment()), ~calcOutput("InternalLabour", aggregate = FALSE, supplementary = TRUE, ...)) %>%
    toolReduce()
}

######################################################################################
# Internal Function
######################################################################################
calcInternalLabour <- function(LabourFuture, extension2150) { # nolint
  x <- switch(
    LabourFuture,
    "SSPs"    = calcOutput("InternalLabourFutureSSPs", aggregate = FALSE),
    "SDPs"    = calcOutput("InternalLabourFutureSDPs", aggregate = FALSE),
    "SSP2EU"  = calcOutput("InternalLabourFutureSSP2EU", aggregate = FALSE),
    "SSPsOld" = calcOutput("InternalLabourFutureSSPsOld", aggregate = FALSE),
    stop("Bad input for calcLabour. Invalid 'LabourFuture' argument.")
  )

  # Apply finishing touches to combined time-series
  x <- toolFinishingTouches(x, extension2150)

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
  x <- readSource("SSP", "lab2018Update") * 1e-3

  # lab2018Update only starts in 2015. However data is needed back until 2005.
  # Use old Labour growth rates to fill in.
  y <- calcOutput("Labour", LabourFuture = "SSPsOld", extension = "none", aggregate = FALSE)
  y <- y[, 1:3, paste0("pop_", getNames(x))]
  getNames(y) <- getNames(x)
  getSets(y) <- getSets(x)
  x <- toolHarmonizeFutureGrPast(future = x, past = y)
  getSets(x) <- c("iso3c", "year", "variable")

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
                        PopulationCalib = "calibSSPs",
                        PopulationPast = "WDI",
                        PopulationFuture = "SSPs",
                        FiveYearSteps = FALSE,
                        extension2150 = "none",
                        aggregate = FALSE)[, , "pop_SSP2"]
  popSSP2EU <- calcOutput("Population",
                          PopulationCalib = "calibSSP2EU",
                          PopulationPast = "Eurostat-WDI",
                          PopulationFuture = "SSP2EU",
                          FiveYearSteps = FALSE,
                          extension2150 = "none",
                          aggregate = FALSE)[, , "pop_SSP2EU"]

  y <- getYears(labSSP2)
  getNames(popSSP2) <- getNames(popSSP2EU) <- getNames(labSSP2) <- "lab_SSP2EU"
  labSSP2EU <- labSSP2 / popSSP2[, y, ] * popSSP2EU[, y, ]
  labSSP2EU[is.na(labSSP2EU)] <- 0
  list(x = labSSP2EU, weight = NULL, unit = "million", description = "Labor from SSP2EU")
}


# Legacy
calcInternalLabourFutureSSPsOld <- function() { # nolint
  aged <- purrr::cross3("Population",
                        c("Male", "Female"),
                        c("Aged15-19", "Aged20-24", "Aged25-29", "Aged30-34", "Aged35-39",
                          "Aged40-44", "Aged45-49", "Aged50-54", "Aged55-59", "Aged60-64")) %>%
    purrr::map_chr(paste, collapse = "|")

  readSource("SSP", subtype = "all")[, , aged]

  data <- collapseNames(readSource("SSP", subtype = "all")[, , aged][, , "IIASA-WiC POP"])
  data <- dimSums(data, dim = 3.2)

  getNames(data) <- paste("pop_", gsub("_v[[:alnum:],[:punct:]]*", "", getNames(data)), sep = "")
  # change name of "SSP4d" to "SSP4
  getNames(data) <- sub("SSP4d", "SSP4", getNames(data))

  timeExtend <- c("y2105", "y2110", "y2115", "y2120", "y2125", "y2130", "y2135", "y2140", "y2145", "y2150")
  data <- time_interpolate(data, timeExtend, extrapolation_type = "constant", integrate_interpolated_years = TRUE)

  # delete 0s
  data <- data[, c("y2000", "y2005"), , invert = TRUE]
  # extrapolate data for 2005
  data <- time_interpolate(data, c("y2005"), extrapolation_type = "linear", integrate_interpolated_years = TRUE)

  # add SDP, SDP_EI, SDP_MC, SDP_RC scenario as copy of SSP1, might be substituted by real data later
  if ("pop_SSP1" %in% getNames(data, dim = 1)) {
     if (!("pop_SDP" %in% getNames(data, dim = 1))) {
      dataSDP <- data[, , "pop_SSP1"]
      getNames(dataSDP) <- gsub("pop_SSP1", "pop_SDP", getNames(dataSDP))
      data <- mbind(data, dataSDP)
    }
     if (!("pop_SDP_EI" %in% getNames(data, dim = 1))) {
      dataSDPEI <- data[, , "pop_SSP1"]
      getNames(dataSDPEI) <- gsub("pop_SSP1", "pop_SDP_EI", getNames(dataSDPEI))
      data <- mbind(data, dataSDPEI)
    }
     if (!("pop_SDP_MC" %in% getNames(data, dim = 1))) {
      dataSDPMC <- data[, , "pop_SSP1"]
      getNames(dataSDPMC) <- gsub("pop_SSP1", "pop_SDP_MC", getNames(dataSDPMC))
      data <- mbind(data, dataSDPMC)
    }
     if (!("pop_SDP_RC" %in% getNames(data, dim = 1))) {
      dataSDPRC <- data[, , "pop_SSP1"]
      getNames(dataSDPRC) <- gsub("pop_SSP1", "pop_SDP_RC", getNames(dataSDPRC))
      data <- mbind(data, dataSDPRC)
    }
  }
  # add SSP2EU scenario as copy of SSP2, might be substituted by real data later
  if ("pop SSP2" %in% getNames(data, dim = 1)) {
     if (!("pop_SSP2EU" %in% getNames(data, dim = 1))) {
      dataSSP2EU <- data[, , "pop_SSP1"]
      getNames(dataSSP2EU) <- gsub("pop_SSP1", "pop_SSP2EU", getNames(dataSSP2EU))
      data <- mbind(data, dataSSP2EU)
    }
  }

  list(x = data, weight = NULL, unit = "million", description = "Labor from SSPsOld")
}
