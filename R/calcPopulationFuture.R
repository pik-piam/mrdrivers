#' calcPopulationFuture
#' 
#' Calculates a time series of Population. Different sources are available and
#' can be selected in the madrat config (getConfig()$calc$PopulationPast):
#' \itemize{ \item \code{"IIASApop"}: Source: IIASA? Lavinia?  \item
#' \code{"IIASApop"}: Source: Lavinia? }
#' 
#' @param PopulationFuture population future data source
#' @return Population in millions.
#' @author Lavinia Baumstark, Benjamin Bodirsky, Johannes Koch
calcPopulationFuture <- function(PopulationFuture = "SSP2018Update_completed_bezierOut") {

  # Look for "_completed" tag. Remove if found.
  if (grepl("_completed", PopulationFuture)) {
    PopulationFuture <- gsub("_completed", "", PopulationFuture)
    complete <- TRUE
  } else {
    complete <- FALSE
  }

  # Look for "_bezierOut" tag. Remove if found.
  if (grepl("_bezierOut", PopulationFuture)) {
    PopulationFuture <- gsub("_bezierOut", "", PopulationFuture)
    bezierOut <- TRUE
  } else {
    bezierOut <- FALSE
  }

  # Check left-over input argument  
  valid_inputs <- c(
    "SSP",
    "SSP2018Update",
    "SRES",
    "IIASApop",
    "SSP2Ariadne"
  )
  if (!PopulationFuture %in% valid_inputs) {
    stop("Bad input for PopulationFuture. Invalid 'PopulationFuture' argument.")
  }

  data <- switch(PopulationFuture,
                 "SSP" = calcPopulationFutureSSP(complete, bezierOut),
                 "SSP2018Update" = calcPopulationFutureSSP2018Update(complete, bezierOut),
                 "SRES" = calcPopulationFutureSRES(complete),
                 "IIASApop" = calcPopulationFutureIIASApop(),
                 "SSP2Ariadne" = calcPopulationFutureSSP2Ariadne(complete, bezierOut))

  # Clean and put in alphabetical order
  data <- clean_magpie(data)
  data <- data[,,order(getNames(data))]

  return(list(x = data,
              weight = NULL,
              unit = "million",
              description = paste0("Population data based on ", PopulationFuture)))
}



######################################################################################
# Functions
######################################################################################
calcPopulationFutureIIASApop <- function() {
  data <- readSource("IIASApop") / 1e+6
  time_extend <- seq(2105, 2150, 5)
  data <- time_interpolate(data,
                           time_extend,
                           extrapolation_type = "constant",
                           integrate_interpolated_years = TRUE)
  data
}

calcPopulationFutureSSP <- function(complete, bezierOut) {
  data <- readSource("SSP", subtype = "all")[,,"Population"][,,"IIASA-WiC POP"]
  
  # Refactor names
  data <- collapseNames(data) 
  getNames(data) <- paste0("pop_", gsub("_v[[:alnum:],[:punct:]]*", "", getNames(data)))
  getNames(data) <- sub("SSP4d", "SSP4", getNames(data))

  # Remove 2000 and 2005, because these years are not complete
  data <- data[,setdiff(getYears(data), c("y2000", "y2005")),]

  # Extend from 2100 to 2150
  time_extend <- seq(2105, 2150, 5)
  if (bezierOut) {
    data <- bezierExtension(data, time_extend)
  } else {
    helper <- getSets(data)
    data <- time_interpolate(data,
                             time_extend,
                             extrapolation_type = "constant",
                             integrate_interpolated_years = TRUE)
    # Time_interpolate destroys the setNames for some reason...
    getSets(data) <- helper
  }
  
  # Complete with missing islands
  if (complete) {
     fill <- readSource("MissingIslands", subtype = "pop", convert = FALSE)
     data <- completeData(data, fill)
  }

  data
}

calcPopulationFutureSSP2018Update <- function(complete, bezierOut) {
  data <- readSource("SSP", "pop2018Update") / 1e+3
  getNames(data) <- paste0("pop_", getNames(data))

  # Extend from 2100 to 2150
  time_extend <- seq(2105, 2150, 5)
  if (bezierOut) {
    data <- bezierExtension(data, time_extend)
  } else {
    helper <- getSets(data)
    data <- time_interpolate(data,
                             time_extend,
                             extrapolation_type = "constant",
                             integrate_interpolated_years = TRUE)
    # Time_interpolate destroys the setNames for some reason...
    getSets(data) <- helper
  }
  
  # Complete with missing islands
  if (complete) {
     fill <- readSource("MissingIslands", subtype = "pop", convert = FALSE)
     data <- completeData(data, fill)
  }

  data
}


calcPopulationFutureSRES <- function(complete) {
  data <- NULL
  for (i in c("sres_a1_pop","sres_a2_pop","sres_b1_pop","sres_b2_pop")) {
    data <- mbind(data, readSource("SRES", i))
  }
  getNames(data) <- paste0("pop_", substr(getNames(data), 6, 7))
  time_extend <- seq(2105, 2150, 5)
  data <- time_interpolate(data,
                           time_extend,
                           extrapolation_type = "constant",
                           integrate_interpolated_years = TRUE)
  data[is.na(data)] <- 0

  if (complete) {
     fill <- calcOutput("PopulationFuture", PopulationFuture = "SSP_completed",  aggregate = F)[,,"pop_SSP2"]
     data <- completeData(data, fill)
  }

  data
}


calcPopulationFutureSSP2Ariadne <- function(complete, bezierOut) {
  data_eurostat <- readSource("Eurostat", "population_projections") / 1e+6
  data_ssp2 <- calcPopulationFutureSSP2018Update(FALSE, FALSE)[,, "pop_SSP2"]

  # Get EUR countries - GBR. (Great Britatin still in EUR mapping, but no Eurostat projections exist.) 
  EUR_countries <- toolGetMapping("regionmappingH12.csv") %>% 
    tibble::as_tibble() %>% 
    dplyr::filter(.data$RegionCode == "EUR", .data$CountryCode != "GBR") %>% 
    dplyr::pull(.data$CountryCode)
  
  # Get common years
  cy <- intersect(getYears(data_ssp2),  getYears(data_eurostat))

  # Start with the SSP2 scenario until 2100. Change the name, and overwrite the EUR
  # countries with the Eurostat data.
  data <- data_ssp2[, getYears(data_ssp2)[getYears(data_ssp2, as.integer = TRUE) <= 2100], ] %>% 
    setNames("pop_SSP2Ariadne")
  data[EUR_countries,,] <- 0
  data[EUR_countries, cy, ] <- data_eurostat[EUR_countries, cy,]

  # Extend from 2100 to 2150
  time_extend <- seq(2105, 2150, 5)
  if (bezierOut) {
    data <- bezierExtension(data, time_extend)
  } else {
    helper <- getSets(data)
    data <- time_interpolate(data,
                             time_extend,
                             extrapolation_type = "constant",
                             integrate_interpolated_years = TRUE)
    # Time_interpolate destroys the setNames for some reason...
    getSets(data) <- helper
  }
  
  # Complete with missing islands
  if (complete) {
     fill <- readSource("MissingIslands", subtype = "pop", convert = FALSE)
     data <- completeData(data, fill)
  }

  data
}
