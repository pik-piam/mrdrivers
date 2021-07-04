#' calcGDPpppFuture
#'
#' Calculates a time series of GDP in Purchase Power Parity (PPP) of million
#' International Dollars of the year 2005.  The source is selected in the config
#' file(getConfig()$calc$GDPpppFuture). Different sources are available:
#' \itemize{ \item \code{SSP}: Lavinia? \item \code{OECD}: Lavinia? }
#'
#' @param GDPpppFuture GDPppp future data source
#' @return GDP PPP(ICP11) in million USD05 equivalents
#' @author Lavinia Baumstark, Benjamin Bodirsky, Johannes Koch

calcGDPpppFuture <- function(GDPpppFuture = "SSP_bezierOut_completed") {
  
  # Look for "_completed" tag. Remove if found.
  if (grepl("_completed", GDPpppFuture)) {
    GDPpppFuture <- gsub("_completed", "", GDPpppFuture)
    complete <- TRUE
  } else {
    complete <- FALSE
  }

  # Look for "_bezierOut" tag. Remove if found.
  if (grepl("_bezierOut", GDPpppFuture)) {
    GDPpppFuture <- gsub("_bezierOut", "", GDPpppFuture)
    bezierOut <- TRUE
  } else {
    bezierOut <- FALSE
  }

  # Check input argument
  valid_inputs <- c(
    "OECD", 
    "SSP",
    "SRES",
    "SSP2Ariadne"
  )
  if (!GDPpppFuture %in% valid_inputs) {
    stop("Bad input for calcGDPpppFuture. Invalid 'GDPpppFuture' argument.")
  }

  data <- switch(GDPpppFuture,
                 "OECD" = calcGDPpppFutureOECD(),
                 "SSP" = calcGDPpppFutureSSP(complete, bezierOut),
                 "SRES" = calcGDPpppFutureSRES(complete),
                 "SSP2Ariadne" = calcGDPpppFutureSSP2Ariadne(complete, bezierOut))

  # Clean and put in alphabetical order (necessary?)
  data <- clean_magpie(data)
  data <- data[,,order(getNames(data))]

  return(list(x = data,
              weight = NULL,
              unit = "Million US Dollar 2005 equivalents in PPP",
              description = "US Dollar 2005 equivalents in PPP"))
}



######################################################################################
# Functions
######################################################################################
calcGDPpppFutureOECD <- function() {
  data <- readSource("OECD", subtype = "gdp") * 1000
  time_extend <- seq(2105, 2150, 5)
  data <- time_interpolate(data,
                           time_extend, 
                           extrapolation_type = "constant", 
                           integrate_interpolated_years = TRUE)
}

calcGDPpppFutureSSP <- function(complete, bezierOut) {
  data <- readSource("SSP", subtype="all")[,,"GDP|PPP"][,,"OECD Env-Growth"] * 1000
  
  # Refactor names
  data <- collapseNames(data) 
  getNames(data) <- paste0("gdp_", gsub("_v[[:alnum:],[:punct:]]*", "", getNames(data)))
  set_names_for_later <- getSets(data)
  
  # Remove 2000 and 2005, because these years are not complete
  data <- data[,setdiff(getYears(data), c("y2000", "y2005")),]
  
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

  if (complete) {
    fill <- readSource("MissingIslands", subtype = "gdp", convert = FALSE)
    data <- completeData(data, fill)
  }
  data
}



calcGDPpppFutureSRES <- function(complete) {
  vcat(1, "growth rates of SRES projections were multiplied on 1990 GDP of James et al")
  data <- NULL
  for (i in c("sres_a1_gdp", "sres_a2_gdp", "sres_b1_gdp", "sres_b2_gdp")) {
    data <- mbind(data, readSource(type = "SRES", subtype=i))
  }
  getNames(data) <- paste0("gdp_",substr(getNames(data),6,7))
  PPP_pc <- readSource(type = "James", subtype = "IHME_USD05_PPP_pc")
  pop <- readSource("WDI", subtype = "SP.POP.TOTL")
  years <- intersect(getYears(PPP_pc),getYears(pop))
  calib <- PPP_pc[,years,]*pop[,years,]
  getNames(calib) <- "IHME_USD05_PPP"
  data <- data*setYears(setNames(calib[,"y1990",],NULL)/data[,"y1990",],NULL)
  time_extend <- seq(2105, 2150, 5)
  data <- time_interpolate(data,time_extend, 
                           extrapolation_type = "constant", 
                           integrate_interpolated_years = TRUE)
  data[is.na(data)]<-0

  if (complete) {
    fill <- calcOutput("GDPpppFuture", GDPpppFuture = "SSP_completed", aggregate = F)[,,"gdp_SSP2"]
    data <- completeData(data, fill)
  }
  data
}


calcGDPpppFutureSSP2Ariadne <- function(complete, bezierOut) {
  data_ariadne <- readSource("ARIADNE_ReferenceScenario", "gdp_corona")
  data_ssp <- calcGDPpppFutureSSP(FALSE, FALSE)

  # Get countries for which ARIADNE/Eurostat GDP projections exist.) 
  EUR_countries <- where(data_ariadne != 0 )$true$regions

  # Get common years
  cy <- intersect(getYears(data_ssp),  getYears(data_ariadne))

  # Start with the SSP2 scenario until 2100. Change the name, and overwrite the EUR
  # countries with the Eurostat data.
  data <- data_ssp[, getYears(data_ssp, as.integer = TRUE) <= 2100, "gdp_SSP2"] %>% 
    setNames("gdp_SSP2Ariadne")
  data[EUR_countries,,] <- 0
  data[EUR_countries, cy, ] <- data_ariadne[EUR_countries, cy,]

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
     fill <- readSource("MissingIslands", subtype = "gdp", convert = FALSE)
     data <- completeData(data, fill)
  }

  data
}
