#' calcGDPFuture
#'
#' @inheritParams calcGDP
#' @inherit calcGDP return
#' 
#' @seealso [madrat::calcOutput()
#' @family GDP functions
#'
#' @examples \dontrun{
#' library(mrdrivers)
#' calcOutput("GDPFuture")}
#'
calcGDPFuture <- function(GDPFuture = "SSPs",
                          unit = "constant 2005 Int$PPP",
                          useMIData = TRUE,
                          extension2150 = "none") {

  data <- switch(
    GDPFuture,
    "SSPs"        = cGDPFutureSSPs(),
    "SSP2Ariadne" = cGDPFutureSSP2Ariadne(),
    "SDPs"        = cGDPFutureSDPs(),
    # Deprecated options ?
    "OECD"        = readSource("OECD", subtype = "gdp") * 1000,
    "SRES"        = cGDPFutureSRES(),
    stop("Bad input for calcGDPFuture. Invalid 'GDPFuture' argument.")
  )

  # Fill in data with Missing Islands dataset, if opted for
  if (useMIData) {
    fill <- readSource("MissingIslands", subtype = "gdp", convert = FALSE)
    data <- completeData(data, fill)
  }

  data <- finishingTouches(data, extension2150)

  list(x = data, weight = NULL, unit = unit, description = glue("GDP data from {GDPFuture}"))
}



######################################################################################
# Functions
######################################################################################
cGDPFutureSSPs <- function() {
  data <- readSource("SSP", subtype = "all")[,,"GDP|PPP"][,,"OECD Env-Growth"] * 1000

  # Refactor names
  data <- collapseNames(data)
  getSets(data)[3] <- "variable"
  getNames(data) <- paste0("gdp_", gsub("_v[[:alnum:],[:punct:]]*", "", getNames(data)))

  # Remove 2000 and 2005, because these years are not complete
  data <- data[,setdiff(getYears(data), c("y2000", "y2005")),]
}

cGDPFutureSDPs <- function() {
  data_SSP1 <- cGDPFutureSSPs()[,, "gdp_SSP1"]

  data <- purrr::map(c("SDP", "SDP_EI", "SDP_RC", "SDP_MC"),
                     ~ setNames(data_SSP1, gsub("SSP1", .x, getNames(data_SSP1)))) %>%
    purrr::reduce(mbind)
}

cGDPFutureSSP2Ariadne <- function() {
  data_ariadne <- readSource("ARIADNE", "gdp_corona")
  data_ssp <- cGDPFutureSSPs()

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
  data
}

######################################################################################
# Legacy
######################################################################################
cGDPFutureSRES <- function() {
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

  data[is.na(data)]<-0


  fill <- calcOutput("GDPFuture",
                     GDPFuture = "SSPs",
                     useMIData = FALSE,
                     extension2150 = "none",
                     aggregate = FALSE)[,,"gdp_SSP2"]
  data <- completeData(data, fill)
}

