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
    "SSPs"   = cGDPFutureSSPs(unit),
    "SSP2EU" = cGDPFutureSSP2EU(unit),
    "SDPs"   = cGDPFutureSDPs(unit),
    # Deprecated options ?
    "OECD"   = readSource("OECD", subtype = "gdp") * 1000,
    "SRES"   = cGDPFutureSRES(),
    stop("Bad input for calcGDPFuture. Invalid 'GDPFuture' argument.")
  )

  # Fill in data with Missing Islands dataset, if opted for
  if (useMIData) {
    fill <- readSource("MissingIslands", subtype = "gdp", convert = FALSE) %>%
      GDPuc::convertGDP("constant 2005 Int$PPP", unit, replace_NAs = 1)
    data <- completeData(data, fill)
  }

  data <- finishingTouches(data, extension2150)

  list(x = data, weight = NULL, unit = unit, description = glue("GDP data from {GDPFuture}"))
}



######################################################################################
# Functions
######################################################################################
cGDPFutureSSPs <- function(unit) {
  data <- readSource("SSP", subtype = "all")[,,"GDP|PPP"][,,"OECD Env-Growth"] * 1000

  # Refactor names
  data <- collapseNames(data)
  getSets(data)[3] <- "variable"
  getNames(data) <- paste0("gdp_", gsub("_v[[:alnum:],[:punct:]]*", "", getNames(data)))

  # Remove 2000 and 2005, because these years are not complete
  data <- data[,setdiff(getYears(data), c("y2000", "y2005")),]

  if (unit == "constant 2017 Int$PPP") {
    # Construct SSP pathways in constant 2017 Int$PPP, by converting the US GDP, and 
    # building the other countries in relation to the US. Same ratio as in 2005 Int$PPP.
    data_2005PPP <- data
        
    y1 <- getYears(data_2005PPP)[getYears(data_2005PPP, as.integer = TRUE) <= 2035]
    data_pre2035 <- data_2005PPP[, y1,] %>% 
      GDPuc::convertGDP("constant 2005 Int$PPP", unit, replace_NAs = 0)
  
    y2 <- getYears(data_2005PPP)[getYears(data_2005PPP, as.integer = TRUE) > 2035 & 
                                 getYears(data_2005PPP, as.integer = TRUE) < 2100]
    data_between2035and2100 <- data_2005PPP[, y2,] * NA
      
    # Convert to 2017 Int$PPP using the 2017 value of base 2005 GDP deflator (in constant 2017 LCU per constant 2005 LCU) of the USA
    data_2100 <- data_2005PPP[, 2100, ] * 1.23304244543521
      
    data_2017PPP <- mbind(data_pre2035, data_between2035and2100, data_2100)
      
    q <- data_2005PPP / data_2017PPP
    q[, 2100,][is.na(q[, 2100,])] <- 0
  
    q <- as.data.frame(q, rev = 2) %>% 
      dplyr::rename("value" = ".value") %>% 
      dplyr::arrange(.data$year) %>% 
      dplyr::group_by(.data$iso3c, .data$variable) %>%
      dplyr::mutate(value = zoo::na.approx(.data$value)) %>% 
      dplyr::ungroup() %>% 
      as.magpie(tidy = TRUE)
    
    data_2017PPP <- data_2005PPP / q
    data_2017PPP[is.na(data_2017PPP)] <- data_2005PPP[is.na(data_2017PPP)]
    # Above should probably be "<- 0"
    ##################
    data <- data_2017PPP
  }

  data
}

cGDPFutureSDPs <- function(unit) {
  data_SSP1 <- cGDPFutureSSPs(unit)[,, "gdp_SSP1"]

  data <- purrr::map(c("SDP", "SDP_EI", "SDP_RC", "SDP_MC"),
                     ~ setNames(data_SSP1, gsub("SSP1", .x, getNames(data_SSP1)))) %>%
    purrr::reduce(mbind)
}

cGDPFutureSSP2EU <- function(unit) {
  data_SSP2EU <- readSource("ARIADNE", "gdp_corona") %>% 
      GDPuc::convertGDP("constant 2005 Int$PPP", unit, replace_NAs = 0)
  data_ssp <- cGDPFutureSSPs(unit)

  # Get countries for which Eurostat GDP projections exist.)
  EUR_countries <- where(data_SSP2EU != 0 )$true$regions

  # Get common years
  cy <- intersect(getYears(data_ssp),  getYears(data_SSP2EU))

  # Start with the SSP2 scenario until 2100. Change the name, and overwrite the EUR
  # countries with the Eurostat data.
  data <- data_ssp[,, "gdp_SSP2"] %>% setNames("gdp_SSP2EU")
  data[EUR_countries,,] <- 0
  data[EUR_countries, cy, ] <- data_SSP2EU[EUR_countries, cy,]
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

