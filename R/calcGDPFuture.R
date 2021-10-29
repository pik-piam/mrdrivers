#' calcGDPFuture
#'
#' @inheritParams calcGDP
#' @inherit calcGDP return
#'
#' @seealso [madrat::calcOutput()]
#' @family GDP functions
#'
#' @examples \dontrun{
#' library(mrdrivers)
#' calcOutput("GDPFuture")
#' }
#'
calcGDPFuture <- function(GDPFuture = "SSPs-MI",
                          unit = "constant 2005 Int$PPP",
                          extension2150 = "none") {
  # Call internalCalcGDPFuture function the appropriate number of times
  toolInternalCalc("GDPFuture",
                   list("GDPFuture" = strsplit(GDPFuture, "-")[[1]],
                        "unit" = unit,
                        "extension2150" = extension2150),
                   mbindOrFillWith = "fillWith")
}


######################################################################################
# Internal Function
######################################################################################
internalCalcGDPFuture <- function(GDPFuture, unit, extension2150) {
  data <- switch(
    GDPFuture,
    "SSPs"   = cGDPFutureSSPs(unit),
    "SSP2EU" = cGDPFutureSSP2EU(unit),
    "SDPs"   = cGDPFutureSDPs(unit),
    "MI"     = cGDPMI(unit),
    # Deprecated options ?
    "OECD"   = readSource("OECD", subtype = "gdp") * 1000,
    "SRES"   = cGDPFutureSRES(),
    stop("Bad input for calcGDPFuture. Invalid 'GDPFuture' argument.")
  )

  data <- toolFinishingTouches(data, extension2150)

  list(x = data, weight = NULL, unit = unit, description = glue("GDP data from {GDPFuture}"))
}



######################################################################################
# Functions
######################################################################################
cGDPFutureSSPs <- function(unit) {
  data <- readSource("SSP", subtype = "gdp") * 1000

  # Refactor names
  data <- collapseNames(data)
  getSets(data)[3] <- "variable"
  getNames(data) <- paste0("gdp_", gsub("_v[[:alnum:],[:punct:]]*", "", getNames(data)))

  # Remove 2000 and 2005, because these years are not complete
  data <- data[, setdiff(getYears(data), c("y2000", "y2005")), ]

  # GDPFutureSSP is constructed in PPPs.
  if (grepl("^constant .* US\\$MER$", unit)) {
    construct_unit <- paste0("constant ",  substr(unit, 10, 13), " Int$PPP")
  } else {
    construct_unit <- unit
  }

  # The default construct unit is "constant 2005 Int$PPP". If another unit is
  # demanded, then some modifications have to be done.
  if (construct_unit != "constant 2005 Int$PPP") {
    # Construct SSP pathways in constant YYYY Int$PPP.
    # Until 2035, convert using current conversion factors.
    # After that the scenarios are built by converting the US GDP, and building
    # the other countries in relation to the US so that by 2100, they have the
    # same ratio as in 2005 Int$PPP.
    data2005PPP <- data

    y1 <- getYears(data2005PPP)[getYears(data2005PPP, as.integer = TRUE) <= 2035]
    dataPre2035 <- data2005PPP[, y1, ] %>%
      GDPuc::convertGDP("constant 2005 Int$PPP", unit, replace_NAs = 0)

    y2 <- getYears(data2005PPP)[getYears(data2005PPP, as.integer = TRUE) > 2035 &
                                 getYears(data2005PPP, as.integer = TRUE) < 2100]
    dataBetween2035and2100 <- data2005PPP[, y2, ] * NA

    # Convert to 2017 Int$PPP using the 2017 value of base 2005 GDP deflator
    # (in constant 2017 LCU per constant 2005 LCU) of the USA
    # LONGTERM: allow other PPP units
    data2100 <- data2005PPP[, 2100, ] * 1.23304244543521

    data2017PPP <- mbind(dataPre2035, dataBetween2035and2100, data2100)

    q <- data2005PPP / data2017PPP
    q[, 2100, ][is.na(q[, 2100, ])] <- 0

    q <- as.data.frame(q, rev = 2) %>%
      dplyr::rename("value" = ".value") %>%
      dplyr::arrange(.data$year) %>%
      dplyr::group_by(.data$iso3c, .data$variable) %>%
      dplyr::mutate(value = zoo::na.approx(.data$value)) %>%
      dplyr::ungroup() %>%
      as.magpie(tidy = TRUE)

    data2017PPP <- data2005PPP / q
    data2017PPP[is.na(data2017PPP)] <- data2005PPP[is.na(data2017PPP)]
    # Above should probably be "<- 0"
    ##################
    data <- data2017PPP
  }

  if (construct_unit != unit) {
     data <- GDPuc::convertGDP(data, construct_unit, unit, replace_NAs = 0)
  }


  data
}

cGDPFutureSDPs <- function(unit) {
  dataSSP1 <- cGDPFutureSSPs(unit)[, , "gdp_SSP1"] # nolint

  purrr::map(c("SDP", "SDP_EI", "SDP_RC", "SDP_MC"),
                     ~ setNames(dataSSP1, gsub("SSP1", .x, getNames(dataSSP1)))) %>%
    mbind()
}

cGDPFutureSSP2EU <- function(unit) {
  dataSSP2EU <- readSource("ARIADNE", "gdp_corona") %>%
      GDPuc::convertGDP("constant 2005 Int$PPP", unit, replace_NAs = 0)
  dataSSP <- cGDPFutureSSPs(unit)

  # Get EU-27 countries
  euCountries <- toolGetEUcountries(onlyWithARIADNEgdpData = TRUE) # nolint

  # Get common years
  cy <- intersect(getYears(dataSSP),  getYears(dataSSP2EU))

  # Start with the SSP2 scenario until 2100. Change the name, and overwrite the EUR
  # countries with the Eurostat data.
  data <- dataSSP[, , "gdp_SSP2"] %>% setNames("gdp_SSP2EU")
  data[euCountries, , ] <- 0
  data[euCountries, cy, ] <- dataSSP2EU[euCountries, cy, ]
  data
}

cGDPMI <- function(unit) {
  readSource("MissingIslands", "gdp") %>%
    GDPuc::convertGDP("constant 2005 Int$PPP", unit, replace_NAs = 1)
}

######################################################################################
# Legacy
######################################################################################
cGDPFutureSRES <- function() {
  vcat(1, "growth rates of SRES projections were multiplied on 1990 GDP of James et al")
  data <- NULL
  for (i in c("sres_a1_gdp", "sres_a2_gdp", "sres_b1_gdp", "sres_b2_gdp")) {
    data <- mbind(data, readSource(type = "SRES", subtype = i))
  }
  getNames(data) <- paste0("gdp_", substr(getNames(data), 6, 7))
  PPPpc <- readSource(type = "James", subtype = "IHME_USD05_PPPpc")
  pop <- readSource("WDI", subtype = "SP.POP.TOTL")
  years <- intersect(getYears(PPPpc), getYears(pop))
  calib <- PPPpc[, years, ] * pop[, years, ]
  getNames(calib) <- "IHME_USD05_PPP"
  data <- data * setYears(setNames(calib[, "y1990", ], NULL) / data[, "y1990", ], NULL)

  data[is.na(data)] <- 0


  fill <- calcOutput("GDPFuture",
                     GDPFuture = "SSPs",
                     extension2150 = "none",
                     aggregate = FALSE)[, , "gdp_SSP2"]

  data %>%
    toolFillWith(fill) %>%
    toolInterpolateAndExtrapolate()
}
