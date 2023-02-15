#' @describeIn calcGDPPast Get future GDP projections
#'
#' @param GDPFuture A string designating the source for the future GDP data. Available sources are:
#'   \itemize{
#'     \item "SSPs": IIASA [SSP database](https://tntcat.iiasa.ac.at/SspDb/dsd?Action=htmlpage&page=welcome)
#'     \item "SSP2EU": Combined SSP2 and Eurostat (for the EU countries) source
#'     \item "SDPs":
#'     \item "MI": Missing island dataset
#'     \item "OECD": OECD
#'   }
#'   See the "Combining data sources with '-'" section below for how to combine data sources.
#'
#' @examples \dontrun{
#' library(mrdrivers)
#' calcOutput("GDPFuture")
#' }
calcGDPFuture <- function(GDPFuture = "SSPs-MI", unit = "constant 2005 Int$PPP") { # nolint
  # Check user input
  toolCheckUserInput("GDPFuture", as.list(environment()))
  # Call calcInternalGDPFuture function the appropriate number of times (map) and combine (reduce)
  # !! Keep formula syntax for madrat caching to work
  purrr::pmap(list("GDPFuture" = unlist(strsplit(GDPFuture, "-")), "unit" = unit),
              ~calcOutput("InternalGDPFuture", aggregate = FALSE, supplementary = TRUE, ...)) %>%
    toolReduce(mbindOrFillWith = "fillWith")
}


######################################################################################
# Internal Function
######################################################################################
calcInternalGDPFuture <- function(GDPFuture, unit) { # nolint
  data <- switch(
    GDPFuture,
    "SSPs"   = calcOutput("InternalGDPFutureSSPs", unit = unit, aggregate = FALSE),
    "SSP2EU" = calcOutput("InternalGDPFutureSSP2EU", unit = unit, aggregate = FALSE),
    "SDPs"   = calcOutput("InternalGDPFutureSDPs", unit = unit, aggregate = FALSE),
    "MI"     = calcOutput("InternalGDPMI", unit = unit, aggregate = FALSE),
    # Deprecated options ?
    "OECD"   = readSource("OECD", subtype = "gdp") * 1000,
    stop("Bad input for calcGDPFuture. Invalid 'GDPFuture' argument.")
  )

  data <- toolFinishingTouches(data)

  list(x = data, weight = NULL, unit = glue("mil. {unit}"), description = glue("GDP data from {GDPFuture}"))
}



######################################################################################
# Functions
######################################################################################
calcInternalGDPFutureSSPs <- function(unit) {
  data <- readSource("SSP", subtype = "gdp") * 1000

  # Refactor names
  data <- collapseNames(data)
  getSets(data)[3] <- "variable"
  getNames(data) <- paste0("gdp_", gsub("_v[[:alnum:],[:punct:]]*", "", getNames(data)))

  # GDPFutureSSP is constructed in PPPs.
  if (grepl("^constant .* US\\$MER$", unit)) {
    constructUnit <- paste0("constant ",  substr(unit, 10, 13), " Int$PPP")
  } else {
    constructUnit <- unit
  }

  # The default construct unit is "constant 2005 Int$PPP". If another unit is
  # demanded, then some modifications have to be done.
  if (constructUnit != "constant 2005 Int$PPP") {
    # Construct SSP pathways in constant YYYY Int$PPP.
    # For the near future, convert using current conversion factors.
    # After that the scenarios are built by converting the US GDP, and building
    # the other countries in relation to the US so that by 2100, they have the
    # same ratio as in 2005 Int$PPP.
    data2005PPP <- data

    # The near future is defined hear by the next 15 years, or until 10 years after the last
    # imf prediction.
    c15 <- max(getYears(readSource("IMF", "GDPpc"), as.integer = TRUE)) + 10

    y1 <- getYears(data2005PPP)[getYears(data2005PPP, as.integer = TRUE) <= c15]
    dataNearFut <- data2005PPP[, y1, ] %>%
      GDPuc::convertGDP("constant 2005 Int$PPP", unit, replace_NAs = c("linear", "no_conversion"))

    y2 <- getYears(data2005PPP)[getYears(data2005PPP, as.integer = TRUE) > c15 &
                                 getYears(data2005PPP, as.integer = TRUE) < 2100]
    dataFarFut <- data2005PPP[, y2, ] * NA

    # Convert to 2017 Int$PPP using the 2017 value of base 2005 GDP deflator
    # (in constant 2017 LCU per constant 2005 LCU) of the USA
    # LONGTERM: allow other PPP units
    data2100 <- data2005PPP[, 2100, ] * 1.23304244543521

    data2017PPP <- mbind(dataNearFut, dataFarFut, data2100)

    q <- data2005PPP / data2017PPP
    # For interpolation to work, the last and first values have to be non-NA/non-NaN
    q[, 2100, ][is.na(q[, 2100, ])] <- 0
    # The first 2 years of the SSP data set are incomplete. For countries that only lack data in these first 2 years,
    # set NaN to 0.
    q[, 2000, ][is.nan(q[, 2000, ]) & !is.nan(q[, 2010, ])] <- 0

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

  # If unit was in $MER
  if (constructUnit != unit) {
     data <- GDPuc::convertGDP(data, constructUnit, unit, replace_NAs = c("linear", "no_conversion"))
  }

  list(x = data, weight = NULL, unit = unit, description = "GDP data from SSPs")
}

calcInternalGDPFutureSDPs <- function(unit) {
  dataSSP1 <- calcOutput("InternalGDPFutureSSPs", unit = unit, aggregate = FALSE)[, , "gdp_SSP1"] # nolint

  data <- purrr::map(c("SDP", "SDP_EI", "SDP_RC", "SDP_MC"),
                     ~ setNames(dataSSP1, gsub("SSP1", .x, getNames(dataSSP1)))) %>%
    mbind()

  list(x = data, weight = NULL, unit = unit, description = "GDP data from SDPs")
}

calcInternalGDPFutureSSP2EU <- function(unit) {
  dataSSP2EU <- readSource("ARIADNE", "gdp_corona") %>%
      GDPuc::convertGDP("constant 2005 Int$PPP", unit, replace_NAs = c("linear", "no_conversion"))
  dataSSP <- calcOutput("InternalGDPFutureSSPs", unit = unit, aggregate = FALSE)

  # Get EU-27 countries
  euCountries <- toolGetEUcountries(onlyWithARIADNEgdpData = TRUE)

  # Get common years
  cy <- intersect(getYears(dataSSP),  getYears(dataSSP2EU))

  # Start with the SSP2 scenario until 2100. Change the name, and overwrite the EUR
  # countries with the Eurostat data.
  data <- dataSSP[, , "gdp_SSP2"] %>% setNames("gdp_SSP2EU")
  data[euCountries, , ] <- 0
  data[euCountries, cy, ] <- dataSSP2EU[euCountries, cy, ]
  list(x = data, weight = NULL, unit = unit, description = "GDP data from ARIADNE")
}

calcInternalGDPMI <- function(unit) {
  data <- readSource("MissingIslands", "gdp") %>%
    GDPuc::convertGDP("constant 2005 Int$PPP", unit, replace_NAs = c("linear", "no_conversion"))
  list(x = data, weight = NULL, unit = unit, description = "GDP data from MI")
}
