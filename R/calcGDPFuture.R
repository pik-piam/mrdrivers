#' @describeIn calcGDPPast Get future GDP projections
#'
#' @param GDPFuture A string designating the source for the future GDP data. Available sources are:
#'   \itemize{
#'     \item "SSPs": IIASA [SSP database](https://tntcat.iiasa.ac.at/SspDb/dsd?Action=htmlpage&page=welcome)
#'     \item "SSP2EU": Combined SSP2 and Eurostat (for the EU countries) source
#'     \item "SDPs":
#'     \item "MI": Missing island dataset
#'   }
#'   See the "Combining data sources with '-'" section below for how to combine data sources.
calcGDPFuture <- function(GDPFuture, unit) { # nolint
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
    "SSPs"   = calcOutput("InternalGDPFutureSSPs", unit = unit, aggregate = FALSE, supplementary = TRUE),
    "SSP2"   = calcOutput("InternalGDPFutureSSP2", unit = unit, aggregate = FALSE, supplementary = TRUE),
    "SSP2EU" = calcOutput("InternalGDPFutureSSP2EU", unit = unit, aggregate = FALSE, supplementary = TRUE),
    "SDPs"   = calcOutput("InternalGDPFutureSDPs", unit = unit, aggregate = FALSE, supplementary = TRUE),
    "MI"     = calcOutput("InternalGDPMI", unit = unit, aggregate = FALSE, supplementary = TRUE),
    stop("Bad input for calcGDPFuture. Invalid 'GDPFuture' argument.")
  )

  data$x <- toolFinishingTouches(data$x)
  data
}



######################################################################################
# Functions
######################################################################################
calcInternalGDPFutureSSPs <- function(unit) {
  # Read in gdp SSP projections (in billions) and convert to millions
  data <- readSource("SSP", subtype = "gdp") * 1000
  # Add gdp_ to variable dimension
  getNames(data) <- paste0("gdp_", getNames(data))

  # GDPFutureSSP is constructed in PPPs.
  if (grepl("^constant .* US\\$MER$", unit)) {
    constructUnit <- paste0("constant ",  substr(unit, 10, 13), " Int$PPP")
  } else {
    constructUnit <- unit
  }

  # THIS MAY BE DEPRECATED IN THE NEAR FUTURE
  # The default construct unit is "constant 2017 Int$PPP". If another unit is
  # demanded, then some modifications have to be done.
  if (constructUnit != "constant 2017 Int$PPP") {
    # Construct SSP pathways in constant YYYY Int$PPP.
    # For the near future, convert using current conversion factors.
    # After that the scenarios are built by converting the US GDP, and building
    # the other countries in relation to the US so that by 2100, they have the
    # same ratio as in 2017 Int$PPP.
    data2017PPP <- data

    # The near future is defined hear by the next 15 years, or until 10 years after the last imf prediction.
    c15 <- max(getYears(readSource("IMF", "GDPpc"), as.integer = TRUE)) + 10

    y1 <- getYears(data2017PPP)[getYears(data2017PPP, as.integer = TRUE) <= c15]
    dataNearFut <- data2017PPP[, y1, ] %>%
      GDPuc::convertGDP("constant 2017 Int$PPP", unit, replace_NAs = c("linear", "no_conversion"))

    y2 <- getYears(data2017PPP)[getYears(data2017PPP, as.integer = TRUE) > c15 &
                                  getYears(data2017PPP, as.integer = TRUE) < 2100]
    dataFarFut <- data2017PPP[, y2, ] * NA

    # Convert to 2005 Int$PPP using the 2005 value of base 2017 GDP deflator
    # (in constant 2005 LCU per constant 2017 LCU) of the USA
    # LONGTERM: allow other PPP units
    data2100 <- data2017PPP[, 2100, ] * GDPuc::convertSingle(1, "USA", "2010", "constant 2017 LCU", "constant 2005 LCU")

    data2005 <- mbind(dataNearFut, dataFarFut, data2100)

    ratio <- data2017PPP / data2005
    # For interpolation to work, the last and first values have to be non-NA/non-NaN
    ratio[, 2100, ][is.na(ratio[, 2100, ])] <- 0
    # The first 2 years of the SSP data set are incomplete. For countries that only lack data in these first 2 years,
    # set NaN to 0.
    ratio[, 2000, ][is.nan(ratio[, 2000, ]) & !is.nan(ratio[, 2010, ])] <- 0

    ratio <- as.data.frame(ratio, rev = 2) %>%
      dplyr::rename("value" = ".value") %>%
      dplyr::arrange(.data$year) %>%
      dplyr::group_by(.data$iso3c, .data$variable) %>%
      dplyr::mutate(value = zoo::na.approx(.data$value)) %>%
      dplyr::ungroup() %>%
      as.magpie(tidy = TRUE)

    data2005 <- data2017PPP / ratio
    data2005[is.na(data2005)] <- data2017PPP[is.na(data2005)]
    # Above should probably be "<- 0"
    ##################
    data <- data2005
  }

  # If unit was in $MER
  if (constructUnit != unit) {
    data <- GDPuc::convertGDP(data, constructUnit, unit, replace_NAs = c("linear", "no_conversion"))
  }

  list(x = data, weight = NULL, unit = glue("mil. {unit}"), description = "SSP projections")
}

calcInternalGDPFutureSSP2 <- function(unit) {
  data <- calcOutput("InternalGDPFutureSSPs", unit = unit, aggregate = FALSE)[, , "gdp_SSP2"]
  list(x = data, weight = NULL, unit = glue("mil. {unit}"), description = "SSP2 projections")
}

calcInternalGDPFutureSDPs <- function(unit) {
  dataSSP1 <- calcOutput("InternalGDPFutureSSPs", unit = unit, aggregate = FALSE)[, , "gdp_SSP1"] # nolint

  data <- purrr::map(c("SDP", "SDP_EI", "SDP_RC", "SDP_MC"),
                     ~ setNames(dataSSP1, gsub("SSP1", .x, getNames(dataSSP1)))) %>%
    mbind()

  list(x = data, weight = NULL, unit = glue("mil. {unit}"), description = "SDP projections")
}

calcInternalGDPFutureSSP2EU <- function(unit) {
  euCountries <- toolGetEUcountries()

  dataSSP2EU <- readSource("EurostatPopGDP", "GDP")[euCountries, , ] %>%
    GDPuc::convertGDP("constant 2015 Int$PPP", unit, replace_NAs = c("linear", "no_conversion"))
  grShort <- readSource("EurostatPopGDP", "GDPgr_projections_short")[euCountries, , ]
  grLong  <- readSource("EurostatPopGDP", "GDPgr_projections_long")[euCountries, , ]

  dataSSP2EU <- add_columns(dataSSP2EU, dim = 2, addnm = getYears(grShort)[!getYears(grShort) %in% getYears(dataSSP2EU)])
  dataSSP2EU <- add_columns(dataSSP2EU, dim = 2, addnm = getYears(grLong)[!getYears(grLong) %in% getYears(dataSSP2EU)])

  for (y in getYears(grShort, as.integer = TRUE)) {
    dataSSP2EU[, y, ] <- dataSSP2EU[, y - 1, ] * (1 + grShort[euCountries, y, ] / 100)
  }
  for (y in getYears(grLong, as.integer = TRUE)) {
    dataSSP2EU[, y, ] <- dataSSP2EU[, y - 1, ] * (1 + grLong[euCountries, y, ] / 100)
  }

  dataSSP2 <- calcOutput("InternalGDPFutureSSPs", unit = unit, aggregate = FALSE)[, , "gdp_SSP2"]

  # Start with the SSP2 scenario until 2100. Change the name, and overwrite the EUR
  # countries with the Eurostat data.
  cy <- intersect(getYears(dataSSP2), getYears(dataSSP2EU))
  data <- dataSSP2[, cy, ] %>% setNames("gdp_SSP2EU")
  data[euCountries, cy, ] <- dataSSP2EU[, cy, ]
  data[is.na(data)] <- 0
  list(x = data, weight = NULL, unit = glue("mil. {unit}"), description = "Eurostat projections")
}

calcInternalGDPMI <- function(unit) {
  data <- readSource("MissingIslands", "gdp") %>%
    GDPuc::convertGDP("constant 2005 Int$PPP", unit, replace_NAs = c("linear", "no_conversion"))
  list(x = data, weight = NULL, unit = glue("mil. {unit}"), description = "MI projections")
}
