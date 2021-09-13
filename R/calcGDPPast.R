#' calcGDPPast
#'
#' Calculates a time series of GDP in Purchase Power Parity (PPP) of million
#' International Dollars of the year 2005. Different sources are available:
#' \itemize{ \item \code{WDI}: The PPP estimate from the World Development
#' Indicators (WDI) are supplemented by values for Argentina, Syria and Somalia
#' which are missing in the database. The values were taken from World Bank.
#' 2014. Purchasing Power Parities and the Real Size of World Economies: A
#' Comprehensive Report of the 2011 International Comparison Program. The World
#' Bank. http://elibrary.worldbank.org/doi/book/10.1596/978-1-4648-0329-1.
#' table 2.13 Then, the 2011 estimate is extrapolated with the GDP projection
#' in local currency units (LCU), as these growth rates should be approximately
#' what the growth models predict. The price index from 2011 was transformed
#' into 2005 equivalents using the inflation rate of the United States (US), as
#' the PPPs are in USDollar equvialents.
#' \item \code{PWT}: Penn World Tables
#' \item \code{IHME_USD05_PPP_pc}: Publication: James, Spencer L., Paul Gubbins,
#' Christopher JL Murray, and Emmanuela Gakidou. 2012. "Developing a
#' Comprehensive Time Series of GDP per Capita for 210 Countries from 1950 to
#' 2015."" Population Health Metrics 10 (1): 12. doi:10.1186/1478-7954-10-12.
#' This publication also contains further interpolated indicators that can be
#' used. }
#'
#' @inheritParams calcGDP
#' @inherit calcGDP return
#' 
#' @seealso [madrat::calcOutput()
#' @family GDP functions
#'
#' @examples \dontrun{
#' library(mrdrivers)
#' calcOutput("GDPPast")}
#'
calcGDPPast <- function(GDPPast = "WDI",
                        unit = "constant 2005 Int$PPP",
                        useMIData = TRUE) {

  # Check input argument
  valid_inputs <- c(
    "WDI",
    "Eurostat_WDI",
    "IHME_USD05_PPP_pc",
    "IHME_USD05_MER_pc",
    "IMF_USD05_PPP_pc",
    "PENN_USD05_PPP_pc",
    "WB_USD05_PPP_pc",
    "MADDISON_USD05_PPP_pc",
    "WB_USD05_MER_pc",
    "IMF_USD05_MER_pc",
    "UN_USD05_MER_pc",
    "PWT"
  )
  if (!GDPPast %in% valid_inputs) {
    stop("Bad input for calcGDPPast. Invalid 'GDPPast' argument.")
  }

  # Call appropriate calcGDPPast function.
  data <- switch(GDPPast,
                 "PWT"          = cGDPPastPWT(),
                 "WDI"          = cGDPPastWDI(),
                 "Eurostat_WDI" = cGDPPastEurostatWDI(),
                 cGDPPastJames(GDPPast))

  if (useMIData) {
    fill <- readSource("MissingIslands", subtype = "gdp", convert = FALSE)
    data <- completeData(data, fill)
  }

  list(x = data, weight = NULL, unit = unit, description = glue("GDP data from {GDPPast}."))
}




######################################################################################
# Functions
######################################################################################
cGDPPastWDI <- function() {
  # "NY.GDP.MKTP.PP.KD" = GDP in constant 2017 Int$PPP (as of time of writing this function)
  data <- readSource("WDI", "NY.GDP.MKTP.PP.KD") %>%
    GDPuc::convertGDP("constant 2017 Int$PPP", "constant 2005 Int$PPP")
  # TODO: Decide if use JAMES 2019 data for NA countries, e.g. DJI (as is now) or
  # convert using regional averages and use JAMES 2019 growth rates
  data[is.na(data)] <- 0

  # There is no PPP data available before 1990, so we shall extrapolate back using constant LCU growth rates
  # data <- harmonizeFutureGrPast(past = readSource("WDI", "NY.GDP.MKTP.KN"),
  #                               future = data)

  # Use the James2019  WB_USD05_PPP_pc series to fill in past data.
  # Using growth rates, since conversion of James2019 data into 2005 Int$PPP not certain to be correct.
  gdppc <- readSource("James2019", "WB_USD05_PPP_pc")
  pop <- readSource("WDI", "SP.POP.TOTL")
  cy <- intersect(getYears(gdppc), getYears(pop))
  past_gdp <- gdppc[, cy,] * pop[, cy,]
  getSets(past_gdp) <- c("iso3c", "year", "data")
  getNames(past_gdp) <- "WB_USD05_PPP"

  x <- new.magpie(getRegions(data), getYears(past_gdp), getNames(data), fill = 0)
  for (i in getRegions(data)) {
    tmp <- data[i, getYears(data)[data[i,,] != 0], ]
    ihme_data <- past_gdp[i, getYears(past_gdp)[past_gdp[i,,] != 0], ]

    if (length(tmp) == 0 && length(ihme_data) == 0) {
      next
    } else if (length(tmp) != 0 && length(ihme_data) == 0) {
      x[i, getYears(tmp),] <- tmp
    } else if (length(tmp) == 0 && length(ihme_data) != 0) {
      x[i, getYears(past_gdp),] <- toolFillYears(ihme_data, getYears(past_gdp))
    } else {
      r <- harmonizeFutureGrPast(past = toolFillYears(ihme_data, getYears(past_gdp)),
                                 future = tmp)
      x[i, getYears(r),] <- r
    }
  }
  data <- x

  getNames(data) <- "gdp in constant 2005 Int$PPP"
  data
}

cGDPPastEurostatWDI <- function() {
  data_eurostat <- readSource("Eurostat", "GDP")
  data_wdi <- readSource("WDI", "NY.GDP.MKTP.PP.KD") %>%
    GDPuc::convertGDP("constant 2017 Int$PPP", "constant 2005 Int$PPP")
  data_wdi[is.na(data_wdi)] <- 0

  # Get EUR countries.
  EUR_countries <- toolGetMapping("regionmappingH12.csv") %>%
    tibble::as_tibble() %>%
    dplyr::filter(.data$RegionCode == "EUR") %>%
    dplyr::pull(.data$CountryCode)

  # Use WDI data for everything but the EUR_countries. Use Eurostat stat for those.
  data <- data_wdi
  cy <- intersect(getYears(data_wdi),  getYears(data_eurostat))
  data[EUR_countries, cy,] <- data_eurostat[EUR_countries, cy,]

  # There is no PPP data available before 1990, so we shall extrapolate back using constant LCU growth rates
  data <- harmonizeFutureGrPast(past = readSource("WDI", "NY.GDP.MKTP.KN"),
                                future = data)

  getNames(data) <- "gdp in constant 2005 Int$PPP"
  data
}

cGDPPastJames <- function(type) {
  PPP_pc <- readSource(type = "James", subtype = type)
  pop <- readSource("WDI", subtype = "SP.POP.TOTL")
  years <- intersect(getYears(PPP_pc), getYears(pop))
  data <- PPP_pc[,years,] * pop[,years,]
  getNames(data) <- substr(type, 1, (nchar(type) - 3))
  getNames(data) <- "gdp" #??

  data
}


######################################################################################
# Legacy
######################################################################################
cGDPPastPWT <- function() {
  data <- readSource("PWT")[,,"rgdpna"]
  getNames(data) <- "GDP_PWT"
  data
}


######################################################################################
# GDPpast Harmonization Functions
######################################################################################
harmonizeFutureGrPast <- function(past, future) {
  firstFutureYear <- min(intersect(getYears(past, as.integer = TRUE),
                                   getYears(future, as.integer = TRUE)))
  lastPastYear <- max(getYears(past, as.integer = TRUE))
  if (lastPastYear < firstFutureYear) {
    stop("The past and future data need to have some overlap")
  }

  # Create future data for all past scenarios
  years_future <- getYears(future)[which(getYears(future, as.integer = TRUE) >= firstFutureYear)]
  tmpFuture <- future[, years_future, rep(1, ndata(past))]
  tmpFuture <- setNames(tmpFuture, getNames(past))
  tmpFuture[is.nan(tmpFuture)] <- 0

  # Create transition magpie object for all future scenarios
  years_past <- getYears(past)[which(getYears(past, as.integer = TRUE) < firstFutureYear)]
  tmpPast <- new.magpie(getRegions(past), years_past, getNames(past), fill = 0)

  # Use growth rates of future object
  tmpPast[,,] <- tmpFuture[,firstFutureYear,] * past[,years_past,] / past[,firstFutureYear,]
  tmpPast[is.nan(tmpPast)] <- 0

  mbind(tmpPast, tmpFuture)
}
