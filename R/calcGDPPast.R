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
#' the PPPs are in USDollar equivalents.
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
#' @seealso [madrat::calcOutput()]
#' @family GDP functions
#'
#' @examples \dontrun{
#' library(mrdrivers)
#' calcOutput("GDPPast")
#' }
#'
calcGDPPast <- function(GDPPast = "WDI-MI", unit = "constant 2005 Int$PPP") { # nolint
  # Check user input
  toolCheckUserInput("GDPpast", as.list(environment()))
  # Call calcInternalGDPPast function the appropriate number of times (map) and combine (reduce)
  # !! Keep formula syntax for madrat caching to work
  purrr::pmap(list("GDPPast" = unlist(strsplit(GDPPast, "-")), "unit" = unit),
              ~calcOutput("InternalGDPPast", aggregate = FALSE, supplementary = TRUE, ...)) %>%
    toolReduce(mbindOrFillWith = "fillWith")
}

######################################################################################
# Internal Function
######################################################################################
calcInternalGDPPast <- function(GDPPast, unit) { # nolint
  # Check input argument
  if (!GDPPast %in% c("WDI", "Eurostat", "MI", "PWT",
                      # All called through readJames
                      "IHME_USD05_PPP_pc", "IHME_USD05_MER_pc", "IMF_USD05_PPP_pc",
                      "PENN_USD05_PPP_pc", "WB_USD05_PPP_pc", "MADDISON_USD05_PPP_pc",
                      "WB_USD05_MER_pc", "IMF_USD05_MER_pc", "UN_USD05_MER_pc")) {
    stop("Bad input for calcGDPPast. Invalid 'GDPPast' argument.")
  }

  # Call appropriate calcInternalGDPPast function.
  data <- switch(GDPPast,
                 "PWT"      = calcOutput("InternalGDPPastPWT", aggregate = FALSE),
                 "WDI"      = calcOutput("InternalGDPPastWDI", unit = unit, aggregate = FALSE),
                 "Eurostat" = calcOutput("InternalGDPPastEurostat", unit = unit, aggregate = FALSE),
                 "MI"       = calcOutput("InternalGDPMI", unit = unit, aggregate = FALSE),
                 calcOutput("InternalGDPPastJames", GDPPast = GDPPast, aggregate = FALSE))

  data <- toolFinishingTouches(data)

  list(x = data, weight = NULL, unit = glue("mil. {unit}"), description = glue("GDP data from {GDPPast}."))
}




######################################################################################
# Functions
######################################################################################
calcInternalGDPPastWDI <- function(unit) {
  # "NY.GDP.MKTP.PP.KD" = GDP in constant 2017 Int$PPP (as of time of writing this function)
  data <- readSource("WDI", "NY.GDP.MKTP.PP.KD") %>%
     GDPuc::convertGDP("constant 2017 Int$PPP", unit, replace_NAs = c("linear", "no_conversion"))

  data <- fillWithWBFromJames2019(data, unit)

  getNames(data) <- glue("gdp in {unit}")
  list(x = data, weight = NULL, unit = unit, description = "GDP from WDI")
}

calcInternalGDPPastEurostat <- function(unit) {
  euCountries <- toolGetEUcountries()

  data <- readSource("EurostatPopGDP", "GDP") %>%
    GDPuc::convertGDP("constant 2005 Int$PPP", unit, replace_NAs = c("linear", "no_conversion")) %>%
    # Keep only EUR countries
    `[`(euCountries, , )

  data <- fillWithWBFromJames2019(data[euCountries, , ], unit)
  data <- data %>% toolCountryFill(fill = 0) %>% suppressMessages()

  getNames(data) <- glue("gdp in {unit}")
  list(x = data, weight = NULL, unit = unit, description = "GDP from Eurostat")
}

calcInternalGDPPastJames <- function(type) {
  gdpPPPpc <- readSource(type = "James", subtype = type)
  pop <- readSource("WDI", subtype = "SP.POP.TOTL")
  years <- intersect(getYears(gdpPPPpc), getYears(pop))
  data <- gdpPPPpc[, years, ] * pop[, years, ]
  getNames(data) <- substr(type, 1, (nchar(type) - 3))
  getNames(data) <- "gdp" # ??

  list(x = data, weight = NULL, unit = "constant 2005 Int$PPP", description = "GDP from IHME,James")
}

# Use the James2019  WB_USD05_PPP_pc series to fill in past data until 1960.
# Using mainly growth rates, since conversion of James2019 data into 2005 Int$PPP not certain to be correct.
fillWithWBFromJames2019 <- function(data, unit) {
  gdppc <- readSource("James2019", "WB_USD05_PPP_pc") %>%
    GDPuc::convertGDP("constant 2005 Int$PPP", unit, replace_NAs = c("linear", "no_conversion"))

  pop <- readSource("WDI", "SP.POP.TOTL")

  cy <- intersect(getYears(gdppc), getYears(pop))
  pastGDP <- gdppc[, cy, ] * pop[, cy, ]
  getSets(pastGDP) <- c("iso3c", "year", "variable")
  getNames(pastGDP) <- "WB_USD05_PPP"

  x <- new.magpie(getItems(data, 1),
                  1960:max(getYears(data, as.integer = TRUE)),
                  getNames(data),
                  fill = 0)
  getSets(x) <- getSets(pastGDP)
  for (i in getItems(data, 1)) {
    tmp <- data[i, getYears(data)[data[i, , ] != 0], ]
    ihmeData <- pastGDP[i, cy[pastGDP[i, , ] != 0], ]

    if (length(tmp) == 0 && length(ihmeData) == 0) {
      next
    } else if (length(tmp) != 0 && length(ihmeData) == 0) {
      x[i, getYears(tmp), ] <- tmp
    } else if (length(tmp) == 0 && length(ihmeData) != 0) {
      x[i, cy, ] <- toolFillYears(ihmeData, cy)
    } else {
      r <- toolHarmonizeFutureGrPast(past = toolFillYears(ihmeData, cy), future = tmp)
      x[i, getYears(r), ] <- r
    }
  }
  x
}



######################################################################################
# Legacy
######################################################################################
calcInternalGDPPastPWT <- function() {
  data <- readSource("PWT")[, , "rgdpna"]
  getNames(data) <- "GDP_PWT"
  list(x = data, weight = NULL, unit = "constant 2005 Int$PPP", description = "GDP from PWT")
}
