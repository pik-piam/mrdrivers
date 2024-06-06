#' Get GDP and GDPpc scenario building blocks
#'
#' @description
#' Get the past and future GDP scenario building blocks with calcGDPPast and calcGDPFuture, respectively.
#' If GDP data for a scenario is required, even if just for a single year, always use [calcGDP()], as what is returned
#' by calcGDPPast or calcGDPFuture may not end up as is in the scenario, depending on the harmonization function.
#' Use calcGDPPast and calcGDPFuture only when trying to access specific GDP data.
#'
#' See the "Combining data sources with '-'" section below for how to combine data sources.
#'
#' @param GDPPast A string designating the source for the historical GDP data. Available sources are:
#'   \itemize{
#'     \item "WDI": World development indicators from the World Bank
#'     \item "MI": Missing island dataset
#'     \item "Eurostat": Eurostat
#'   }
#' @inheritParams calcGDP
#' @inheritSection calcScenarioConstructor Combining data sources with "-"
#' @keywords internal
calcGDPPast <- function(GDPPast = "WDI-MI", unit = "constant 2005 Int$PPP") { # nolint
  # Check user input
  toolCheckUserInput("GDPPast", as.list(environment()))
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
  # Call appropriate calcInternalGDPPast function.
  data <- switch(
    GDPPast,
    "WDI"      = calcOutput("InternalGDPPastWDI", unit = unit, aggregate = FALSE),
    "Eurostat" = calcOutput("InternalGDPPastEurostat", unit = unit, aggregate = FALSE),
    "MI"       = calcOutput("InternalGDPMI", unit = unit, aggregate = FALSE),
    stop("Bad input for calcGDPPast. Invalid 'GDPPast' argument.")
  )

  data <- toolFinishingTouches(data)

  list(x = data, weight = NULL, unit = glue("mil. {unit}"), description = glue("{GDPPast} data"))
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
  list(x = data, weight = NULL, unit = unit, description = "WDI data")
}

calcInternalGDPPastEurostat <- function(unit) {
  euCountries <- toolGetEUcountries()

  data <- readSource("EurostatPopGDP", "GDP")[euCountries, , ] %>%
    GDPuc::convertGDP("constant 2015 Int$PPP", unit, replace_NAs = c("linear", "no_conversion"))

  data <- fillWithWBFromJames2019(data, unit)
  data <- data %>% toolCountryFill(fill = 0) %>% suppressMessages()

  getNames(data) <- glue("gdp in {unit}")
  list(x = data, weight = NULL, unit = unit, description = "Eurostat data")
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
