#' @rdname calcGDP
#' @examples \dontrun{
#' calcOutput("GDPpc")
#' }
#'
calcGDPpc <- function(scenario = c("SSPs", "SDPs", "SSP2EU"),
                      unit = "constant 2017 Int$PPP",
                      average2020 = TRUE,
                      ...) {
  # Check user input
  toolCheckUserInput(driver = "GDPpc", args = c(list(...), as.list(environment())))

  # GDPpc scenarios are constructed in 2017 Int$PPP, and converted, if necessary, at the end.
  gdppc <- calcOutput("Driver",
                      driver = "GDPpc",
                      scenario = scenario,
                      unit = "constant 2017 Int$PPP",
                      popAsWeight = TRUE,
                      aggregate = FALSE,
                      supplementary = TRUE,
                      ...)

  if (average2020) {
    # For REMIND, the consensus is to average the 2020 value so as to dampen the effect of the COVID shock. (The
    # reasoning being that REMIND uses 5-year time steps, and that the year-in-itself should represent the 2,5 years
    # before and after.)
    # The dampening is supposed to take place on GDP. So for GDP per capita in 2020 to be consistent with the dampened
    # GDP, it has to calculated from GDP and population. (In other words we can't just use the same formula as for GDP,
    # since it would lead to inconsistency at the end.) This is a bit hacky...
    gdp2020 <- calcOutput("GDP",
                          scenario = scenario,
                          unit = "constant 2017 Int$PPP",
                          average2020 = TRUE,
                          naming = "scenario",
                          extension2150 = "none",
                          aggregate = FALSE,
                          years = 2020)
    pop2020 <- calcOutput("Population",
                          scenario = scenario,
                          naming = "scenario",
                          extension2150 = "none",
                          aggregate = FALSE,
                          years = 2020)
    gdppc2020 <- gdp2020 / pop2020
    gdppc2020[is.nan(gdppc2020)] <- 0
    getNames(gdppc2020) <- getNames(gdppc$x)
    gdppc$x[, 2020, ] <- gdppc2020
    gdppc$description <- paste(gdppc$description, "|| 2020 value averaged over 2018-2022 time period.")
    # Return only 5 year time steps, since the yearly data around 2020 is not connected to the 2020 value anymore.
    years5ts <- getYears(gdppc$x, as.integer = TRUE)[getYears(gdppc$x, as.integer = TRUE) %% 5 == 0 &
                                                     getYears(gdppc$x, as.integer = TRUE) != 1960]
    gdppc$x <- gdppc$x[, years5ts, ]
    gdppc$weight <- gdppc$weight[, years5ts, ]
    gdppc$description <- paste(gdppc$description, "5 year time steps only.")
    message("The 2020 value is an an avergae over the 2018-2022 time period!!")
  }

  # Convert to US$MER if required
  if (grepl("US\\$MER", unit)) {
    # Convert by interpolating and extrapolating missing conversion factors when possible.
    gdppc$x <- GDPuc::toolConvertGDP(gdppc$x,
                                 unit_in = "constant 2017 Int$PPP",
                                 unit_out = "constant 2017 US$MER",
                                 replace_NAs = c("linear", "no_conversion"))
  }
  # Temporary shifting to 2005 prices, using only the US deflator
  if (grepl("2005", unit)) gdppc$x <- gdppc$x * 0.8121123

  list(x = gdppc$x, weight = gdppc$weight, unit = unit, description = gdppc$description)
}
