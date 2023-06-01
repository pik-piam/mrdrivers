#' @rdname calcGDP
#' @examples \dontrun{
#' calcOutput("GDPpc")
#' }
#'
calcGDPpc <- function(scenario = c("SSPs", "SDPs", "SSP2EU"),
                      unit = "constant 2005 Int$PPP",
                      average2020 = TRUE,
                      ...) {
  # Check user input
  toolCheckUserInput(driver = "GDPpc", args = c(list(...), as.list(environment())))

  # GDPpc scenarios are constructed in PPPs. If MERs are desired, scenarios with the
  # same base year but in PPPs are constructed, and converted to MERs at the end.
  constructUnit <- unit
  if (grepl("^constant .* US\\$MER$", unit)) {
    constructUnit <- paste0("constant ",  substr(unit, 10, 13), " Int$PPP")
  }

  gdppc <- calcOutput("Driver",
                      driver = "GDPpc",
                      scenario = scenario,
                      unit = constructUnit,
                      popAsWeight = TRUE,
                      aggregate = FALSE,
                      supplementary = TRUE,
                      ...)

  if (average2020 && any(grepl("SSPsOld", scenario))) {
    warning("Average 2020 is not compatible with SSPsOld. Setting to FALSE.")
    average2020 <- FALSE
  }
  if (average2020) {
    # For REMIND, the concensus is to avergae the 2020 value so as to dampen the effect of the COVID shock. (The
    # reasoning being that REMIND uses 5-year time steps, and that the year-in-itself should represent the 2,5 years
    # before and after.)
    # The dampening is supposed to take place on GDP. So for GDP per capita in 2020 to be consistent with the dampened
    # GDP, it has to calculated from GDP and population. (In other words we can't just use the same formula as for GDP,
    # since it would lead to inconsistency at the end.) This is a bit hacky...
    gdp2020 <- calcOutput("GDP",
                          scenario = scenario,
                          unit = constructUnit,
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

  if (constructUnit != unit) {
    # Convert by interpolating and extrapolating missing conversion factors when possible.
    gdppc$x <- GDPuc::convertGDP(gdppc$x, constructUnit, unit, replace_NAs = c("linear", "no_conversion"))
  }

  list(x = gdppc$x, weight = gdppc$weight, unit = unit, description = gdppc$description)
}
