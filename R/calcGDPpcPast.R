#' @describeIn calcGDPPast Get historic GDPpc data
#'
#' @param GDPpcPast A string designating the source for the historical GDPpc data. Available sources are:
#'   \itemize{
#'     \item "WDI": World development indicators from the World Bank
#'     \item "MI": Missing island dataset
#'   }
#'   See the "Combining data sources with '-'" section below for how to combine data sources.
calcGDPpcPast <- function(GDPpcPast = "WDI-MI", unit = "constant 2005 Int$PPP") { # nolint
  # Call appropriate calcGDPPast function.
  data <- switch(GDPpcPast,
                 "WDI"    = cGDPpcFromGDPAndPop(GDPpcPast, unit),
                 "WDI-MI" = cGDPpcFromGDPAndPop(GDPpcPast, unit),
                 "MI"     = cGDPpcFromGDPAndPop(GDPpcPast, unit),
                 stop("Bad input for calcGDPpcPast. Invalid 'GDPpcPast' argument."))

  weight <- calcOutput("PopulationPast",
                       PopulationPast = GDPpcPast,
                       aggregate = FALSE)

  list(x = data, weight = weight, unit = unit, description = glue("GDPpc data from {GDPpcPast}."))
}

cGDPpcFromGDPAndPop <- function(GDPpcPast, unit) { # nolint
  gdp <- calcOutput("GDPPast", GDPPast = GDPpcPast, unit = unit, aggregate = FALSE)
  pop <- calcOutput("PopulationPast", PopulationPast = GDPpcPast, aggregate = FALSE)
  years <- intersect(getYears(gdp), getYears(pop))

  data <- gdp[, years, ] / pop[, years, ]
  data <- setNames(data, glue("gdppc_{GDPpcPast}"))
  data[is.nan(data) | data == Inf] <- 0
  data
}
