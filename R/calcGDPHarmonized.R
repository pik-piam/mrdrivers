#' Get Harmonized GDP Data
#'
#' @inheritParams calcGDPpcHarmonized
#' @inherit madrat::calcOutput return
#' @keywords internal
calcGDPHarmonized <- function(harmonization, past, future, scenario, unit, ...) {
  harmonizedData <- switch(harmonization,
    "GDPpcWithPop"    = toolMultiplyGDPpcWithPop(scenario, unit),
    "calibSSP2EU"     = toolGDPHarmonizeSSP2EU(past, future, unit),
    stop(glue("Bad input for calcGDPHarmonized. Argument harmonization = '{harmonization}' is invalid. \\
              Possible values are: 'GDPpcWithPop', 'calibsSSP2EU' or 'past_transition'."))
  )
  list(x = harmonizedData$x, weight = NULL, unit = glue("mil. {unit}"), description = harmonizedData$description)
}


######################################################################################
# GDP Harmonization Functions
######################################################################################
toolMultiplyGDPpcWithPop <- function(scenario, unit) {
  gdppc <- calcOutput("GDPpc",
                      scenario = scenario,
                      unit = unit,
                      extension2150 = "none",
                      average2020 = FALSE,
                      aggregate = FALSE,
                      supplementary = TRUE)
  # GDP is equal to GDPpc * population
  gdp <- gdppc$x * gdppc$weight
  getNames(gdp) <- gsub("gdppc", "gdp", getNames(gdp))
  list(x = gdp,
       description = glue("use product of corresponding GDP per capita and population scenarios. \\
                           {gdppc$description}"))
}

toolGDPHarmonizeSSP2EU <- function(past, future, unit) {
  # We explicitly use the bezier Extension for SSP2 here, but only for harmonization purposes.
  # We return only up until 2100.
  ssp2Data <- calcOutput("GDP",
                         scenario = "SSP2",
                         unit = "constant 2017 Int$PPP",
                         extension2150 = "bezier",
                         average2020 = FALSE,
                         aggregate = FALSE)

  # For SSP2EU: EU countries are equal to past
  euCountries <- toolGetEUcountries()
  ssp2EUData <- ssp2Data[euCountries, , ]
  ssp2EUData[, , ] <- 0
  ssp2EUData[, getYears(past$x), ]   <- past$x[euCountries, , ]
  ssp2EUData[, getYears(future$x), ] <- future$x[euCountries, , ]

  # After last year of future, transition to SSP2 values by 2150
  pastYears <- getYears(ssp2EUData)[getYears(ssp2EUData, as.integer = TRUE) <= max(getYears(future$x, as.integer = TRUE))]
  combinedSSP2EU <- toolHarmonizePast(ssp2EUData[, pastYears, ],
                                      ssp2Data[euCountries, , ],
                                      method = "transition",
                                      yEnd = 2150)

  combined <- ssp2Data
  combined[euCountries, , ] <- 0
  combined[euCountries, getYears(combinedSSP2EU), ]  <- combinedSSP2EU[euCountries, , ]
  getNames(combined) <- "gdp_SSP2EU"

  combined <- combined[, getYears(combined)[getYears(combined, as.integer = TRUE) <= 2100], ]

  list(x = combined,
       description = glue("equal to SSP2 in all countries except for EU countries. \\
                          For EU countries use {past$description} until {max(getYears(past$x, as.integer = TRUE))}, \\
                          {future$description} until {max(getYears(future$x, as.integer = TRUE))}, \\
                          and converge to 2150 (bezier-extended) SSP2 values thereafter."))
}
