#' Get Harmonized GDP Data
#'
#' @param args Arguments passed on to harmonization functions
#' @inherit madrat::calcOutput return
#' @keywords internal
calcGDPHarmonized <- function(args) {
  harmonizedData <- switch(args$harmonization,
    "GDPpcWithPop"    = toolMultiplyGDPpcWithPop(args$scenario, args$unit),
    "calibSSP2EU"     = toolGDPHarmonizeSSP2EU(args$past, args$future, args$unit),
    "past_transition" = toolHarmonizePastTransition(args$past$x, args$future$x, yEnd = 2050, aslist = TRUE),
    stop(glue("Bad input for calcGDPHarmonized. Argument harmonization = '{args$harmonization}' is invalid. \\
              Possible values are: 'GDPpcWithPop', 'calibsSSP2EU' or 'past_transition'."))
  )
  list(x = harmonizedData$x, weight = NULL, unit = glue("mil. {args$unit}"), description = harmonizedData$description)
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
  ssps <- calcOutput("GDP",
                     scenario = "SSPs",
                     unit = "constant 2005 Int$PPP",
                     extension2150 = "bezier",
                     average2020 = FALSE,
                     aggregate = FALSE,
                     supplementary = TRUE)
  ssp2Data <- ssps$x[, , "gdp_SSP2"]

  # For SSP2EU: EU countries are equal to past
  euCountries <- toolGetEUcountries(onlyWithARIADNEgdpData = TRUE)
  ssp2EUData <- ssp2Data[euCountries, getYears(ssp2Data)[getYears(ssp2Data, as.integer = TRUE) <= 2100], ]
  ssp2EUData[, , ] <- 0
  ssp2EUData[, getYears(past$x), ] <- past$x[euCountries, , ]

  # Use GDP growth rates of eurostat for the years 2022, 2023, 2024
  gr <- readSource("EurostatPopGDP", "GDPgr_projections")
  ssp2EUData[, 2022, ] <- ssp2EUData[, 2021, ] * (1 + gr[euCountries, 2022, ] / 100)
  ssp2EUData[, 2023, ] <- ssp2EUData[, 2022, ] * (1 + gr[euCountries, 2023, ] / 100)
  ssp2EUData[, 2024, ] <- ssp2EUData[, 2023, ] * (1 + gr[euCountries, 2024, ] / 100)

  # After 2024 use growth rates from future object
  pastYears <- getYears(ssp2EUData)[getYears(ssp2EUData, as.integer = TRUE) <= 2024]
  cy <- union(pastYears, getYears(future$x))
  ssp2EUData[, cy, ] <- toolHarmonizePastGrFuture(ssp2EUData[, pastYears, ],
                                                  future$x[euCountries, , ])

  # After 2070, transition to SSP2 values by 2150
  pastYears <- getYears(ssp2EUData)[getYears(ssp2EUData, as.integer = TRUE) <= 2070]
  combinedSSP2EU <- toolHarmonizePastTransition(ssp2EUData[, pastYears, ],
                                                ssp2Data[euCountries, , ],
                                                2150)

  combined <- ssp2Data
  combined[euCountries, , ] <- 0
  combined[euCountries, getYears(combinedSSP2EU), ]  <- combinedSSP2EU[euCountries, , ]
  getNames(combined) <- "gdp_SSP2EU"

  combined <- combined[, getYears(combined)[getYears(combined, as.integer = TRUE) <= 2100], ]

  list(x = combined,
       description = glue("equal to SSP2 in all countries except for EU countries. \\
                          For EU countries use {past$description} until 2021, \\
                          growth rates from Eurostat until 2024, \\
                          growth rates from {future$description} until 2070, \\
                          and converge to 2150 (bezier-extended) SSP2 values thereafter."))
}
