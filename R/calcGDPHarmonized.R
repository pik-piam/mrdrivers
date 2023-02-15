#' Get Harmonized GDP Data
#'
#' @param args Arguments passed on to harmonization functions
#' @inherit madrat::calcOutput return
#' @keywords internal
calcGDPHarmonized <- function(args) {
  # Combine "past" and "future" time series.
  harmonizedData <- switch(args$harmonization,
    "calibSSPs"       = toolGDPHarmonizeWithGDPpc(args$scenario, args$unit),
    "calibSDPs"       = toolGDPHarmonizeWithGDPpc(args$scenario, args$unit),
    "calibNoCovid"    = toolGDPHarmonizeWithGDPpc(args$scenario, args$unit),
    "calibShortCovid" = toolGDPHarmonizeWithGDPpc(args$scenario, args$unit),
    "calibLongCovid"  = toolGDPHarmonizeWithGDPpc(args$scenario, args$unit),
    "calibSSP2EU2"     = toolGDPHarmonizeSSP2EU2(args$past$x, args$future$x, args$unit),
    "past_transition" = toolHarmonizePastTransition(args$past$x, args$future$x, yEnd = 2050),
    stop("Bad input for calcGDP. Invalid 'harmonization' argument.")
  )

  # Get description of harmonization function.
  description <- switch(args$harmonization,
    "calibSSPs"       = glue("use past data, short term growth rates from IMF and \\
                             afterwards transition between {args$pastData} and {args$futureData} \\
                             with a transition period until 2100"),
    "calibSDPs"       = glue("use past data, short term growth rates from IMF and \\
                             afterwards transition between {args$pastData} and {args$futureData} \\
                             with a transition period until 2100"),
    "calibSSP2EU2"     = glue("use past data, short term growth rates from IMF and afterwards transition \\
                              between {args$pastData} and {args$futureData} with a transition period until 2100. For \\
                              European countries, just glue past with future and after 2070 converge \\
                              to 2150 SSP2 values."),
    "calibNoCovid"    = glue("use past data until 2019, short term growth rates from IMF (WEO from Oct2019 - pre \\
                             Covid) and afterwards transition to {args$futureData} by 2100."),
    "calibShortCovid" = glue("use past data, short term growth rates from IMF and afterwards transition to \\
                              noCovid until 2030."),
    "calibLongCovid"  = glue("use past data, short term growth rates from IMF and afterwards growth rates from the \\
                              noCovid scenario until 2100."),
    "past_transition" = glue("use past data and afterwards transition between {args$pastData} and \\
                             {args$futureData} with a transition period until 2050")
  )

  list(x = harmonizedData, weight = NULL, unit = glue("mil. {args$unit}"), description = description)
}


######################################################################################
# GDP Harmonization Functions
######################################################################################
toolGDPHarmonizeWithGDPpc <- function(scenario, unit) {
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
  gdp
}



toolGDPHarmonizeSSP2EU <- function(past, future, unit) {
  # We explicitly use the bezier Extension for SSP2 here, but only for harmonization purposes.
  # We return only up until 2100.
  ssp2Data <- calcOutput("GDP",
                         scenario = "SSPs",
                         unit = unit,
                         extension2150 = "bezier",
                         average2020 = FALSE,
                         aggregate = FALSE) %>%
    `[`(, , "gdp_SSP2")

  # For SSP2EU: simply glue past with future
  # Get EUR countries.
  euCountries <- toolGetEUcountries(onlyWithARIADNEgdpData = TRUE)
  futYears <- getYears(future)[getYears(future, as.integer = TRUE) >= max(getYears(past, as.integer = TRUE))]

  ssp2EUData <- ssp2Data
  ssp2EUData[euCountries, , ] <- 0
  ssp2EUData[euCountries, getYears(past), ] <- past[euCountries, , ]
  ssp2EUData[euCountries, futYears, ] <- future[euCountries, futYears, ]

  # After 2070, transition to SSP2 values by 2150
  pastYears <- getYears(ssp2EUData)[getYears(ssp2EUData, as.integer = TRUE) <= 2070]
  combinedSSP2EU <- toolHarmonizePastTransition(ssp2EUData[euCountries, pastYears, ],
                                                 ssp2Data[euCountries, , ],
                                                 2150)

  combined <- ssp2Data
  combined[euCountries, , ] <- 0
  combined[euCountries, getYears(combinedSSP2EU), ]  <- combinedSSP2EU[euCountries, , ]
  getNames(combined) <- "gdp_SSP2EU"

  combined[, getYears(combined)[getYears(combined, as.integer = TRUE) <= 2100], ]
}

toolGDPHarmonizeSSP2EU2 <- function(past, future, unit) {
  # We explicitly use the bezier Extension for SSP2 here, but only for harmonization purposes.
# We return only up until 2100.
ssp2Data <- calcOutput("GDP",
                       scenario = "SSPs",
                       unit = "constant 2005 Int$PPP",
                       extension2150 = "bezier",
                       average2020 = FALSE,
                       aggregate = FALSE) %>%
  `[`(, , "gdp_SSP2")

# For SSP2EU: simply glue past with future
# Get EUR countries.
euCountries <- toolGetEUcountries(onlyWithARIADNEgdpData = TRUE)
ssp2EUData <- ssp2Data[, getYears(ssp2Data)[getYears(ssp2Data, as.integer = TRUE) <= 2100], ]
ssp2EUData[euCountries, , ] <- 0
ssp2EUData[euCountries, getYears(past), ] <- past[euCountries, , ]

# Use GDP growth rates of eurostat for the years 2022, 2023, 2024
gr <- readSource("EurostatPopGDP", "GDPgr_projections")
ssp2EUData[euCountries, 2022, ] <- ssp2EUData[euCountries, 2021, ] * (1 + gr[euCountries, 2022, ] / 100)
ssp2EUData[euCountries, 2023, ] <- ssp2EUData[euCountries, 2022, ] * (1 + gr[euCountries, 2023, ] / 100)
ssp2EUData[euCountries, 2024, ] <- ssp2EUData[euCountries, 2023, ] * (1 + gr[euCountries, 2024, ] / 100)

# After 2024 use growth rates from future object
pastYears <- getYears(ssp2EUData)[getYears(ssp2EUData, as.integer = TRUE) <= 2024]
cy <- union(pastYears, getYears(future))
ssp2EUData[euCountries, cy, ] <- toolHarmonizePastGrFuture(ssp2EUData[euCountries, pastYears, ],
                                                           future[euCountries, , ])

# After 2070, transition to SSP2 values by 2150
pastYears <- getYears(ssp2EUData)[getYears(ssp2EUData, as.integer = TRUE) <= 2070]
combinedSSP2EU <- toolHarmonizePastTransition(ssp2EUData[euCountries, pastYears, ],
                                              ssp2Data[euCountries, , ],
                                              2150)

combined <- ssp2Data
combined[euCountries, , ] <- 0
combined[euCountries, getYears(combinedSSP2EU), ]  <- combinedSSP2EU[euCountries, , ]
getNames(combined) <- "gdp_SSP2EU"

combined[, getYears(combined)[getYears(combined, as.integer = TRUE) <= 2100], ]

}
