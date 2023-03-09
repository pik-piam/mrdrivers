
#' Get Harmonized GDPpc Data
#'
#' @param args Arguments passed on to harmonization functions
#' @inherit madrat::calcOutput return
#' @keywords internal
calcGDPpcHarmonized <- function(args) {
  # Combine "past" and "future" time series.
  harmonizedData <- switch(args$harmonization,
    "calibSSPs"       = toolGDPpcHarmonizeSSP(args$past$x, args$future$x, args$unit, yEnd = 2100),
    "calibSDPs"       = toolGDPpcHarmonizeSDP(args$unit),
    "calibSSP2EU"     = toolGDPpcHarmonizeSSP2EU(args$unit),
    "calibNoCovid"    = toolGDPpcHarmonizeSSP(args$past$x, args$future$x, args$unit, yEnd = 2100, noCovid = TRUE),
    "calibLongCovid"  = toolGDPpcHarmonizeLongCovid(args$unit),
    "calibShortCovid" = toolGDPpcHarmonizeShortCovid(args$unit),
    stop("Bad input for calcGDPpc. Invalid 'harmonization' argument.")
  )

  # Get description of harmonization function.
  description <- switch(args$harmonization,
    "calibSSP2EU" = glue("use past data, short term growth rates from IMF and afterwards transition \\
                          between {args$pastData} and {args$futureData} with a transition period until 2100. For \\
                          European countries, just glue past with future and after 2070 converge \\
                          to 2150 SSP2 values."),
    "calibNoCovid" = glue("use past data until 2019, short term growth rates from IMF (WEO from Oct2019 - pre Covid) \\
                           and afterwards transition to {args$futureData} by 2100."),
    "calibShortCovid" = glue("use past data, short term growth rates from IMF and afterwards transition to \\
                              noCovid until 2030."),
    "calibLongCovid" = glue("use past data, short term growth rates from IMF and afterwards growth rates from the \\
                             noCovid scenario until 2100."),
    glue("use past data, short term growth rates from IMF and \\
          afterwards transition between {args$pastData} and {args$futureData} \\
          with a transition period until 2100")
  )

  list(x = harmonizedData, weight = NULL, unit = args$unit, description = description)
}




######################################################################################
# GDPpc Harmonization Functions
######################################################################################
toolGDPpcHarmonizeSSP <- function(pastGDPpc, futureGDPpc, unit, yEnd, noCovid = FALSE) {

  if (!noCovid) {
    # Get IMF short-term income projections and fill missing with SSP2
    imfGDPpc <- readSource("IMF", "GDPpc")
  } else {
    # noCovid = TRUE leads to a counterfactual scenario where no Covid shock is experienced
    ## Use past data only until 2019
    pastGDPpc <- pastGDPpc[, getYears(pastGDPpc, as.integer = T)[getYears(pastGDPpc, as.integer = TRUE) <= 2019], ]
    ## Get pre-covid IMF short-term income projections and fill missing with SSP2
    imfGDPpc <- readSource("IMF", "GDPpc", "WEOOct2019all.xls")
  }

  fill <- calcOutput("GDPpcFuture", GDPpcFuture = "SSPsOld-MI", unit = unit, aggregate = FALSE)[, , "gdppc_SSP2"]
  imfGDPpc <- imfGDPpc %>%
    toolFillWith(fill) %>%
    toolInterpolateAndExtrapolate()

  # Use short term IMF growth rates (here, as far as possible)
  tmpGDPpc <- toolHarmonizePastGrFuture(pastGDPpc, imfGDPpc)

  # Transform into tibble, combine past and future tibbles
  tmpGDPpc <- tmpGDPpc %>%
    tibble::as_tibble() %>%
    dplyr::select(-"variable") %>%
    dplyr::group_by(.data$iso3c) %>%
    dplyr::filter(!all(.data$value == 0)) %>%
    dplyr::ungroup()

  # Make sure to add the last IMF year to the future SSP data, just in case it's not there. That is the year from which
  # convergence begins.
  yStart <- max(getYears(imfGDPpc, as.integer = TRUE))
  futureGDPpc <- futureGDPpc %>%
    magclass::time_interpolate(yStart, integrate_interpolated_years = TRUE) %>%
    dplyr::as_tibble() %>%
    dplyr::group_by(.data$iso3c) %>%
    dplyr::filter(!all(.data$value == 0)) %>%
    dplyr::ungroup()

  combinedGDPpc <- tidyr::expand_grid(iso3c = unique(tmpGDPpc$iso3c),
                                      year = unique(c(tmpGDPpc$year, futureGDPpc$year)),
                                      variable = unique(futureGDPpc$variable)) %>%
    dplyr::left_join(tmpGDPpc, by = c("iso3c", "year")) %>%
    dplyr::left_join(dplyr::select(futureGDPpc, "iso3c", "year", "variable", "iiasa_gdppc" = "value"),
                     by = c("iso3c", "year", "variable")) %>%
    dplyr::rename("SSP" = "variable") %>%
    dplyr::mutate(SSP = sub("^......", "", .data$SSP))

  # Pass to special convergence function
  combinedGDPpc <- convergeSpecial(combinedGDPpc, yearStart = yStart, yearEnd = yEnd)

  # Retransform into magpie
  combinedGDPpc <- combinedGDPpc %>%
    dplyr::mutate(SSP = paste0("gdppc_", .data$SSP)) %>%
    dplyr::select("iso3c", "year", "variable" = "SSP", "value") %>%
    tidyr::replace_na(list("value" = 0)) %>%
    as.magpie() %>%
    toolCountryFill(fill = 0)

  combinedGDPpc
}

toolGDPpcHarmonizeSDP <- function(unit) {

  gdppcapSSP1 <- calcOutput("GDPpc",
                             scenario = "SSPs",
                             unit = unit,
                             extension2150 = "none",
                             average2020 = FALSE,
                             aggregate = FALSE)[, , "gdppc_SSP1"]

  # standard SDP inherits SSP1 GDP
  gdppcapSDP <- gdppcapSSP1
  getNames(gdppcapSDP) <- gsub("SSP1", "SDP", getNames(gdppcapSDP))
  # SHAPE SDP_XX variants are calculated as modifications of SSP1 GDP/cap growth rates
  combined <- purrr::map(c("gdppc_SDP_EI", "gdppc_SDP_MC", "gdppc_SDP_RC"),
                         computeSHAPEgrowth,
                         gdppcapSSP1 = gdppcapSSP1,
                         startFromYear = 2020) %>%
    mbind() %>%
    mbind(gdppcapSDP)

  combined[is.nan(combined) | combined == Inf] <- 0
  combined
}

toolGDPpcHarmonizeSSP2EU <- function(unit) {
  gdp <- calcOutput("GDP",
                    scenario = "SSP2EU",
                    unit = unit,
                    extension2150 = "none",
                    average2020 = FALSE,
                    aggregate = FALSE)
  pop <- calcOutput("Population", scenario = "SSP2EU", extension2150 = "none", aggregate = FALSE)
  getNames(gdp) <- getNames(pop) <- gsub("pop", "gdppc", getNames(pop))
  gdp / pop
}

toolGDPpcHarmonizeShortCovid <- function(unit) {

  gdppcSSPs <- calcOutput("GDPpc",
                          scenario = "SSPs",
                          unit = unit,
                          extension2150 = "none",
                          average2020 = FALSE,
                          aggregate = FALSE)

  gdppcNoCovid <- calcOutput("GDPpc",
                             scenario = "noCovid",
                             unit = unit,
                             extension2150 = "none",
                             average2020 = FALSE,
                             aggregate = FALSE)

  # Use SSPs until the last year of the IMF predictions, afterwards converge to NoCovid by 2030
  yIMF <- max(getYears(readSource("IMF", "GDPpc"), as.integer = TRUE))
  gdppcSSPs <- gdppcSSPs[, getYears(gdppcSSPs, as.integer = TRUE)[getYears(gdppcSSPs, as.integer = TRUE) <= yIMF], ]
  mbind(purrr::map(1:5, ~toolHarmonizePastTransition(gdppcSSPs[, , .x], gdppcNoCovid[, , .x], yEnd = 2030)))
}

toolGDPpcHarmonizeLongCovid <- function(unit) {

  gdppcSSPs <- calcOutput("GDPpc",
                          scenario = "SSPs",
                          unit = unit,
                          extension2150 = "none",
                          average2020 = FALSE,
                          aggregate = FALSE)

  gdppcNoCovid <- calcOutput("GDPpc",
                             scenario = "noCovid",
                             unit = unit,
                             extension2150 = "none",
                             average2020 = FALSE,
                             aggregate = FALSE)

  # Use SSPs until the last year of the IMF predictions, afterwards use NoCovid growth rates
  yIMF <- max(getYears(readSource("IMF", "GDPpc"), as.integer = TRUE))
  gdppcSSPs <- gdppcSSPs[, getYears(gdppcSSPs, as.integer = TRUE)[getYears(gdppcSSPs, as.integer = TRUE) <= yIMF], ]
  mbind(purrr::map(1:5, ~toolHarmonizePastGrFuture(gdppcSSPs[, , .x], gdppcNoCovid[, , .x])))
}




#########################
### Helper functions
#########################
convergeSpecial <- function(x, yearStart, yearEnd) {

  # Compute the difference in yearStart.
  dif <- x %>%
    dplyr::filter(.data$year == yearStart) %>%
    dplyr::mutate(d = .data$iiasa_gdppc - .data$value) %>%
    dplyr::select("iso3c", "SSP", "d")

  # Define the years marking the start of medium, and slow convergence
  # (yearStart being the start for fast convergence)
  y1 <- yearStart + 5
  y2 <- yearStart + 10

  x <- x %>%
    dplyr::left_join(dif, by = c("iso3c", "SSP")) %>%
    dplyr::mutate(
      # Medium convergence
      value = dplyr::if_else(
        .data$year > yearStart & .data$SSP == "SSP2",
        dplyr::if_else(
          .data$year <= y1,
          .data$iiasa_gdppc - .data$d,
          dplyr::if_else(
            .data$year <= yearEnd,
            .data$iiasa_gdppc - .data$d * (yearEnd - .data$year) / (yearEnd - y1),
            .data$iiasa_gdppc
          )
        ),
        .data$value
      ),
      # Fast convergence
      value = dplyr::if_else(
         .data$year > yearStart &
            ((.data$SSP %in% c("SSP1", "SSP5") & .data$d >= 0) |
             (.data$SSP %in% c("SSP3", "SSP4") & .data$d < 0)),
         dplyr::if_else(
             .data$year <= yearEnd,
             .data$iiasa_gdppc - .data$d * (yearEnd - .data$year) / (yearEnd - yearStart),
             .data$iiasa_gdppc
         ),
         .data$value
      ),
      # Slow convergence
      value = dplyr::if_else(
        .data$year > yearStart &
            ((.data$SSP %in% c("SSP3", "SSP4") & .data$d >= 0) |
             (.data$SSP %in% c("SSP1", "SSP5") & .data$d < 0)),
        dplyr::if_else(
            .data$year <= y2,
            .data$iiasa_gdppc - .data$d,
            dplyr::if_else(
              .data$year <= yearEnd,
              .data$iiasa_gdppc - .data$d * (yearEnd - .data$year) / (yearEnd - y2),
              .data$iiasa_gdppc
            )
        ),
        .data$value
      ),
      # Add a minimum value for value here. This can occur when the d computed in yearStart is
      # so large relative to the original GDPpc value in yearStart, that a further dercease in the
      # years y1 and y2 pushes the GDPpc into the negative.
      value = pmax(.data$value, 0.01)
    ) %>%
    dplyr::select(-"d")
  x
}

###########################################
# Additional functions to derive the SHAPE GDP scenarios from the SSP1 scenario

# Note that here we label the GDP scenarios with their scenario abbreviations,
# and not with the name of the SHAPE economics dimensions.

# Mapping: Scenario <-> economics dimension
# Economy-driven innovation (SDP_EI) <-> innovation-driven economy
# Resilient communities (SDP_RC) <-> society-driven economy
# Managing the global commons (SDP_MC) <-> service-driven economy

# The two alternative SHAPE scenarios will re-use these GDP trajectories, so they are not explicitly included here.
# Local solutions (SDP_LS) <-> society-driven economy (same as SDP_RC)
# Green and social market economy (SDP_GS) <-> service-driven economy (same as SDP_MC)

# for details see the SHAPE scenario spreadsheet
# https://docs.google.com/spreadsheets/d/1v8dlZDj-AW8_oiyd9rdV3rGfVBt_PBCja1_NyEEFRMA/edit?usp=sharing
###########################################

# calculate modified growth rates and resulting gdp/capita in forward simulation
computeSHAPEgrowth <- function(shapeGDPScenario, gdppcapSSP1, startFromYear) {

  # calculation of growth rates
  yrs <- getYears(gdppcapSSP1, as.integer = TRUE)
  # flexible timestep
  # LONGTERM would be better to always use yearly timesteps in this computation,
  # and select years afterwards
  timestep <- new.magpie(years = yrs)
  timestep[, , ] <- dplyr::lead(yrs) - yrs
  # assign average growth rate g_t of period t -> t+ timestep
  # this means modifications of growth rate t will affect GDP in t+timestep
  yrsShifted <- yrs[2:length(yrs)]
  yrsBase <- yrs[1:(length(yrs) - 1)]
  growthrateSSP1 <- 100 *
    ((setYears(gdppcapSSP1[, yrsShifted, ], yrsBase) / gdppcapSSP1[, yrsBase, ]) ^
       (1. / timestep[, yrsBase, ]) - 1)

  # modified growth rates and gdp/cap
  growthrate <- setNames(as.magpie(growthrateSSP1), shapeGDPScenario)
  gdppcap <- setNames(as.magpie(gdppcapSSP1), shapeGDPScenario)
  gdppcap[, yrs > startFromYear, ] <- NA

  for (yr in yrs[1:(length(yrs) - 1)]) {
    # modify growth rates only for future period (default: from 2020 onwards)
    if (yr >= startFromYear) {
      # innovation-driven (SDP_EI): enhance growth rates for low-income countries by up to 15%
      if (shapeGDPScenario == "gdppc_SDP_EI") {
        modificationFactor <- logisticTransition(
          gdppcap[, yr, ], l0 = 1.15, l = 1, k = 20, x0 = 15e3, useLog10 = TRUE
        )
      }
      # service-driven (SDP_MC): growth rate reduced based on relative distance to technology frontier (given by the US)
      else if (shapeGDPScenario == "gdppc_SDP_MC") {
        # define US as technology frontier
        frontier <- gdppcap["USA", yr, ]
        getItems(frontier, 1) <- "GLO"
        # countries with gdp/cap above US are treated the same as the US -> set diff = 0
        reldiff2frontier <- pmax((frontier[, yr, ] - gdppcap[, yr, ]) / frontier[, yr, ], 0)
        modificationFactor <- logisticTransition(
          reldiff2frontier[, yr, ], l0 = 1, l = 0.5, k = -30, x0 = 0.2, useLog10 = FALSE
        )
      }
      # society-driven (SDP_RC): gradual transition to zero growth for high-income countries
      else if (shapeGDPScenario == "gdppc_SDP_RC") {
        modificationFactor <- logisticTransition(gdppcap[, yr, ], l0 = 1, l = 0, k = 10, x0 = 30e3, useLog10 = TRUE)
      } else {
        stop("cannot create SHAPE GDP scenarios: unknown scenario")
      }

      # for service (SDP_MC) and society (SDP_RC) additionally add a smoothing for 2020 and 2025 timesteps
      # apply only 1/3 (2020-2024) and 2/3 (2025-2029) of the modification
      if (shapeGDPScenario %in% c("gdppc_SDP_MC", "gdppc_SDP_RC")) {
        if (yr >= 2020 && yr < 2025) {
          modificationFactor[, yr, ] <- 1 / 3. * (modificationFactor[, yr, ] - 1) + 1
        } else if (yr >= 2025 && yr < 2030) {
          modificationFactor[, yr, ] <- 2 / 3. * (modificationFactor[, yr, ] - 1) + 1
        }
      }
      growthrate[, yr, ] <- growthrate[, yr, ] * modificationFactor[, yr, ]
    }

    # calculate next gdp/cap based on current value and (modified) growth rate
    gdppcap[, yr + as.integer(timestep[, yr, ]), ] <- gdppcap[, yr, ] * (1 + growthrate[, yr, ] / 100.)^timestep[, yr, ]
  }
  return(gdppcap)
}

# helper function: smooth transition from lO to l, with steepness k and midpoint x0
logisticTransition <- function(x, l0, l, k, x0, useLog10 = FALSE) {
  if (useLog10) {
    x <- log10(x)
    x0 <- log10(x0)
  }
  logistic <- 1. / (1 + exp(-k * (x - x0)))
  return(l0 - (l0 - l) * logistic)
}
