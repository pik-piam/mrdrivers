toolMultiplyGDPpcWithPop <- function(scenario) {
  gdppc <- calcOutput("GDPpc",
                      scenario = scenario,
                      extension2150 = "none",
                      average2020 = FALSE,
                      naming = "scenario",
                      aggregate = FALSE,
                      supplementary = TRUE)
  # GDP is equal to GDPpc * population
  gdp <- gdppc$x * gdppc$weight
  list(x = gdp,
       description = glue("use product of corresponding GDP per capita and population scenarios. \\
                           {gdppc$description}"))
}

toolHarmonizeGDPpcSSPs <- function(past, future, yEnd) {
  # Get IMF short-term income projections and fill missing with SSP2 (the SSP2 values need to be extrapolated until
  # the last year of the IMF data. Constant extrapolation assumed.)
  shortTerm <- readSource("IMF", "GDPpc") %>%
    toolFillWith(calcOutput("GDPpcFuture", scenario = "SSP2", aggregate = FALSE)) %>%
    toolInterpolateAndExtrapolate()

  # Use short term IMF growth rates (here, as far as possible)
  tmpGDPpc <- toolHarmonizePast(past$x, shortTerm, method = "growth")

  lastYearIMF <- max(getYears(shortTerm, as.integer = TRUE))
  # Make sure to add the last IMF year to the future data, just in case it is not there.
  future$x <- time_interpolate(future$x, lastYearIMF, integrate_interpolated_years = TRUE)
  # Compute difference in last IMF year
  delta <- setNames(tmpGDPpc[, lastYearIMF, ], NULL) - future$x[, lastYearIMF, ]

  # Define the years marking the start of medium (y1), and slow convergence (y2) (lastYearIMF marking the start of
  # fast convergence).
  y1 <- lastYearIMF + 5
  y2 <- lastYearIMF + 10

  # Magpie implementation would look like this (super slow, but shorter, and left here for clarity with #c do avoid
  # linter warnings):
  #c extraYearsIn <- all(c(y1, y2) %in% getYears(future$x, as.integer = TRUE))
  #c if (!extraYearsIn) future$x <- time_interpolate(future$x, c(y1, y2), integrate_interpolated_years = TRUE)
  #c years1 <- getYears(future$x)[getYears(future$x, as.integer = TRUE) <= y1]
  #c years2 <- getYears(future$x)[getYears(future$x, as.integer = TRUE) <= y2]
  #c # Placeholder
  #c x <- toolHarmonizePast(tmpGDPpc, future$x, method = "level")
  #c # Split countries with and without projection data
  #c cWithProj    <- where(future$x[, lastYearIMF, ] != 0)$true$regions
  #c # Loop over countries (only those with projection data) and scenarios
  #c for (i in cWithProj) {
  #c   for (j in getItems(x, 3)) {
  #c     if (j == "SSP2") {
  #c       x[i, , j] <- tmpGDPpc[i, , ] %>%
  #c         toolHarmonizePast(future$x[i, years1, j], method = "parallel") %>%
  #c         toolHarmonizePast(future$x[i, , j], method = "transition", yEnd = 2100)
  #c     }
  #c     if ((j %in% c("SSP1", "SSP5") && delta[i, lastYearIMF, j] >= 0) ||
  #c           j %in% c("SSP3", "SSP4") && delta[i, lastYearIMF, j] < 0) {
  #c       x[i, , j] <- toolHarmonizePast(tmpGDPpc[i, , ], future$x[i, , j], method = "transition", yEnd = 2100)
  #c     }
  #c     if ((j %in% c("SSP1", "SSP5") && delta[i, lastYearIMF, j] < 0) ||
  #c           j %in% c("SSP3", "SSP4") && delta[i, lastYearIMF, j] >= 0) {
  #c       x[i, , j] <- tmpGDPpc[i, , ] %>%
  #c         toolHarmonizePast(future$x[i, years2, j], method = "parallel") %>%
  #c         toolHarmonizePast(future$x[i, , j], method = "transition", yEnd = 2100)
  #c     }
  #c   }
  #c }
  #c if (!extraYearsIn) x <- x[, extraYears, , invert = TRUE]

  # The following is much faster, with the operation performed in a tibble.
  # Placeholder
  x <- toolHarmonizePast(tmpGDPpc, future$x, method = "level")
  # Split countries with and without projection data
  cWithProj    <- where(future$x[, lastYearIMF, ] != 0)$true$regions
  cWithoutProj <- where(future$x[, lastYearIMF, ] == 0)$true$regions
  x <- toolFastConvergeSSP(x[cWithProj, , ], delta, lastYearIMF, y1, y2, 2100) %>%
    mbind(x[cWithoutProj, , ]) %>%
    magpiesort()

  x <- toolInterpolateAndExtrapolate(x)
  lastPastYear <- max(getYears(past$x, as.integer = TRUE))
  list(x = x,
       description = glue("use {past$description} until {lastPastYear}, short term growth rates from IMF until \\
                           {lastYearIMF}, and transition to {future$description} by {yEnd}."))
}

toolBuildGDPpcSDPs <- function() {
  gdppcapSSP1 <- calcOutput("GDPpc",
                            scenario = "SSPs",
                            average2020 = FALSE,
                            extension2150 = "none",
                            naming = "scenario",
                            aggregate = FALSE)[, , "SSP1"]

  # Standard SDP inherits SSP1 GDP
  gdppcapSDP <- setNames(gdppcapSSP1, "SDP")
  # SHAPE SDP_XX variants are calculated as modifications of SSP1 GDP/cap growth rates
  combined <- purrr::map(c("SDP_EI", "SDP_MC", "SDP_RC"),
                         toolSHAPEgrowth,
                         gdppcapSSP1 = gdppcapSSP1,
                         startFromYear = 2025) %>%
    mbind()
  combined <- mbind(gdppcapSDP, combined)

  combined[is.nan(combined) | combined == Inf] <- 0

  list(x = combined,
       description = glue("use SSP1 scenario and adapt growth rates."))
}

toolDivideGDPbyPop <- function(scenario) {
  gdp <- calcOutput("GDP",
                    scenario = scenario,
                    extension2150 = "none",
                    average2020 = FALSE,
                    naming = "scenario",
                    aggregate = FALSE,
                    supplementary = TRUE)
  pop <- calcOutput("Population",
                    scenario = scenario,
                    extension2150 = "none",
                    naming = "scenario",
                    aggregate = FALSE,
                    supplementary = TRUE,
                    years = getYears(gdp$x))
  gdppc <- gdp$x / pop$x
  list(x = gdppc,
       description = glue("use ratio of corresponding GDP and population scenarios. {gdp$description} \\
                          {pop$description}"))
}

toolHarmonizeGDPpcADBs <- function(past, future) {
  ssp2Data <- calcOutput("GDPpc", scenario = "SSP2", extension2150 = "none", average2020 = FALSE, aggregate = FALSE)

  # For both ADB scenarios, overwrite SSP2 IND data with ADB IND data
  combined <- purrr::map(getNames(future$x), function(x) {
    y <- setNames(ssp2Data, x)
    y["IND", , ] <- 0
    # Transition IND from past to future. Here keep future as is, and use growth rates from past.
    dataIND <- toolHarmonizeFuture(past$x["IND", , ], future$x["IND", , x], method = "growth")
    y["IND", getYears(dataIND), ] <- dataIND
    y
  }) %>%
    mbind()

  list(x = combined,
       description = glue("equal to SSP2 in all countries except for IND. \\
                          For IND use {past$description} until {max(getYears(past$x, as.integer = TRUE))}, \\
                          and converge to {future$description} by 2030."))
}


toolFastConvergeSSP <- function(x, delta, yearStart, y1, y2, yearEnd) {
  x <- x %>% tibble::as_tibble() %>% dplyr::rename("SSP" = "variable")
  delta <- delta %>% tibble::as_tibble() %>%  dplyr::select("iso3c", "SSP" = "d3", "d" = "value")

  x <- x %>%
    dplyr::left_join(delta, by = c("iso3c", "SSP")) %>%
    dplyr::mutate(
      # Medium convergence
      value = dplyr::if_else(.data$year > yearStart & .data$SSP == "SSP2",
                             dplyr::if_else( # nolint: indentation_linter.
                               .data$year <= y1,
                               .data$value + .data$d,
                               dplyr::if_else(
                                 .data$year <= yearEnd,
                                 .data$value + .data$d * (yearEnd - .data$year) / (yearEnd - y1),
                                 .data$value
                               )
                             ),
                             .data$value
      ),
      # Fast convergence
      value = dplyr::if_else(.data$year > yearStart &
                               ((.data$SSP %in% c("SSP1", "SSP5") & .data$d < 0) | # nolint: indentation_linter.
                                  (.data$SSP %in% c("SSP3", "SSP4") & .data$d >= 0)),
                             dplyr::if_else( # nolint: indentation_linter.
                               .data$year <= yearEnd,
                               .data$value + .data$d * (yearEnd - .data$year) / (yearEnd - yearStart),
                               .data$value
                             ),
                             .data$value
      ),
      # Slow convergence
      value = dplyr::if_else(.data$year > yearStart &
                               ((.data$SSP %in% c("SSP3", "SSP4") & .data$d < 0) | # nolint: indentation_linter.
                                  (.data$SSP %in% c("SSP1", "SSP5") & .data$d >= 0)),
                             dplyr::if_else( # nolint: indentation_linter.
                               .data$year <= y2,
                               .data$value + .data$d,
                               dplyr::if_else(
                                 .data$year <= yearEnd,
                                 .data$value + .data$d * (yearEnd - .data$year) / (yearEnd - y2),
                                 .data$value
                               )
                             ),
                             .data$value
      ),
      # Add a minimum value for value here. This can occur when the delta computed in yearStart is
      # so large relative to the original GDPpc value in yearStart, that a further decrease in the
      # years y1 and y2 pushes the GDPpc into the negative.
      value = pmax(.data$value, 0.01)
    ) %>%
    dplyr::select(-"d")
  x %>%
    dplyr::rename("variable" = "SSP") %>%
    as.magpie(spatial = "iso3c", temporal = "year", tidy = TRUE)
}

# Note that here we label the GDP scenarios with their scenario abbreviations, and not with the name of the SHAPE
# economics dimensions. Mapping (Scenario <-> economics dimension):
# - Economy-driven Innovation (SDP_EI) <-> innovation-driven economy
# - Resilient Communities (SDP_RC) <-> society-driven economy
# - Managing the global Commons (SDP_MC) <-> service-driven economy
# The two alternative SHAPE scenarios will re-use these GDP trajectories, so they are not explicitly included here.
# - Local Solutions (SDP_LS) <-> society-driven economy (same as SDP_RC)
# - Green and Social market economy (SDP_GS) <-> service-driven economy (same as SDP_MC)
# Calculate modified growth rates and resulting gdp/capita in forward simulation
toolSHAPEgrowth <- function(shapeGDPScenario, gdppcapSSP1, startFromYear) {

  # calculation of growth rates
  yrs <- getYears(gdppcapSSP1, as.integer = TRUE)
  # flexible timestep
  # LONGTERM would be better to always use yearly timesteps in this computation, and select years afterwards
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
      if (shapeGDPScenario == "SDP_EI") {
        modificationFactor <- logisticTransition(
          gdppcap[, yr, ], l0 = 1.15, l = 1, k = 20, x0 = 15e3, useLog10 = TRUE
        )
        # service-driven (SDP_MC): growth rate reduced based on relative distance to technology frontier
        # (given by the US)
      } else if (shapeGDPScenario == "SDP_MC") {
        # define US as technology frontier
        frontier <- gdppcap["USA", yr, ]
        getItems(frontier, 1) <- "GLO"
        # countries with gdp/cap above US are treated the same as the US -> set diff = 0
        reldiff2frontier <- pmax((frontier[, yr, ] - gdppcap[, yr, ]) / frontier[, yr, ], 0)
        modificationFactor <- logisticTransition(
          reldiff2frontier[, yr, ], l0 = 1, l = 0.5, k = -30, x0 = 0.2, useLog10 = FALSE
        )
        # society-driven (SDP_RC): gradual transition to zero growth for high-income countries
      } else if (shapeGDPScenario == "SDP_RC") {
        modificationFactor <- logisticTransition(gdppcap[, yr, ], l0 = 1, l = 0, k = 10, x0 = 30e3, useLog10 = TRUE)
      } else {
        stop("cannot create SHAPE GDP scenarios: unknown scenario")
      }

      # For service (SDP_MC) and society (SDP_RC) additionally add a smoothing for the first two 5-year timesteps
      # (2025 and 2030 with current default startFromYear = 2025)
      # Apply only 1/3 of the modification for first 5 years, and 2/3 of the modification for another 5 years      
      if (shapeGDPScenario %in% c("SDP_MC", "SDP_RC")) {
        if (yr >= startFromYear && yr < startFromYear + 5) {
          modificationFactor[, yr, ] <- 1 / 3. * (modificationFactor[, yr, ] - 1) + 1
        } else if (yr >= startFromYear + 5 && yr < startFromYear + 10) {
          modificationFactor[, yr, ] <- 2 / 3. * (modificationFactor[, yr, ] - 1) + 1
        }
      }
      growthrate[, yr, ] <- growthrate[, yr, ] * modificationFactor[, yr, ]
    }

    # Calculate next gdp/cap based on current value and (modified) growth rate
    gdppcap[, yr + as.integer(timestep[, yr, ]), ] <- gdppcap[, yr, ] * (1 + growthrate[, yr, ] / 100.)^timestep[, yr, ]
  }

  gdppcap
}

# Smooth transition from lO to l, with steepness k and midpoint x0
logisticTransition <- function(x, l0, l, k, x0, useLog10 = FALSE) {
  if (useLog10) {
    x <- log10(x)
    x0 <- log10(x0)
  }
  logistic <- 1. / (1 + exp(-k * (x - x0)))
  return(l0 - (l0 - l) * logistic)
}
