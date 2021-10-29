#' calcGDPpc
#'
#' @param GDPpcCalib String or vector of strings
#' @param GDPpcPast String or vector of strings
#' @param GDPpcFuture String ot vector of strings
#' @inheritParams calcGDP
#' @inherit calcGDP return
#'
#' @seealso [madrat::calcOutput()]
#' @family GDPpc functions
#' @family Combined scenario functions
#'
#' @examples \dontrun{
#' library(mrdrivers)
#' calcOutput("GDPpc")
#' }
#'
calcGDPpc <- function(GDPpcCalib  = c("calibSSPs", "calibSDPs", "calibSSP2EU"),
                      GDPpcPast   = c("WDI-MI",    "WDI-MI",    "Eurostat-WDI-MI"),
                      GDPpcFuture = c("SSPs-MI",   "SDPs-MI",   "SSP2EU-MI"),
                      unit = "constant 2005 Int$PPP",
                      extension2150 = "bezier",
                      FiveYearSteps = TRUE,
                      naming = "indicator_scenario") {
  # Check user input
  toolCheckUserInput("GDPpc", as.list(environment()))
  # Call internalCalcGDPpc function the appropriate number of times
  toolInternalCalc("GDPpc", as.list(environment()))
}

######################################################################################
# Internal Function
######################################################################################
internalCalcGDPpc <- function(GDPpcCalib,
                              GDPpcPast,
                              GDPpcFuture,
                              unit,
                              extension2150,
                              FiveYearSteps,
                              naming) {
   # GDPpc scenarios are constructed in PPPs. If MERs are desired, scenarios with the
   # same base year but in PPPs are constructed, and converted to MERs at the end.
  if (grepl("^constant .* US\\$MER$", unit)) {
    construct_unit <- paste0("constant ",  substr(unit, 10, 13), " Int$PPP")
  } else {
    construct_unit <- unit
  }

  # Depending on the chosen GDPpcCalib, the harmonization function either requires 'past' and
  # 'future' GDPpc scenarios, OR NOT, which is the case for "calibSDPs" for example, where
  # the computations are done based off of the combined SSP1 GDPpc scenario.
  if (GDPpcCalib == "calibSSPs") {
    # Compute "past" and "future" time series.
    past <- calcOutput("GDPpcPast",
                       GDPpcPast = GDPpcPast,
                       unit = construct_unit,
                       aggregate = FALSE)
    future <- calcOutput("GDPpcFuture",
                         GDPpcFuture = GDPpcFuture,
                         unit = construct_unit,
                         extension2150 = "none",
                         aggregate = FALSE)
  } else {
    # Save arguments as list.
    args <- as.list(environment())
  }

  # Combine "past" and "future" time series.
  combined <- switch(
    GDPpcCalib,
    "calibSSPs"   = gdppcHarmonizeSSP(past, future, construct_unit, yEnd = 2100),
    "calibSDPs"   = gdppcHarmonizeSDP(args),
    "calibSSP2EU" = gdppcHarmonizeSSP2EU(args),
    stop("Bad input for calcGDPpc. Invalid 'GDPpcCalib' argument.")
  )

  # Get description of harmonization function.
  description <- switch(
    GDPpcCalib,
    "calibSSP2EU" = glue("use past data, short term growth rates from IMF and afterwards transition \\
                          between {GDPpcPast} and {GDPpcFuture} with a transition period until 2100. For \\
                          European countries, just glue past with future and after 2070 converge \\
                          to 2150 SSP2 values."),
    glue("use past data, short term growth rates from IMF and \\
          afterwards transition between {GDPpcPast} and {GDPpcFuture} \\
          with a transition period until 2100")
  )

  # Apply finishing touches to combined time-series
  combined <- toolFinishingTouches(combined, extension2150, FiveYearSteps, naming, unit, construct_unit)

  # Get weight
  weight <- calcOutput("Population",
                       PopulationCalib = GDPpcCalib,
                       PopulationPast = GDPpcPast,
                       PopulationFuture = GDPpcFuture,
                       FiveYearSteps = FiveYearSteps,
                       extension2150 = extension2150,
                       naming = naming,
                       aggregate = FALSE)
  # Give weight same names as data, so that aggregate doesn't mess up data dim
  getNames(weight) <- gsub("pop", "gdppc", getNames(weight))

  list(x = combined,
       weight = weight,
       unit = unit,
       description = glue("Datasource for the Past: {GDPpcPast}. Datasource for the Future: \\
                           {GDPpcFuture}. Calibrated to {description}."))
}



######################################################################################
# GDPpc Harmonization Functions
######################################################################################
gdppcHarmonizeSSP <- function(pastGDPpc, futureGDPpc, unit, yEnd) {
  # Get IMF short-term income projcetions and fill missing with SSP2
  imfGDPpc <- readSource("IMF", "GDPpc")
  fill <- calcOutput("GDPpcFuture",
                     GDPpcFuture = "SSPs-MI",
                     unit = unit,
                     extension2150 = "none",
                     aggregate = FALSE)[, , "gdppc_SSP2"]
  imfGDPpc <- imfGDPpc %>%
    toolFillWith(fill) %>%
    toolInterpolateAndExtrapolate()

  # Use short term IMF growth rates (here, as far as possible)
  tmpGDPpc <- toolHarmonizePastGrFuture(pastGDPpc, imfGDPpc)

  # Transform into tibble, combine past and future tibbles
  tmpGDPpc <- tmpGDPpc %>%
    as.data.frame(rev = 2) %>%
    tibble::as_tibble() %>%
    dplyr::select("iso3c", "year", "value" = ".value")

  # Make sure to add the last IMF year to the future SSP data, jsut in case
  # it's not there. That is the year from which convergence begins.
  yStart <- max(getYears(imfGDPpc, as.integer = TRUE))
  futureGDPpc <- futureGDPpc %>%
    toolAddInterpolatedYear(yStart) %>%
    as.data.frame(rev = 2) %>%
    tibble::as_tibble() %>%
    dplyr::select("iso3c", "year", "variable", "value" = ".value")

  combinedGDPpc <- tidyr::expand_grid(iso3c = unique(tmpGDPpc$iso3c),
                                       year = unique(c(tmpGDPpc$year, futureGDPpc$year)),
                                       variable = unique(futureGDPpc$variable)) %>%
    dplyr::left_join(tmpGDPpc, by = c("iso3c", "year")) %>%
    dplyr::left_join(futureGDPpc %>%
                dplyr::select(.data$iso3c, .data$year, .data$variable, "iiasa_gdppc" = .data$value),
              by = c("iso3c", "year", "variable")) %>%
    dplyr::rename("SSP" = .data$variable) %>%
    dplyr::mutate(SSP = sub("^......", "", .data$SSP))

  # Pass to special convergence function
  combinedGDPpc <- convergeSpecial(combinedGDPpc, yearStart = yStart, yearEnd = yEnd)

  # Retransform into magpie
  combinedGDPpc <- combinedGDPpc %>%
    dplyr::mutate(SSP = paste0("gdppc_", .data$SSP)) %>%
    dplyr::select(.data$iso3c, .data$year, "variable" = .data$SSP, .data$value) %>%
    as.magpie()

  combinedGDPpc
}


gdppcHarmonizeSDP <- function(args) {

  gdppcapSSP1 <- calcOutput("GDPpc",
                             GDPpcCalib  = "calibSSPs",
                             GDPpcPast   = args$GDPpcPast,
                             GDPpcFuture = "SSPs-MI",
                             unit = args$unit,
                             extension2150 = "none",
                             FiveYearSteps = FALSE,
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


gdppcHarmonizeSSP2EU <- function(args) {
  gdp <- calcOutput("GDP",
                    GDPCalib = args$GDPpcCalib,
                    GDPPast = args$GDPpcPast,
                    GDPFuture = args$GDPpcFuture,
                    unit = args$unit,
                    extension2150 = "none",
                    FiveYearSteps = FALSE,
                    aggregate = FALSE)

  pop <- calcOutput("Population",
                    PopulationCalib = args$GDPpcCalib,
                    PopulationPast = args$GDPpcPast,
                    PopulationFuture = args$GDPpcFuture,
                    extension2150 = "none",
                    FiveYearSteps = FALSE,
                    aggregate = FALSE)

  getNames(gdp) <- getNames(pop) <- gsub("pop", "gdppc", getNames(pop))
  gdp / pop
}


#########################
### Helper functions
#########################
convergeSpecial <- function(x, yearStart, yearEnd) {

  # Compute the difference in yearStart.
  dif <- x %>%
    dplyr::filter(.data$year == yearStart) %>%
    dplyr::mutate(d = .data$iiasa_gdppc - .data$value) %>%
    dplyr::select(.data$iso3c, .data$SSP, .data$d)

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
          .data$year == y1,
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
    dplyr::select(-.data$d)
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
          gdppcap[, yr, ], L0 = 1.15, L = 1, k = 20, x0 = 15e3, useLog10 = TRUE
        )
      }
      # service-driven (SDP_MC): growth rate reduced based on relative distance to technology frontier (given by the US)
      else if (shapeGDPScenario == "gdppc_SDP_MC") {
        # define US as technology frontier
        frontier <- gdppcap["USA", yr, ]
        getItems(frontier, 1) <- "GLO"
        # countries with gdp/cap above US are treated the same as the US -> set diff = 0
        reldiff_to_frontier <- pmax((frontier[, yr, ] - gdppcap[, yr, ]) / frontier[, yr, ], 0)
        modificationFactor <- logisticTransition(
          reldiff_to_frontier[, yr, ], L0 = 1, L = 0.5, k = -30, x0 = 0.2, useLog10 = FALSE
        )
      }
      # society-driven (SDP_RC): gradual transition to zero growth for high-income countries
      else if (shapeGDPScenario == "gdppc_SDP_RC") {
        modificationFactor <- logisticTransition(gdppcap[, yr, ], L0 = 1, L = 0, k = 10, x0 = 30e3, useLog10 = TRUE)
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

# helper function: smooth transition from LO to L, with steepness k and midpoint x0
logisticTransition <- function(x, L0, L, k, x0, useLog10 = FALSE) {
  if (useLog10) {
    x <- log10(x)
    x0 <- log10(x0)
  }
  logistic <- 1. / (1 + exp(-k * (x - x0)))
  return(L0 - (L0 - L) * logistic)
}
