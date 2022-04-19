#' Get GDP per capita scenarios and building blocks
#'
#' @description
#' Get complete GDP per capita scenarios with calcGDPpc, or the past/future scenario building blocks with calcGDPpcPast
#' and calcGDPpcFuture.
#'
#' Complete scenarios are created by harmonizing future projections (returned by calcGDPpcFuture) onto historical
#' data (returned by calcGDPpcPast) and cover the years between 1960 and 2100.
#'
#' If GDP per capita data for a scenario is required, even if just for a single year, always use calcGDPpc, as what is
#' returned by calcGDPpcPast or calcGDPpcFuture may not end up as is in the scenario, depending on the harmonization
#' function used (see the GDPpcCalib argument for more information). Use calcGDPpcPast and calcGDPpcFuture only when
#' trying to access specific GDP per capita data, or when constructing new complete scenarios.
#'
#' By default, calcGDPpc returns the following scenarios:
#' \itemize{
#'   \item the SSPs, i.e. SSP1-5
#'   \item the SDPs, i.e. SDP, SDP_EI, SDP_RC, and SDP_MC
#'   \item SSP2EU
#' }
#'
#' @param GDPpcCalib String or vector of strings
#' @param GDPpcPast String or vector of strings
#' @param GDPpcFuture String or vector of strings
#'
#' @inheritParams calcGDP
#' @inherit calcGDP return
#' @inheritSection calcGDP Combining data sources with "-"
#' @inheritSection calcGDP Vectorization of arguments
#' @inheritSection calcGDP Return supplementary information
#'
#' @seealso [madrat::calcOutput()]
#' @family mrdrivers calc-functions
#'
#' @examples \dontrun{
#' library(mrdrivers)
#' calcOutput("GDPpc")
#' }
#'
calcGDPpc <- function(scenario = c("SSPs", "SDPs", "SSP2EU"),
                      GDPpcCalib  = NULL,                      # nolint
                      GDPpcPast   = NULL,                      # nolint
                      GDPpcFuture = NULL,                      # nolint
                      unit = "constant 2005 Int$PPP",
                      extension2150 = "bezier",
                      FiveYearSteps = TRUE,                    # nolint
                      average2020 = TRUE,
                      naming = "indicator_scenario") {
  # Check user input
  toolCheckUserInput("GDPpc", as.list(environment()))

  # If the xPast, xFuture and xCalib arguments are null, then query them using "scenario" and load them into the
  # function environment.
  if (is.null(c(GDPpcCalib, GDPpcPast, GDPpcFuture))) {
    invisible(list2env(toolGetScenarioDefinition("GDPpc", scenario, unlist = TRUE), environment()))
  }

  # Create a list of all the arguments, dropping the scenario argument, which isn't required for the internal
  # calculation.
  h <- as.list(environment())
  l <- purrr::keep(h, names(h) != "scenario")
  # Call calcInternalGDP function the appropriate number of times (map) and combine (reduce)
  # !! Keep formula syntax for madrat caching to work
  purrr::pmap(l, ~calcOutput("InternalGDPpc", aggregate = FALSE, supplementary = TRUE, ...)) %>%
    toolReduce()
}

######################################################################################
# Internal Function
######################################################################################
calcInternalGDPpc <- function(GDPpcCalib,    # nolint
                              GDPpcPast,     # nolint
                              GDPpcFuture,   # nolint
                              unit,
                              extension2150,
                              FiveYearSteps, # nolint
                              average2020,
                              naming) {
   # GDPpc scenarios are constructed in PPPs. If MERs are desired, scenarios with the
   # same base year but in PPPs are constructed, and converted to MERs at the end.
  if (grepl("^constant .* US\\$MER$", unit)) {
    constructUnit <- paste0("constant ",  substr(unit, 10, 13), " Int$PPP")
  } else {
    constructUnit <- unit
  }

  # Depending on the chosen GDPpcCalib, the harmonization function either requires 'past' and
  # 'future' GDPpc scenarios, OR NOT, which is the case for "calibSDPs" for example, where
  # the computations are done based off of the combined SSP1 GDPpc scenario.
  if (GDPpcCalib %in% c("calibSSPs", "calibNoCovid")) {
    # Compute "past" and "future" time series.
    past <- calcOutput("GDPpcPast",
                       GDPpcPast = GDPpcPast,
                       unit = constructUnit,
                       aggregate = FALSE)
    future <- calcOutput("GDPpcFuture",
                         GDPpcFuture = GDPpcFuture,
                         unit = constructUnit,
                         extension2150 = "none",
                         aggregate = FALSE)
  } else {
    # Save arguments as list.
    args <- as.list(environment())
  }

  # Combine "past" and "future" time series.
  combined <- switch(
    GDPpcCalib,
    "calibSSPs"       = toolGDPpcHarmonizeSSP(past, future, constructUnit, yEnd = 2100),
    "calibSDPs"       = toolGDPpcHarmonizeSDP(args),
    "calibSSP2EU"     = toolGDPpcHarmonizeSSP2EU(args),
    "calibNoCovid"    = toolGDPpcHarmonizeSSP(past, future, constructUnit, yEnd = 2100, noCovid = TRUE),
    "calibLongCovid"  = toolGDPpcHarmonizeLongCovid(args),
    "calibShortCovid" = toolGDPpcHarmonizeShortCovid(args),
    stop("Bad input for calcGDPpc. Invalid 'GDPpcCalib' argument.")
  )

  # Get description of harmonization function.
  description <- switch(
    GDPpcCalib,
    "calibSSP2EU" = glue("use past data, short term growth rates from IMF and afterwards transition \\
                          between {GDPpcPast} and {GDPpcFuture} with a transition period until 2100. For \\
                          European countries, just glue past with future and after 2070 converge \\
                          to 2150 SSP2 values."),
    "calibNoCovid" = glue("use past data until 2019, short term growth rates from IMF (WEO from Oct2019 - pre Covid) \\
                           and afterwards transition to {GDPpcFuture} by 2100."),
    "covidShortCovid" = glue("use past data, short term growth rates from IMF and afterwards transition to \\
                             noCovid until 2030."),
    "covidLongCovid" = glue("use past data, short term growth rates from IMF and afterwards growth rates from the \\
                             noCovid scenario until 2100."),
    glue("use past data, short term growth rates from IMF and \\
          afterwards transition between {GDPpcPast} and {GDPpcFuture} \\
          with a transition period until 2100")
  )

  # Apply finishing touches to combined time-series
  combined <- toolFinishingTouches2(combined, extension2150, FiveYearSteps, naming, unit, constructUnit, average2020)

  # Get weight
  if (grepl("-MI$", GDPpcPast)) {
    h1 <- sub("-MI", "-UN_PopDiv-MI", GDPpcPast)
  } else {
    h1 <- GDPpcPast
  }
  if (grepl("-MI$", GDPpcFuture)) {
    h2 <- sub("-MI", "-UN_PopDiv-MI", GDPpcFuture)
  } else {
    h2 <- GDPpcFuture
  }
  weight <- calcOutput("Population",
                       PopulationCalib = GDPpcCalib,
                       PopulationPast = h1,
                       PopulationFuture = h2,
                       FiveYearSteps = FiveYearSteps,
                       extension2150 = extension2150,
                       naming = naming,
                       aggregate = FALSE)
  # Give weight same names as data, so that aggregate doesn't mess up data dim
  getNames(weight) <- gsub("pop", "gdppc", getNames(weight))
  # Make sure weight has the same yearly resolution as combined (this relates specifically to the noCovid scenario)
  weight <- weight[, getYears(combined), ]

  list(x = combined,
       weight = weight,
       unit = unit,
       description = glue("Datasource for the Past: {GDPpcPast}. Datasource for the Future: {GDPpcFuture}. Calibrated \\
                          to {description}."))
}



######################################################################################
# GDPpc Harmonization Functions
######################################################################################
toolGDPpcHarmonizeSSP <- function(pastGDPpc, futureGDPpc, unit, yEnd, noCovid = FALSE) {

  if (!noCovid) {
    # Get IMF short-term income projections and fill missing with SSP2
    imfGDPpc <- readSource("IMF", "GDPpc")
  } else{
    # noCovid = TRUE leads to a counterfactual scenario where no Covid shock is experienced
    ## Use past data only until 2019
    pastGDPpc <- pastGDPpc[, getYears(pastGDPpc, as.integer = T)[getYears(pastGDPpc, as.integer = T) <= 2019], ]
    ## Get pre-covid IMF short-term income projections and fill missing with SSP2
    imfGDPpc <- readSource("IMF", "GDPpc", "WEOallOct2019.xls")
  }

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

  # Make sure to add the last IMF year to the future SSP data, just in case
  # it's not there. That is the year from which convergence begins.
  yStart <- max(getYears(imfGDPpc, as.integer = TRUE))
  futureGDPpc <- futureGDPpc %>%
    magclass::time_interpolate(yStart, integrate_interpolated_years = TRUE) %>%
    as.data.frame(rev = 2) %>%
    tibble::as_tibble() %>%
    dplyr::select("iso3c", "year", "variable", "value" = ".value")

  combinedGDPpc <- tidyr::expand_grid(iso3c = unique(tmpGDPpc$iso3c),
                                      year = unique(c(tmpGDPpc$year, futureGDPpc$year)),
                                      variable = unique(futureGDPpc$variable)) %>%
    dplyr::left_join(tmpGDPpc, by = c("iso3c", "year")) %>%
    dplyr::left_join(dplyr::select(futureGDPpc, .data$iso3c, .data$year, .data$variable, "iiasa_gdppc" = .data$value),
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

toolGDPpcHarmonizeSDP <- function(args) {

  gdppcapSSP1 <- calcOutput("GDPpc",
                             GDPpcCalib  = "calibSSPs",
                             GDPpcPast   = args$GDPpcPast,
                             GDPpcFuture = "SSPs-MI",
                             unit = args$unit,
                             extension2150 = "none",
                             FiveYearSteps = FALSE,
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

toolGDPpcHarmonizeSSP2EU <- function(args) {
  gdp <- calcOutput("GDP",
                    GDPCalib = args$GDPpcCalib,
                    GDPPast = args$GDPpcPast,
                    GDPFuture = args$GDPpcFuture,
                    unit = args$unit,
                    extension2150 = "none",
                    FiveYearSteps = FALSE,
                    average2020 = FALSE,
                    aggregate = FALSE)

  if (grepl("-MI$", args$GDPpcPast)) {
    h1 <- sub("-MI", "-UN_PopDiv-MI", args$GDPpcPast)
  } else {
    h1 <- args$GDPpcPast
  }
  if (grepl("-MI$", args$GDPpcFuture)) {
    h2 <- sub("-MI", "-UN_PopDiv-MI", args$GDPpcFuture)
  } else {
    h2 <- args$GDPpcFuture
  }
  pop <- calcOutput("Population",
                    PopulationCalib = args$GDPpcCalib,
                    PopulationPast = h1,
                    PopulationFuture = h2,
                    extension2150 = "none",
                    FiveYearSteps = FALSE,
                    aggregate = FALSE)

  getNames(gdp) <- getNames(pop) <- gsub("pop", "gdppc", getNames(pop))
  gdp / pop
}

toolGDPpcHarmonizeShortCovid <- function(args) {

  gdppcSSPs <- calcOutput("GDPpc",
                          GDPpcCalib  = "calibSSPs",
                          GDPpcPast   = args$GDPpcPast,
                          GDPpcFuture = args$GDPpcFuture,
                          unit = args$unit,
                          extension2150 = "none",
                          FiveYearSteps = FALSE,
                          average2020 = FALSE,
                          aggregate = FALSE)

  gdppcNoCovid <- calcOutput("GDPpc",
                             GDPpcCalib  = "calibNoCovid",
                             GDPpcPast   = args$GDPpcPast,
                             GDPpcFuture = args$GDPpcFuture,
                             unit = args$unit,
                             extension2150 = "none",
                             FiveYearSteps = FALSE,
                             average2020 = FALSE,
                             aggregate = FALSE)

  # Use SSPs until the last year of the IMF predictions, afterwards converge to NoCovid by 2030
  yIMF <- max(getYears(readSource("IMF", "GDPpc"), as.integer = TRUE))
  gdppcSSPs <- gdppcSSPs[, getYears(gdppcSSPs, as.integer = T)[getYears(gdppcSSPs, as.integer = T) <= yIMF], ]
  mbind(purrr::map(1:5, ~toolHarmonizePastTransition(gdppcSSPs[, , .x], gdppcNoCovid[, , .x], yEnd = 2030)))
}

toolGDPpcHarmonizeLongCovid <- function(args) {

  gdppcSSPs <- calcOutput("GDPpc",
                          GDPpcCalib  = "calibSSPs",
                          GDPpcPast   = args$GDPpcPast,
                          GDPpcFuture = args$GDPpcFuture,
                          unit = args$unit,
                          extension2150 = "none",
                          FiveYearSteps = FALSE,
                          average2020 = FALSE,
                          aggregate = FALSE)

  gdppcNoCovid <- calcOutput("GDPpc",
                             GDPpcCalib  = "calibNoCovid",
                             GDPpcPast   = args$GDPpcPast,
                             GDPpcFuture = args$GDPpcFuture,
                             unit = args$unit,
                             extension2150 = "none",
                             FiveYearSteps = FALSE,
                             average2020 = FALSE,
                             aggregate = FALSE)

  # Use SSPs until the last year of the IMF predictions, afterwards use NoCovid growth rates
  yIMF <- max(getYears(readSource("IMF", "GDPpc"), as.integer = TRUE))
  gdppcSSPs <- gdppcSSPs[, getYears(gdppcSSPs, as.integer = T)[getYears(gdppcSSPs, as.integer = T) <= yIMF], ]
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
