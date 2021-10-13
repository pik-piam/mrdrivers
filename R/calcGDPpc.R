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
#' calcOutput("GDPpc")}
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
  # Call internal_calcGDPpc function the appropriate number of times
  toolInternalCalc("GDPpc", as.list(environment()))
}

######################################################################################
# Internal Function
######################################################################################
internal_calcGDPpc <- function(GDPpcCalib,
                               GDPpcPast,
                               GDPpcFuture,
                               unit,
                               extension2150,
                               FiveYearSteps,
                               naming){
  # Depending on the chosen GDPpcCalib, the harmonization function either requires 'past' and
  # 'future' GDPpc scenarios, OR NOT, which is the case for "calibSDPs" for example, where
  # the computations are done based off of the combined SSP1 GDPpc scenario.

  if (GDPpcCalib == "calibSSPs") {
    # Compute "past" and "future" time series.
    past <- calcOutput("GDPpcPast",
                       GDPpcPast = GDPpcPast,
                       unit = unit,
                       aggregate = FALSE)
    future <- calcOutput("GDPpcFuture",
                         GDPpcFuture = GDPpcFuture,
                         unit = unit,
                         extension2150 = "none",
                         aggregate = FALSE)
  } else {
    # Save arguments as list.
    args <- as.list(environment())
  }

  # Combine "past" and "future" time series.
  combined <- switch(
    GDPpcCalib,
    "calibSSPs"   = gdppcHarmonizeSSP(past, future, unit, yEnd = 2100),
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
  combined <- toolFinishingTouches(combined, extension2150, FiveYearSteps, naming)

  # Get weigth
  weight <- calcOutput("Population", 
                       PopulationCalib = GDPpcCalib,
                       PopulationPast = GDPpcPast, 
                       PopulationFuture = GDPpcFuture,
                       FiveYearSteps = FiveYearSteps,
                       extension2150 = extension2150,
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
gdppcHarmonizeSSP <- function(past_gdppc, future_gdppc, unit, yEnd) {
  # Get IMF short-term income projcetions and fill missing with SSP2
  imf_gdppc <- readSource("IMF", "GDPpc")
  fill <- calcOutput("GDPpcFuture", 
                     GDPpcFuture = "SSPs-MI", 
                     unit = unit,
                     extension2150 = "none",
                     aggregate = FALSE)[,, "gdppc_SSP2"]
  imf_gdppc <- imf_gdppc %>% 
    toolFillWith(fill) %>% 
    toolInterpolateAndExtrapolate()

  # Use short term IMF growth rates (here, as far as possible = 2026)
  tmp_gdppc <- toolHarmonizePastGrFuture(past_gdppc, imf_gdppc)

  # Transform into tibble, combine past and future tibbles
  tmp_gdppc <- tmp_gdppc %>%
    as.data.frame(rev = 2) %>% 
    tibble::as_tibble() %>% 
    dplyr::select("iso3c", "year", "value" = ".value") %>% 
    dplyr::filter(.data$year != 2026)

  future_gdppc <- future_gdppc %>% 
    as.data.frame(rev = 2) %>% 
    tibble::as_tibble() %>% 
    dplyr::select("iso3c", "year", "variable", "value" = ".value")

  combined_gdppc <- tidyr::expand_grid(iso3c = unique(tmp_gdppc$iso3c),
                                       year = unique(c(tmp_gdppc$year, future_gdppc$year)),
                                       variable = unique(future_gdppc$variable)) %>%
    dplyr::left_join(tmp_gdppc, by = c("iso3c", "year")) %>%
    dplyr::left_join(future_gdppc %>%
                dplyr::select(.data$iso3c, .data$year, .data$variable, "iiasa_gdppc" = .data$value),
              by = c("iso3c", "year", "variable")) %>%
    dplyr::rename("SSP" = .data$variable) %>%
    dplyr::mutate(SSP = sub("^......", "", .data$SSP))

  # Pass to special convergence function
  combined_gdppc <- convergeSpecial(combined_gdppc)

  # Retransform into magpie
  combined_gdppc <- combined_gdppc %>%
    dplyr::mutate(SSP = paste0("gdppc_", .data$SSP)) %>%
    dplyr::select(.data$iso3c, .data$year, "variable" = .data$SSP, .data$value) %>%
    as.magpie()

  combined_gdppc
}


gdppcHarmonizeSDP <- function(args) {

  gdppcap_SSP1 <- calcOutput("GDPpc",
                             GDPpcCalib  = "calibSSPs",
                             GDPpcPast   = args$GDPpcPast,
                             GDPpcFuture = "SSPs-MI",
                             unit = args$unit,
                             extension2150 = "none",
                             FiveYearSteps = FALSE,
                             aggregate = FALSE)[,, "gdppc_SSP1"]

  # standard SDP inherits SSP1 GDP
  gdppcap_SDP <- gdppcap_SSP1
  getNames(gdppcap_SDP) <- gsub("SSP1", "SDP", getNames(gdppcap_SDP))
  # SHAPE SDP_XX variants are calculated as modifications of SSP1 GDP/cap growth rates
  combined <- purrr::map(c("gdppc_SDP_EI", "gdppc_SDP_MC", "gdppc_SDP_RC"),
                         compute_SHAPE_growth,
                         gdppcap_SSP1 = gdppcap_SSP1,
                         startFromYear = 2020) %>%
    mbind() %>%
    mbind(gdppcap_SDP)

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
  gdppc <- gdp / pop
}


#########################
### Helper functions
#########################
convergeSpecial <- function(x) {

  dif <- x %>%
    dplyr::filter(.data$year == 2025) %>%
    dplyr::mutate(d = .data$iiasa_gdppc - .data$value) %>%
    dplyr::select(.data$iso3c, .data$SSP, .data$d)

  x <- x %>%
    dplyr::left_join(dif, by = c("iso3c", "SSP")) %>%
    dplyr::mutate(
      # Medium convergence
      value = dplyr::if_else(
        .data$year > 2025 & .data$SSP == "SSP2",
        dplyr::if_else(
          .data$year == 2030,
          .data$iiasa_gdppc - .data$d,
          dplyr::if_else(
            .data$year <= 2100,
            .data$iiasa_gdppc - .data$d * (2100 - .data$year) / 70,
            .data$iiasa_gdppc
          )
        ),
        .data$value
      ),
      # Fast convergence
      value = dplyr::if_else(
         .data$year > 2025 &
            ((.data$SSP %in% c("SSP1", "SSP5") & .data$d >= 0) |
             (.data$SSP %in% c("SSP3", "SSP4") & .data$d < 0)),
         dplyr::if_else(
             .data$year <= 2100,
             .data$iiasa_gdppc - .data$d * (2100 - .data$year) / 75,
             .data$iiasa_gdppc
         ),
         .data$value
      ),
      # Slow convergence
      value = dplyr::if_else(
        .data$year > 2025 &
            ((.data$SSP %in% c("SSP3", "SSP4") & .data$d >=0 ) |
             (.data$SSP %in% c("SSP1", "SSP5") & .data$d < 0)),
        dplyr::if_else(
            .data$year <= 2035,
            .data$iiasa_gdppc - .data$d,
            dplyr::if_else(
              .data$year <= 2100,
              .data$iiasa_gdppc - .data$d * (2100 - .data$year) / 65,
              .data$iiasa_gdppc
            )
        ),
        .data$value
      ),
      # Add a minimum value for value here. This can occur when the d computed in 2025 is 
      # so large relative to the original GDPpc value in 2025, that a further dercease in the
      # years 2030 and 2035 pushes the GDPpc into the negative.
      value = pmax(.data$value, 0.01)
    ) %>%
    dplyr::select(-.data$d)
  x
}

###########################################
#Additional functions to derive the SHAPE GDP scenarios from the SSP1 scenario

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
compute_SHAPE_growth <- function(SHAPE_GDPscenario, gdppcap_SSP1, startFromYear){
  
  # calculation of growth rates
  yrs <- getYears(gdppcap_SSP1, as.integer = TRUE)
  # flexible timestep
  # TODO would be better to always use yearly timesteps in this computation, and select years afterwards
  timestep <- new.magpie(years = yrs)
  timestep[,,] <- dplyr::lead(yrs) - yrs
  # assign average growth rate g_t of period t -> t+ timestep
  # this means modifications of growth rate t will affect GDP in t+timestep
  yrs_shifted <- yrs[2:length(yrs)]
  yrs_base <- yrs[1:length(yrs)-1]
  growthrate_SSP1 <- 100* ((setYears(gdppcap_SSP1[,yrs_shifted,],yrs_base)/gdppcap_SSP1[,yrs_base,])^(1./timestep[,yrs_base,]) - 1)
  
  #modified growth rates and gdp/cap
  growthrate <- setNames(as.magpie(growthrate_SSP1),SHAPE_GDPscenario)
  gdppcap <- setNames(as.magpie(gdppcap_SSP1),SHAPE_GDPscenario)
  gdppcap[,yrs > startFromYear,] <- NA
  
  for (yr in yrs[1:length(yrs)-1]){
    # modify growth rates only for future period (default: from 2020 onwards) 
    if (yr >= startFromYear){
      # innovation-driven (SDP_EI): enhance growth rates for low-income countries by up to 15%
      if (SHAPE_GDPscenario == "gdppc_SDP_EI"){
        modification_factor <- logistic_transition(gdppcap[,yr,], L0 = 1.15, L = 1, k = 20, x0 = 15e3, use_log10 = TRUE)
      }
      # service-driven (SDP_MC): growth rate reduced based on relative distance to technology frontier (given by the US)
      else if (SHAPE_GDPscenario == "gdppc_SDP_MC"){
        # define US as technology frontier
        frontier <- gdppcap["USA",yr,]
        getItems(frontier, 1) <- "GLO"
        # countries with gdp/cap above US are treated the same as the US -> set diff = 0
        reldiff_to_frontier <- pmax((frontier[,yr,] - gdppcap[,yr,])/frontier[,yr,] , 0)
        modification_factor <- logistic_transition(reldiff_to_frontier[,yr,], L0 = 1, L = 0.5, k = -30, x0 = 0.2, use_log10 = FALSE)
      } 
      # society-driven (SDP_RC): gradual transition to zero growth for high-income countries
      else if (SHAPE_GDPscenario == "gdppc_SDP_RC") {
        modification_factor <- logistic_transition(gdppcap[,yr,], L0 = 1, L = 0, k = 10, x0 = 30e3, use_log10 = TRUE)
      } else {
        stop("cannot create SHAPE GDP scenarios: unknown scenario")
      }
      
      # for service (SDP_MC) and society (SDP_RC) additionally add a smoothing for 2020 and 2025 timesteps
      # apply only 1/3 (2020-2024) and 2/3 (2025-2029) of the modification
      if (SHAPE_GDPscenario %in% c("gdppc_SDP_MC","gdppc_SDP_RC")){
        if (yr >= 2020 && yr < 2025){
          modification_factor[,yr,] <- 1/3.*(modification_factor[,yr,] - 1) + 1
        } else if (yr >= 2025 && yr < 2030) {
          modification_factor[,yr,] <- 2/3.*(modification_factor[,yr,] - 1) + 1
        }
      }
      growthrate[,yr,] <- growthrate[,yr,] * modification_factor[,yr,]
    }
    
    # calculate next gdp/cap based on current value and (modified) growth rate
    gdppcap[,yr+as.integer(timestep[,yr,]),] <- gdppcap[,yr,]*(1 + growthrate[,yr,]/100.)^timestep[,yr,]
  }
  return(gdppcap)
}

# helper function: smooth transition from LO to L, with steepness k and midpoint x0
logistic_transition <- function(x,L0,L,k,x0, use_log10 = FALSE){
  if (use_log10){
    x <- log10(x)
    x0 <- log10(x0)
  }
  logistic <- 1./(1+exp(-k*(x-x0)))
  return( L0 - (L0-L)*logistic )
} 
