#' Get information on available scenarios
#'
#' toolGetScenarioDefinition can be used to figure out which scenarios are made available by mrdrivers, and how they
#' are constructed, i.e. what past data, future data and harmonization methods are used.
#'
#' @param driver NULL or a character vector designating the driver for which information is to be returned. If NULL,
#'  information for all drivers is returned. Available drivers are:
#'  \itemize{
#'   \item GDP
#'   \item Population
#'   \item GDPpc
#'   \item Labour
#'   \item Urban
#'  }
#'
#' @param scen NULL or a character vector designating the scenario for which information is to be returned. If NULL,
#' information for all scenarios is returned.
#'
#' @param aslist TRUE or FALSE (default). If TRUE then the pastData, futureData and harmonization strings are returned
#' as a list.
#'
#' @return A tibble with the driver and scenario information.
#' @export
#'
#' @examples
#' toolGetScenarioDefinition()
#' toolGetScenarioDefinition(driver = "GDP")
#' toolGetScenarioDefinition(scen = "SSP2")
#' toolGetScenarioDefinition(driver = "Population", scen = "SSPs", aslist = TRUE)
#'
toolGetScenarioDefinition <- function(driver = NULL, scen = NULL, aslist = FALSE) {

  # Start of scenario-design section: Developers can modify this section!
  scenarios <- tibble::tribble(
    ~driver,      ~scenario,    ~pastData,                 ~futureData,              ~harmonization,
    # GDPpc scenarios
    "GDPpc",      "SSPs",       "WDI-MI-James",            "SSPs",                   "GDPpcSSPs",
    "GDPpc",      "SSP2",       "WDI-MI-James",            "SSP2",                   "GDPpcSSPs",
    "GDPpc",      "SSP2EU",     "WDI-MI-James",            "SSP2EU",                 "GDPpcSSPs",
    "GDPpc",      "SDPs",       "-",                       "-",                      "GDPpcSDPs",
    "GDPpc",      "ISIMIP",     "WDI-MI-James",            "SSPs",                   "GDPpcSSPs",
    "GDPpc",      "ADBs",       "WDI-MI-James",            "ADBs-SSP2",              "GDPpcADBs",
    # GDP scenarios
    "GDP",        "SSPs",       "-",                       "-",                      "GDPpcWithPop",
    "GDP",        "SSP2",       "-",                       "-",                      "GDPpcWithPop",
    "GDP",        "SSP2EU",     "-",                       "-",                      "GDPpcWithPop",
    "GDP",        "SDPs",       "-",                       "-",                      "GDPpcWithPop",
    "GDP",        "ISIMIP",     "-",                       "-",                      "GDPpcWithPop",
    "GDP",        "ADBs",       "-",                       "-",                      "GDPpcWithPop",
    # Population Scenarios
    "Population", "SSPs",       "WDI-UN_PopDiv-MI",        "SSPs-UN_PopDiv",         "PopSSPs",
    "Population", "SSP2",       "WDI-UN_PopDiv-MI",        "SSP2-UN_PopDiv",         "PopSSPs",
    "Population", "SSP2EU",     "WDI-UN_PopDiv-MI",        "SSP2EU-UN_PopDiv",       "PopSSPs",
    "Population", "SDPs",       "WDI-UN_PopDiv-MI",        "SDPs-UN_PopDiv",         "PopSSPs",
    "Population", "ISIMIP",     "UN_PopDiv-MI",            "SSPs-UN_PopDiv",         "PopISIMIP",
    "Population", "ADBs",       "WDI-UN_PopDiv-MI",        "ADBs-SSP2-UN_PopDiv",    "PopADBs",
    # Labour Scenarios
    "Labour",     "SSPs",       "WDI-UN_PopDiv",           "SSPs-UN_PopDiv",         "pastAndLevel",
    "Labour",     "SSP2",       "WDI-UN_PopDiv",           "SSP2-UN_PopDiv",         "pastAndLevel",
    "Labour",     "SSP2EU",     "WDI-UN_PopDiv",           "SSP2EU-UN_PopDiv",       "pastAndLevel",
    "Labour",     "SDPs",       "WDI-UN_PopDiv",           "SDPs-UN_PopDiv",         "pastAndLevel",
    "Labour",     "ADBs",       "-",                       "-",                      "LabourADBs",
    # Urban population scenarios
    "Urban",      "SSPs",       "WDI",                     "SSPs",                   "pastAndGrowth",
    "Urban",      "SSP2",       "WDI",                     "SSP2",                   "pastAndGrowth",
    "Urban",      "SSP2EU",     "WDI",                     "SSP2EU",                 "pastAndGrowth",
    "Urban",      "SDPs",       "WDI",                     "SDPs",                   "pastAndGrowth",
  )
  # End of scenario-design section

  s <- scenarios
  if (exists("mrdrivers_scenarios")) {
    message("Also considering user defined scenarios in mrdrivers_scenarios.")
    s <- dplyr::bind_rows(s, get("mrdrivers_scenarios"))
  }

  if (!is.null(driver)) {
    availableDrivers <- dplyr::pull(scenarios, driver) %>% unique()
    if (!all(driver %in% availableDrivers)) {
      stop(glue::glue("Unknown driver. Available drivers are: {paste(availableDrivers, collapse = ', ')}"))
    }
    s <- dplyr::filter(s, driver %in% !!driver)
  }

  if (!is.null(scen)) {
    availableScen <- dplyr::pull(s, .data$scenario) %>% unique()
    if (!all(scen %in% availableScen)) {
      stop(glue::glue("Unknown scenario. Available scenarios are: {paste(availableScen, collapse = ', ')}"))
    }
    s <- dplyr::filter(s, .data$scenario %in% scen)
    s <- dplyr::arrange(s, order(match(s$scenario, scen)))
  }

  if (aslist) {
    s <- as.list(s)
  }

  s
}
