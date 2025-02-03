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
    ~driver,      ~scenario,         ~pastData,             ~futureData,              ~harmonization,
    # GDPpc scenarios
    "GDPpc",      "SSPs",            "WDI-MI-James",        "SSPs",                   "GDPpcSSPs",
    "GDPpc",      "SSP1",            "WDI-MI-James",        "SSP1",                   "GDPpcSSPs",
    "GDPpc",      "SSP2",            "WDI-MI-James",        "SSP2",                   "GDPpcSSPs",
    "GDPpc",      "SSP3",            "WDI-MI-James",        "SSP3",                   "GDPpcSSPs",
    "GDPpc",      "SSP4",            "WDI-MI-James",        "SSP4",                   "GDPpcSSPs",
    "GDPpc",      "SSP5",            "WDI-MI-James",        "SSP5",                   "GDPpcSSPs",
    "GDPpc",      "SSP2EU",          "WDI-MI-James",        "SSP2EU",                 "GDPpcSSPs",
    "GDPpc",      "SDPs",            "-",                   "-",                      "GDPpcSDPs",
    "GDPpc",      "ISIMIP",          "WDI-MI-James",        "SSPs",                   "GDPpcSSPs",
    "GDPpc",      "SSP2IndiaDEAs",   "WDI-MI-James",        "IndiaDEAs-SSP2",         "GDPpcSSP2IndiaDEAs",
    "GDPpc",      "SSP2IndiaMedium", "WDI-MI-James",        "IndiaDEAbase-SSP2",      "GDPpcSSP2IndiaDEAs",
    "GDPpc",      "SSP2IndiaHigh",   "WDI-MI-James",        "IndiaDEAopt-SSP2",       "GDPpcSSP2IndiaDEAs",
    # GDP scenarios
    "GDP",        "SSPs",            "-",                   "-",                      "GDPpcWithPop",
    "GDP",        "SSP1",            "-",                   "-",                      "GDPpcWithPop",
    "GDP",        "SSP2",            "-",                   "-",                      "GDPpcWithPop",
    "GDP",        "SSP3",            "-",                   "-",                      "GDPpcWithPop",
    "GDP",        "SSP4",            "-",                   "-",                      "GDPpcWithPop",
    "GDP",        "SSP5",            "-",                   "-",                      "GDPpcWithPop",
    "GDP",        "SSP2EU",          "-",                   "-",                      "GDPpcWithPop",
    "GDP",        "SDPs",            "-",                   "-",                      "GDPpcWithPop",
    "GDP",        "ISIMIP",          "-",                   "-",                      "GDPpcWithPop",
    "GDP",        "SSP2IndiaDEAs",   "-",                   "-",                      "GDPpcWithPop",
    "GDP",        "SSP2IndiaMedium", "-",                   "-",                      "GDPpcWithPop",
    "GDP",        "SSP2IndiaHigh",   "-",                   "-",                      "GDPpcWithPop",
    # Population Scenarios
    "Population", "SSPs",            "WDI-UN_PopDiv-MI",    "SSPs-UN_PopDiv",              "PopSSPs",
    "Population", "SSP1",            "WDI-UN_PopDiv-MI",    "SSP1-UN_PopDiv",              "PopSSPs",
    "Population", "SSP2",            "WDI-UN_PopDiv-MI",    "SSP2-UN_PopDiv",              "PopSSPs",
    "Population", "SSP3",            "WDI-UN_PopDiv-MI",    "SSP3-UN_PopDiv",              "PopSSPs",
    "Population", "SSP4",            "WDI-UN_PopDiv-MI",    "SSP4-UN_PopDiv",              "PopSSPs",
    "Population", "SSP5",            "WDI-UN_PopDiv-MI",    "SSP5-UN_PopDiv",              "PopSSPs",
    "Population", "SSP2EU",          "WDI-UN_PopDiv-MI",    "SSP2EU-UN_PopDiv",            "PopSSPs",
    "Population", "SDPs",            "WDI-UN_PopDiv-MI",    "SDPs-UN_PopDiv",              "PopSSPs",
    "Population", "ISIMIP",          "UN_PopDiv-MI",        "SSPs-UN_PopDiv",              "PopISIMIP",
    "Population", "SSP2IndiaDEAs",   "WDI-UN_PopDiv-MI",    "IndiaDEAs-SSP2-UN_PopDiv",    "PopSSP2IndiaDEAs",
    "Population", "SSP2IndiaMedium", "WDI-UN_PopDiv-MI",    "IndiaDEAbase-SSP2-UN_PopDiv", "PopSSP2IndiaDEAs",
    "Population", "SSP2IndiaHigh",   "WDI-UN_PopDiv-MI",    "IndiaDEAopt-SSP2-UN_PopDiv",  "PopSSP2IndiaDEAs",
    # Labour Scenarios
    "Labour",     "SSPs",            "WDI-UN_PopDiv",       "SSPs-UN_PopDiv",         "pastAndLevel",
    "Labour",     "SSP1",            "WDI-UN_PopDiv",       "SSP1-UN_PopDiv",         "pastAndLevel",
    "Labour",     "SSP2",            "WDI-UN_PopDiv",       "SSP2-UN_PopDiv",         "pastAndLevel",
    "Labour",     "SSP3",            "WDI-UN_PopDiv",       "SSP3-UN_PopDiv",         "pastAndLevel",
    "Labour",     "SSP4",            "WDI-UN_PopDiv",       "SSP4-UN_PopDiv",         "pastAndLevel",
    "Labour",     "SSP5",            "WDI-UN_PopDiv",       "SSP5-UN_PopDiv",         "pastAndLevel",
    "Labour",     "SSP2EU",          "WDI-UN_PopDiv",       "SSP2EU-UN_PopDiv",       "pastAndLevel",
    "Labour",     "SDPs",            "WDI-UN_PopDiv",       "SDPs-UN_PopDiv",         "pastAndLevel",
    "Labour",     "SSP2IndiaDEAs",   "-",                   "-",                      "LabourSSP2IndiaDEAs",
    "Labour",     "SSP2IndiaMedium", "-",                   "-",                      "LabourSSP2IndiaDEAs",
    "Labour",     "SSP2IndiaHigh",   "-",                   "-",                      "LabourSSP2IndiaDEAs",
    # Urban population scenarios
    "Urban",      "SSPs",            "WDI",                 "SSPs",                   "pastAndGrowth",
    "Urban",      "SSP1",            "WDI",                 "SSP1",                   "pastAndGrowth",
    "Urban",      "SSP2",            "WDI",                 "SSP2",                   "pastAndGrowth",
    "Urban",      "SSP3",            "WDI",                 "SSP3",                   "pastAndGrowth",
    "Urban",      "SSP4",            "WDI",                 "SSP4",                   "pastAndGrowth",
    "Urban",      "SSP5",            "WDI",                 "SSP5",                   "pastAndGrowth",
    "Urban",      "SSP2EU",          "WDI",                 "SSP2EU",                 "pastAndGrowth",
    "Urban",      "SDPs",            "WDI",                 "SDPs",                   "pastAndGrowth",
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
