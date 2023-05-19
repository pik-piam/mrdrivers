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
#' toolGetScenarioDefinition(scen = "SSP2EU")
#' toolGetScenarioDefinition(driver = "Population", scen = "SSPs", aslist = TRUE)
#'
toolGetScenarioDefinition <- function(driver = NULL, scen = NULL, aslist = FALSE) {

  # Start of scenario-design section: Developers can modify this section!
  scenarios <- tibble::tribble(
    ~driver,      ~scenario,    ~pastData,                    ~futureData,            ~harmonization,
    # GDPpc scenarios
    "GDPpc",      "SSPs",       "WDI-MI",                     "SSPsOld-MI",           "calibSSPs",
    "GDPpc",      "SDPs",       "-",                          "-",                    "calibSDPs",
    "GDPpc",      "SSP2EU",     "-",                          "-",                    "GDPoverPop",
    "GDPpc",      "noCovid",    "WDI-MI",                     "SSPsOld-MI",           "calibNoCovid",
    "GDPpc",      "longCovid",  "-",                          "-",                    "calibLongCovid",
    "GDPpc",      "shortCovid", "-",                          "-",                    "calibShortCovid",
    "GDPpc",      "SSPsOld",    "-",                          "-",                    "GDPoverPop",
    # GDP scenarios
    "GDP",        "SSPs",       "-",                          "-",                    "GDPpcWithPop",
    "GDP",        "SDPs",       "-",                          "-",                    "GDPpcWithPop",
    "GDP",        "SSP2EU",     "Eurostat-WDI-MI",            "SSP2EU-MI",            "calibSSP2EU",
    "GDP",        "noCovid",    "-",                          "-",                    "GDPpcWithPop",
    "GDP",        "longCovid",  "-",                          "-",                    "GDPpcWithPop",
    "GDP",        "shortCovid", "-",                          "-",                    "GDPpcWithPop",
    "GDP",        "SSPsOld",    "IHME_USD05_PPP_pc-MI",       "SSPs-MI",              "past_transition",
    # Population Scenarios
    "Population", "SSPs",       "WDI-UN_PopDiv-MI",           "SSPs-UN_PopDiv-MI",    "withPEAPandFuture",
    "Population", "SDPs",       "WDI-UN_PopDiv-MI",           "SDPs-UN_PopDiv-MI",    "withPEAPandFuture",
    "Population", "SSP2EU",     "Eurostat-WDI-UN_PopDiv-MI",  "SSP2EU-UN_PopDiv-MI",  "calibSSP2EU",
    "Population", "ISIMIP",     "UN_PopDiv-MI",               "SSPs-UN_PopDiv-MI",    "calibISIMIP",
    "Population", "SSPsOld",    "WDI-MI",                     "SSPsOld-MI",           "past_transition",
    "Population", "noCovid",    "WDI-UN_PopDiv-MI",           "SSPs-UN_PopDiv-MI",    "withPEAPandFuture",
    "Population", "longCovid",  "WDI-UN_PopDiv-MI",           "SSPs-UN_PopDiv-MI",    "withPEAPandFuture",
    "Population", "shortCovid", "WDI-UN_PopDiv-MI",           "SSPs-UN_PopDiv-MI",    "withPEAPandFuture",
    # Labour Scenarios
    "Labour",     "SSPs",       "-",                          "SSPs",                 "-",
    "Labour",     "SDPs",       "-",                          "SDPs",                 "-",
    "Labour",     "SSP2EU",     "-",                          "SSP2EU",               "-",
    "Labour",     "SSPsOld",    "-",                          "SSPsOld",              "-",
    # Urban population scenarios
    "Urban",      "SSPs",       "WDI",                        "SSPs",                 "past",
    "Urban",      "SDPs",       "WDI",                        "SDPs",                 "past",
    "Urban",      "SSP2EU",     "WDI",                        "SSP2EU",               "past"
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
