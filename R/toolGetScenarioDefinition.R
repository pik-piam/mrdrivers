#' Get information on available scenarios
#'
#' toolGetScenarioDefinition can be used to figure out which scenarios are made available by mrdrivers, and how they
#' are constructed, i.e. what xPast, xFuture and xCalib strings are used.
#'
#' @param driver NULL or a character vector designating the driver for which information is to be returned. If NULL,
#'  information for all drivers is returned. Available drivers are "GDP", "Population" and "GDPpc".
#'
#' @param scen NULL or a character vector designating the scenario for which information is to be returned. If NULL,
#'  information for all scenarios is returned. Available scenarios are:
#'  \itemize{
#'   \item GDP-scenarios: "SSPs", "SDPs", and "SSP2EU"
#'   \item Populations-scenarios: "SSPs", "SDPs", "SSP2EU" and "ISIMIP"
#'   \item GDPpc-scenarios: "SSPs", "SDPs", and "SSP2EU"
#'  }
#'
#' @param unlist TRUE or FALSE (default). If TRUE the xCalib, xPast and xFuture entries are returned directly.
#'
#' @return A list with the scenarios and their xPast, xFuture and xCalib strings.
#' @export
#'
#' @examples
#' toolGetScenarioDefinition()
#' toolGetScenarioDefinition(driver = "GDP")
#' toolGetScenarioDefinition(scen = "SSP2EU")
#' toolGetScenarioDefinition(driver = "Population", scen = "SSPs", unlist = TRUE)
#'
toolGetScenarioDefinition <- function(driver = NULL, scen = NULL, unlist = FALSE) {

  # Start of scenario-design section: Developers can modify this section!

  ## GDPpc scenarios
  gdppcScenarios <- tibble::tribble(
    ~scenario,    ~GDPpcPast,             ~GDPpcFuture,  ~GDPpcCalib,
    "SSPs",       "WDI-MI",               "SSPs-MI",     "calibSSPs",
    "SDPs",       "WDI-MI",               "SDPs-MI",     "calibSDPs",
    "SSP2EU",     "Eurostat-WDI-MI",      "SSP2EU-MI",   "calibSSP2EU",
    "noCovid",    "WDI-MI",               "SSPs-MI",     "calibNoCovid",
    "longCovid",  "WDI-MI",               "SSPs-MI",     "calibLongCovid",
    "shortCovid", "WDI-MI",               "SSPs-MI",     "calibShortCovid"
  )

  ## Population scenarios
  popScenarios <- tibble::tribble(
    ~scenario,   ~PopulationPast,               ~PopulationFuture,      ~PopulationCalib,
    "SSPs",      "WDI-UN_PopDiv-MI",            "SSPs-UN_PopDiv-MI",    "calibSSPs",
    "SDPs",      "WDI-UN_PopDiv-MI",            "SDPs-UN_PopDiv-MI",    "calibSDPs",
    "SSP2EU",    "Eurostat-WDI-UN_PopDiv-MI",   "SSP2EU-UN_PopDiv-MI",  "calibSSP2EU",
    "ISIMIP",    "UN_PopDiv-MI",                "SSPs-UN_PopDiv-MI",    "calibISIMIP",
    "SSPsOld",   "WDI-MI",                      "SSPs_old-MI",          "past_transition"
  )

  ## GDP scenarios
  gdpScenarios <- tibble::tribble(
    ~scenario,    ~GDPPast,                 ~GDPFuture,    ~GDPCalib,
    "SSPsOld",    "IHME_USD05_PPP_pc-MI",   "SSPs-MI",     "past_transition",
  )
  # GDP scenarios created by multiplying GDPpc and Population
  gdpScenarios <- dplyr::bind_rows(gdpScenarios, dplyr::rename_with(gdppcScenarios, ~gsub("GDPpc", "GDP", .x)))


  s <- list("GDP" = gdpScenarios,
            "Population" = popScenarios,
            "GDPpc" = gdppcScenarios)
  # End of scenario-design section


  availableDrivers <- names(s)
  if (!is.null(driver)) {
    if (!all(driver %in% availableDrivers)) {
      stop(glue::glue("Unknown driver. Available drivers are: {paste(availableDrivers, collapse = ', ')}"))
    }
    s <- s[driver]
  }

  if (!is.null(scen)) {
    availableScen <- purrr::map(s, dplyr::pull, "scenario") %>% purrr::reduce(c) %>% unique()
    if (!all(scen %in% availableScen)) {
      stop(glue::glue("Unknown scenario. Available scenarios are: {paste(availableScen, collapse = ', ')}"))
    }
    s <- purrr::map(s, ~.x %>%
                      dplyr::filter(.x$scenario %in% scen) %>%
                      dplyr::arrange(order(match(.$scenario, scen)))) %>%
      purrr::discard(~dim(.x)[1] == 0)
  }

  if (unlist) {
    s <- purrr::map(s, ~dplyr::select(.x, -"scenario")) %>% unlist(recursive = FALSE)
    names(s) <- gsub(paste0("(", paste0(availableDrivers, collapse = "|"), ")\\."), "", names(s))
  }

  s
}
