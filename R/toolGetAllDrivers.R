#' Get all drivers
#'
#' Return a list of all drivers, with data on the region- and country-level.
#'
#' @inheritParams calcGDP
#' @param version version tag to add to the file name.
#'
#' @returns A list of magpie objects
#' @keywords internal
toolGetAllDrivers <- function(version = "current", scenario = "SSPs") {
  if (version != "current") {
    return(readr::read_rds(glue::glue("compare_versions/scenarios/{tolower(scenario)}_v{version}.Rds")))
  }

  list(
    "regions" = list(
      "pop" = calcOutput("Population", scenario = scenario),
      "gdp" = calcOutput("GDP", scenario = scenario, average2020 = FALSE),
      "gdppc" = calcOutput("GDPpc", scenario = scenario, average2020 = FALSE),
      "lab" = calcOutput("Labour", scenario = scenario),
      "urb" = calcOutput("Urban", scenario = scenario)
    ),
    "countries" = list(
      "pop" = calcOutput("Population", scenario = scenario, aggregate = FALSE),
      "gdp" = calcOutput("GDP", scenario = scenario, aggregate = FALSE, average2020 = FALSE),
      "gdppc" = calcOutput("GDPpc", scenario = scenario, aggregate = FALSE, average2020 = FALSE),
      "lab" = calcOutput("Labour", scenario = scenario, aggregate = FALSE),
      "urb" = calcOutput("Urban", scenario = scenario, aggregate = FALSE)
    )
  )
}

#' Save all drivers as rds file
#'
#' Save all drivers as rds file.
#'
#' @inheritParams toolGetAllDrivers
#'
#' @returns A list of magpie objects
#' @keywords internal
toolSaveAllDrivers <- function(version, scenario = "SSPs") {
  x <- readr::write_rds(toolGetAllDrivers(version = "current", scenario = scenario),
                        glue::glue("compare_versions/scenarios/{tolower(scenario)}_v{version}.Rds"))
  invisible(x)
}
