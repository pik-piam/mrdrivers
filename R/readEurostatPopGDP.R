#' Read Eurostat Population and GDP data
#'
#' Download, read and convert Eurostat population and GDP data.
#'
#' @param subtype A string. Available subtypes are:
#' \itemize{
#' \item "population": Population, ref demo_gind
#' \item "population_projections": Population projections, ref proj_19np
#' \item "GDP": GDP, ref nama_10_gdp
#' \item "GDPgr_projections": Projected GDP growth rates, 2023 forecast.
#' }
#' @inherit madrat::readSource return
#' @seealso [madrat::readSource()] and [madrat::downloadSource()]
#' @examples \dontrun{
#' readSource("EurostatPopGDP", subtype = "population")
#' }
#' @order 2
readEurostatPopGDP <- function(subtype) {
  x <- switch(
    subtype,
    "population" = readr::read_rds("demo_gind_num_code_FF.rds"),
    "population_projections" = readr::read_rds("proj_19np_num_code_FF.rds"),
    "GDP" = readr::read_rds("nama_10_gdp_num_code_FF.rds"),
    "GDPgr_projections" = readr::read_csv("Economic-Forecast---Winter-2023.csv", col_types = "cddd") %>%
      tidyr::pivot_longer(2:4, names_to = "time") %>%
      dplyr::rename("geo" = "Category"),
    stop("Bad input for readEurostatPopGDP. Invalid 'subtype' argument.")
  )
  as.magpie(x, spatial = "geo", temporal = "time")
}

#' @rdname readEurostatPopGDP
#' @order 3
#' @param x MAgPIE object returned by readEurostatPopGDP
convertEurostatPopGDP <- function(x, subtype) {
  switch(
    subtype,
    "population"             = convEurostatPopulation(x),
    "population_projections" = convEurostatPopulation(x),
    "GDP"                    = convEurostatGDP(x),
    "GDPgr_projections"      = convEurostatGDPgrProjections(x)
  )
}

convEurostatPopulation <- function(x) {
  # Fix names of sets, and of variable
  x <- collapseDim(x, dim = 3)
  getNames(x) <- "population"
  # Use the "DE_TOT" values for Germany, if they exist (DE_TOT = East + West Germany)
  x["DE", , ] <- if ("DE_TOT" %in% getItems(x, 1)) x["DE_TOT", , ] else x["DE", , ]
  # Drop any countries with more than 2 charachters in their Eurostat identifier. Those are aggregates.
  myCountries <- getItems(x, 1)[purrr::map_lgl(getItems(x, 1), ~ nchar(.x) == 2)]
  x <- x[myCountries, , ]
  # Convert the eurostat countrycodes to iso3c codes
  getItems(x, 1) <- countrycode::countrycode(getItems(x, 1), "eurostat", "iso3c", warn = FALSE)
  # ABOVE Warning: Some values were not matched unambiguously: FX, XK

  toolGeneralConvert(x, note = FALSE)
}

convEurostatGDP <- function(x) {
  # Convert the eurostat countrycodes to iso3c codes
  getItems(x, 1) <- countrycode::countrycode(getItems(x, 1), "eurostat", "iso3c", warn = FALSE)
  # ABOVE warning that is being ignored:
  # Some values were not matched unambiguously: EA, EA12, EA19, EA20, EU15, EU27_2020, EU28, XK

  x <- toolGeneralConvert(x, note = FALSE)

  # Convert from constant 2005 LCU to constant 2005 Int$PPP.
  getNames(x) <- "GDP"
  GDPuc::convertGDP(x, "constant 2005 LCU", "constant 2005 Int$PPP", replace_NAs = c("linear", "no_conversion"))
}

convEurostatGDPgrProjections <- function(x) {
  # Drop EA and EU country aggregates
  x <- x[getItems(x, 1)[!getItems(x, 1) %in% c("EA", "EU")], , ]
  # Convert the eurostat countrycodes to iso3c codes
  getItems(x, 1) <- countrycode::countrycode(getItems(x, 1), "country.name", "iso3c")
  toolGeneralConvert(x, note = FALSE)
}

#' @rdname readEurostatPopGDP
#' @order 1
downloadEurostatPopGDP <- function(subtype) {
  rlang::check_installed("eurostat")
  # By defining the cache_dir in eurostat::get_eurostat the data is saved to the madrat source directory
  switch(
    subtype,
    # Filter for "AVG" = average population.
    "population" = eurostat::get_eurostat("demo_gind",
                                          filters = list(indic_de = "AVG"),
                                          time_format = "num",
                                          cache_dir = "."),
    # Filter for baseline projection of total population.
    "population_projections" = eurostat::get_eurostat("proj_19np",
                                                      filters = list(sex = "T", projection = "BSL", age = "TOTAL"),
                                                      time_format = "num",
                                                      cache_dir = "."),
    # Filter for GDP at market prices (=B1GQ) in Chained-Linked Volumes in 2005 mil. National Currencies (= CLV05_MNAC)
    "GDP" = eurostat::get_eurostat("nama_10_gdp",
                                   filters = list(freq = "A", na_item = "B1GQ", unit = "CLV05_MNAC"),
                                   time_format = "num",
                                   cache_dir = "."),
    "GDPgr_projections" = stop("Download EUROSTAT GDP growth rate projections manually.")
  )

  switch(
    subtype,
    "population" = list(
      url           = "",
      doi           = "",
      title         = "Eurostat Population Data",
      description   = "",
      unit          = "",
      author        = "Eurostat",
      release_date  = "Updated frequently",
      license       = "",
      comment       = "see https://ec.europa.eu/eurostat"
     ),
    "population_projections" = list(
      url           = "",
      doi           = "",
      title         = "Eurostat Population Projections",
      description   = "",
      unit          = "",
      author        = "Eurostat",
      release_date  = "Updated frequently",
      license       = "",
      comment       = "see https://ec.europa.eu/eurostat"
    ),
    "GDP" = list(
      url           = "",
      doi           = "",
      title         = "Eurostat GDP Data",
      description   = "",
      unit          = "2005 mil. National Currencies",
      author        = "Eurostat",
      release_date  = "Updated frequently",
      license       = "",
      comment       = "see https://ec.europa.eu/eurostat"
    ),
  )
}
