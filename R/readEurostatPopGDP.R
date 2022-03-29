#' Read Eurostat historical emissions
#'
#' Read-in Eurostat historical emissions csv files as magclass object
#'
#' @param subtype emissions for original eurostat emissions split, MACCemi for MACC historical emissions, or
#' sectorEmi for sector specific emissions
#' @return magpie object of Eurostat historical emissions (MtCO2)
#' @seealso [madrat::readSource()]
#' @examples \dontrun{
#' readSource("EurostatPopGDP", subtype = "population")
#' }
#' @order 1
readEurostatPopGDP <- function(subtype) {

  switch(subtype,
         "population" = rEurostatPopulation(),
         "population_projections" = rEurostatPopulationProjections(),
         "GDP" = rEurostatGDP(),
         stop("Bad input for readEurostat. Invalid 'subtype' argument."))
}

######################################################################################
# Functions
######################################################################################
rEurostatPopulation <- function() {
  readr::read_csv("estat_demo_gind.csv.gz", col_types = "ccccdd_", progress = FALSE) %>%
    # Filter for "AVG" = average population.
    dplyr::filter(.data$indic_de == "AVG") %>%
    # Convert to magpie
    as.magpie(spatial = "geo", temporal = "TIME_PERIOD")
}

rEurostatPopulationProjections <- function() {
  readr::read_csv("estat_proj_19np.csv.gz", col_types = "cccccccdd_", progress = FALSE) %>%
    # Filter for baseline projection of total population.
    dplyr::filter(.data$sex == "T",
                  .data$age == "TOTAL",
                  .data$projection == "BSL") %>%
    # Convert to magpie
    as.magpie(spatial = "geo", temporal = "TIME_PERIOD")
}

rEurostatGDP <- function() {
  readr::read_csv("estat_nama_10_gdp.csv.gz", col_types = "cccccdd_", progress = FALSE) %>%
    # Filter for GDP at market prices (=B1GQ) in Chained-Linked Volumes in
    # 2005 mil. National Currencies (= CLV05_MNAC)
    dplyr::filter(.data$na_item == "B1GQ", .data$unit == "CLV05_MNAC") %>%
    # Convert to magpie
    as.magpie(spatial = "geo", temporal = "TIME_PERIOD")
}
