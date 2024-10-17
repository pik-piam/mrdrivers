#' Read James et al. (2012) dataset
#'
#' Read-in GDP per-capita data from the publication James, Spencer L., Paul
#' Gubbins, Christopher JL Murray, and Emmanuela Gakidou. 2012. "Developing a
#' Comprehensive Time Series of GDP per Capita for 210 Countries from 1950 to
#' 2015." Population Health Metrics 10 (1): 12. doi:10.1186/1478-7954-10-12.
#'
#' The data is in Annex 3
#'
#' @param subtype String indicating the data series
#' @inherit madrat::readSource return
#' @seealso [madrat::readSource()] and [madrat::downloadSource()]
#' @examples \dontrun{
#' readSource("James", subtype = "gdp")
#' }
#' @order 2
readJames <- function(subtype) {
  # Add alias for easy referencing
  if (subtype == "gdp") subtype <- "WB_USD05_PPP_pc"

  utils::read.csv("james.csv", sep = ";", dec = ",") %>%
    `[`(, c("ISO3", "Year", subtype)) %>%
    as.magpie(spatial = 1, temporal = 2, tidy = TRUE)
}

#' @rdname readJames
#' @param x MAgPIE object returned by readJames
#' @order 3
convertJames <- function(x, subtype) {
  # Add alias for easy referencing
  if (subtype == "gdp") subtype <- "WB_USD05_PPP_pc"
  if (subtype == "WB_USD05_PPP_pc") {
    x <- GDPuc::toolConvertGDP(x,
                               unit_in = "constant 2005 Int$PPP",
                               unit_out = toolGetUnitDollar(inPPP = TRUE),
                               replace_NAs = c("linear", "no_conversion"))
  }
  # Ignore warning: Data for following unknown country codes removed: ANT, SUN
  toolGeneralConvert(x, warn = FALSE)
}

#' @rdname readJames
#' @order 1
downloadJames <- function() {
  stop("Manual download of James data required!")
  # Compose meta data
  list(url           = "https://static-content.springer.com/esm/art%3A10.1186%2F1478-7954-10-12/MediaObjects/12963_2011_195_MOESM1_ESM.xlsx", # nolint: line_length_linter.
       doi           = "-",
       title         = "James GDP per capita dataset",
       description   = "Developing a comprehensive time series of GDP per capita for 210 countries from 1950 to 2015",
       unit          = "-",
       author        = "James, S.L., Gubbins, P., Murray, C.J. et al.",
       release_date  = "2012",
       license       = "-",
       comment       = "Manual download required! Accessed on the 08.10.2024.")
}
