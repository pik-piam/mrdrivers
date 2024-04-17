#' Read WDI data
#'
#' Download, read and convert WDI (World development indicators) data.
#'
#' @param subtype A string. Type of WDI data that should be read. Use the
#' World Bank indicator abbreviation. Available subtypes are:
#' \itemize{
#' \item \code{"SP.POP.TOTL"}: Population, total
#' \item \code{"SP.POP.1564.TO"}: Working age population (15-64 years old)
#' \item \code{"SP.URB.TOTL.IN.ZS"}: Urban Population (% of total)
#' \item \code{"NY.GDP.MKTP.PP.KD"}: GDP,PPP (constant 2017 international Dollar)
#' \item \code{"NV.AGR.TOTL.KD"}: Ag GDP, MER, (2010 US$)
#' \item \code{"PA.NUS.PPPC.RF"}: Price level ratio of PPP conversion factor (GDP) to market exchange rate
#' \item \code{"AG.SRF.TOTL.K2"}: Surface area (in square kms)
#' }
#'
#' @details
#'  The workflow to update the WDI data is the following: call the download function manually, and rename the new
#'  WDI.rds file including the download date. Then change the file_name that is read by readWDI. This ensures, that
#'  the past data isn't changed between users.
#'
#'
#' @inherit madrat::readSource return
#' @seealso [madrat::readSource()] and [madrat::downloadSource()]
#'
#' @examples \dontrun{
#' readSource("WDI", subtype = "SP.POP.TOTL")
#' }
#' @order 2
readWDI <- function(subtype) {
  x <- readr::read_rds("WDI_15_04_2024.Rds")
  possibleSubtypes <- colnames(x)[!colnames(x) %in% c("iso3c", "iso2c", "country", "year")]

  if (!subtype %in% possibleSubtypes) {
     stop(glue("Bad subtype. Possible subtypes are: \n{paste0(possibleSubtypes, collapse = '\n')}."))
  }

  x[, c("iso2c", "year", subtype)] %>%
    # Replace "." with "_" otherwise readSource messes up the data dim
    dplyr::rename_with(~ gsub(".", "_", .x, fixed = TRUE)) %>%
    as.magpie(spatial = 1, tidy = TRUE, replacement = ".")
}


#' @rdname readWDI
#' @param x MAgPIE object returned by readWDI
#' @order 3
convertWDI <- function(x, subtype) {

  if (subtype %in% c("SP.POP.TOTL", "SP.POP.1564.TO", "NY.GDP.MKTP.PP.KD", "NV.AGR.TOTL.KD")) {
    # Change scale of indicators
    x <- x / 1e+6
    # Add Kosovo to Serbia
    x["RS", , ] <- dimSums(x[c("RS", "XK"), , ], dim = 1, na.rm = TRUE)
  } else {
    vcat("Warning: Kosovo left out of conversion and has differing population values from FAO", verbosity = 2)
  }

  getCells(x) <- countrycode::countrycode(getCells(x),
                                          "iso2c",
                                          "iso3c",
                                          custom_match = c("JG" = "JEY"),
                                          warn = FALSE)

  x <- toolGeneralConvert(x)

  # Remove years which only contain 0s as entries
  x <- x[, !apply(x, 2, function(y) all(y == 0)), ]
  x
}


#' @rdname readWDI
#' @order 1
downloadWDI <- function() {
  rlang::check_installed("WDI")
  # The WDI data is updated with the function "WDISearch(cache = WDIcache())"
  WDI::WDIsearch(cache = WDI::WDIcache())
  indicator <- c("SP.POP.TOTL",       # Total population
                 "SP.POP.1564.TO",    # Working age population (15-64 years old)
                 "SP.URB.TOTL.IN.ZS", # Urban Population (% of total)
                 "PA.NUS.PPPC.RF",    # Price Level Ration (PPP/MER)
                 "NY.GDP.MKTP.PP.KD", # GDP [constant 2017 Int$PPP]
                 "NV.AGR.TOTL.KD",    # For mrvalidation: AgFF value added [constant 2015 US$MER]
                 "AG.SRF.TOTL.K2"     # For mredgebuildings: surface area [square kms]
                 )
  endYear <- as.numeric(strsplit(as.character(Sys.Date()), "-")[[1]][1]) - 1
  wdi <- WDI::WDI(indicator = indicator, start = 1960, end = endYear)
  readr::write_rds(wdi, "WDI.Rds")

  # Compose meta data
  list(url           = "-",
       doi           = "-",
       title         = "Select indicators from the WDI",
       description   = "Select indicators from the World Development Indicators database from the World Bank",
       unit          = "-",
       author        = "World Bank",
       release_date  = "-",
       license       = "-",
       comment       = "see also https://databank.worldbank.org/source/world-development-indicators")
}
