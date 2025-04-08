#' Read-in data from the World Bank's World Development Indicator (WDI) database.
#'
#' Download, read-in and convert WDI (World development indicators) data.
#'
#' @param subtype A string. Type of WDI data that should be read. Use the
#' World Bank indicator abbreviation. Available subtypes are:
#' \itemize{
#' \item "pop" or "SP.POP.TOTL": Population, total
#' \item "lab" or "SP.POP.1564.TO": Working age population (15-64 years old)
#' \item "urb" or "SP.URB.TOTL.IN.ZS": Urban Population (% of total)
#' \item "gdp" or "NY.GDP.MKTP.PP.KD": GDP, PPP (constant 2017 international Dollar)
#' \item "NV.AGR.TOTL.KD": Ag GDP, MER, (2010 US$)
#' \item "PA.NUS.PPPC.RF": Price level ratio of PPP conversion factor (GDP) to market exchange rate
#' \item "AG.SRF.TOTL.K2": Surface area (in square kms)
#' }
#'
#' @details
#'  The workflow to update the WDI data is the following: call the download function manually, and rename the new
#'  WDI.rds file including the download date. Then change the file_name that is read by readWDI. This ensures, that
#'  the past data is not changed between users.
#'
#'
#' @inherit madrat::readSource return
#' @seealso [madrat::readSource()] and [madrat::downloadSource()]
#'
#' @examples \dontrun{
#' readSource("WDI", subtype = "pop")
#' }
#' @order 2
readWDI <- function(subtype) {
  # Add some aliases for easier referencing
  subtype <- switch(
    subtype,
    "gdp" = "NY.GDP.MKTP.PP.KD",
    "pop" = "SP.POP.TOTL",
    "lab" = "SP.POP.1564.TO",
    "urb" = "SP.URB.TOTL.IN.ZS",
    subtype
  )

  x <- readr::read_rds("WDI_11_04_2025.Rds")

  possibleSubtypes <- colnames(x)[!colnames(x) %in% c("iso3c", "iso2c", "country", "year")]

  if (!subtype %in% possibleSubtypes) {
    stop(glue("Bad subtype. Possible subtypes are: \n{paste0(possibleSubtypes, collapse = '\n')}."))
  }

  y <- x[, c("iso2c", "year", subtype)] %>%
    # Replace "." with "_" otherwise readSource messes up the data dim
    dplyr::rename_with(~ gsub(".", "_", .x, fixed = TRUE))

  as.magpie(y, spatial = "iso2c", temporal = "year", tidy = TRUE, replacement = ".")
}


#' @rdname readWDI
#' @param x MAgPIE object returned by readWDI
#' @order 3
convertWDI <- function(x, subtype) {

  # Note: Part of convertWDI and readWDI are problematic in many ways
  # --> these problems are solved in the following commits

  # Add some aliases for easier referencing
  subtype <- switch(
    subtype,
    "gdp" = "NY.GDP.MKTP.PP.KD",
    "pop" = "SP.POP.TOTL",
    "lab" = "SP.POP.1564.TO",
    "urb" = "SP.URB.TOTL.IN.ZS",
    subtype
  )

  if (subtype %in% c("SP.POP.TOTL", "SP.POP.1564.TO", "NY.GDP.MKTP.PP.KD", "NV.AGR.TOTL.KD")) {
    # Change scale of indicators
    x <- x / 1e+6
    # Add Kosovo to Serbia
    x["RS", , ] <- dimSums(x[c("RS", "XK"), , ], dim = 1, na.rm = TRUE)
  } else {
    vcat("Warning: Kosovo left out of conversion and has differing population values from FAO", verbosity = 2)
  }

  # Must occur before GPDuc (which seems to only work with iso3c)
  getCells(x) <- countrycode::countrycode(getCells(x),
                                          "iso2c",
                                          "iso3c",
                                          custom_match = c("JG" = "JEY"),
                                          warn = FALSE)

  # Check the structure of the underlying data
  strNames <- getCells(x)
  # Find indices where region names are not NA or empty
  validIndices <- which(!is.na(strNames) & strNames != "")
  # Create a new object with only valid regions
  x <- x[validIndices, , ]

  # Normalization of GDP due to update units by the World Bank
  if (subtype == "NY.GDP.MKTP.PP.KD") {
    x <- GDPuc::toolConvertGDP(
      gdp = x,
      unit_in = "constant 2021 Int$PPP",
      unit_out = "constant 2017 Int$PPP",
      source = "wb_wdi"
    )
  }

  toolGeneralConvert(x)
}


#' @rdname readWDI
#' @order 1
downloadWDI <- function() {
  rlang::check_installed("WDI")
  # The WDI data is updated with the function "WDISearch(cache = WDIcache())"
  WDI::WDIsearch(cache = WDI::WDIcache())
  indicator <- c(
    "SP.POP.TOTL",       # Total population
    "SP.POP.1564.TO",    # Working age population (15-64 years old)
    "SP.URB.TOTL.IN.ZS", # Urban Population (% of total)
    "PA.NUS.PPPC.RF",    # Price Level Ration (PPP/MER)
    "NY.GDP.MKTP.PP.KD", # GDP [constant 2017 Int$PPP]
    "NV.AGR.TOTL.KD",    # For mrvalidation: AgFF value added [constant 2015 US$MER]
    "AG.SRF.TOTL.K2"     # For mredgebuildings: surface area [square kms]
  )
  endYear <- as.numeric(strsplit(as.character(Sys.Date()), "-")[[1]][1]) - 1
  wdi <- WDI::WDI(indicator = indicator, start = 1960, end = endYear, extra=FALSE)

  # remove rows where country is in the regions list
  # lead to (relevant yet ignored) duplication warnings otherwise
  regions <- c("Africa Eastern and Southern", "Africa Western and Central", "Arab World", "Caribbean small states",
               "Central Europe and the Baltics", "Early-demographic dividend", "East Asia & Pacific",
               "East Asia & Pacific (excluding high income)", "East Asia & Pacific (IDA & IBRD countries)", "Euro area",
               "Europe & Central Asia", "Europe & Central Asia (excluding high income)",
               "Europe & Central Asia (IDA & IBRD countries)", "European Union",
               "Fragile and conflict affected situations", "Global Partnership for Education",
               "Heavily indebted poor countries (HIPC)", "High income", "IBRD only",
               "IDA & IBRD total", "IDA blend", "IDA only", "IDA total", "Late-demographic dividend",
               "Latin America & Caribbean", "Latin America & Caribbean (excluding high income)",
               "Latin America & the Caribbean (IDA & IBRD countries)", "Least developed countries: UN classification",
               "Lending category not classified", "Low & middle income",
               "Low income", "Lower middle income", "Micronesia, Fed. Sts.",
               "Middle East & North Africa", "Middle East & North Africa (excluding high income)",
               "Middle East & North Africa (IDA & IBRD countries)", "Middle income", "North America",
               "Not classified", "OECD members", "Other small states", "Pacific island small states",
               "Post-demographic dividend", "Pre-demographic dividend", "Small states", "South Asia",
               "South Asia (IDA & IBRD)", "Sub-Saharan Africa", "Sub-Saharan Africa (excluding high income)",
               "Sub-Saharan Africa (IDA & IBRD countries)",
               "Upper middle income", "World")

  wdi <- dplyr::filter(wdi, !(country %in% regions))

  # add ISO3 codes for countries that don't have them
  iso3Mapping <- c("Anguilla" = "AIA",
                   "Cook Islands" = "COK",
                   "Tokelau" = "TKL",
                   "Niue" = "NIU",
                   "Netherlands Antilles" = "ANT",
                   "Montserrat" = "MSR",
                   "Mayotte" = "MYT"
  )
  wdi <- dplyr::mutate(wdi, iso3c = ifelse(country %in% names(iso3Mapping),
                                    iso3_lookup[match(country, names(iso3Mapping))],
                                    iso3c))

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


