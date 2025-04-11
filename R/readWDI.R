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

  x <- readr::read_rds("WDI_11_04_2025_2.Rds")

  possibleSubtypes <- colnames(x)[!colnames(x) %in% c("iso3c", "iso2c", "country", "year")]

  if (!subtype %in% possibleSubtypes) {
    stop(glue("Bad subtype. Possible subtypes are: \n{paste0(possibleSubtypes, collapse = '\n')}."))
  }

  y <- x[, c("iso3c", "year", subtype)] %>%
    # Replace "." with "_" otherwise readSource messes up the data dim
    dplyr::rename_with(~ gsub(".", "_", .x, fixed = TRUE))

  as.magpie(y, spatial = "iso3c", temporal = "year", tidy = TRUE, replacement = ".")
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
    # !TODO could be improved to adjust to the fact that Kosovo data might be missing for some years
    x["SRB", , ] <- dimSums(x[c("SRB", "XKK"), , ], dim = 1, na.rm = TRUE)
    x <- x["XKK", , , invert = TRUE]
  }

  # Normalization of GDP due to update units by the World Bank
  if (subtype == "NY.GDP.MKTP.PP.KD") {
    # All three lead to the same outputs
    # wdi_l[is.na(wdi_n[,,1])]
    # !TODO ask Johannes if it is normal
    wdi <- GDPuc::toolConvertGDP(
      gdp = x,
      unit_in = "constant 2021 Int$PPP",
      unit_out = "constant 2017 Int$PPP",
      source = "wb_wdi",
      replace_NAs = NA
    )
    # wdi_l <- GDPuc::toolConvertGDP(
    #   gdp = x,
    #   unit_in = "constant 2021 Int$PPP",
    #   unit_out = "constant 2017 Int$PPP",
    #   source = "wb_wdi", replace_NAs = NA
    # )
    # wdi_n <- GDPuc::toolConvertGDP(
    #   gdp = x,
    #   unit_in = "constant 2021 Int$PPP",
    #   unit_out = "constant 2017 Int$PPP",
    #   source = "wb_wdi", replace_NAs = "linear"
    # )

    # if (verbose) {
    #   lost <- x[is.na(wdi[, , 1])]
    #   perLost <- 1 - length(lost[is.na(lost)]) / length(lost)
    #   cat(sprintf("GDP data lost due to unit conversion: %.1f%%", perLost * 100), "\n")
    # }

    x <- wdi
  }

  toolGeneralConvert(x)
}


#' @rdname readWDI
#' @order 1
downloadWDI <- function(verbose = FALSE) {
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
  wdiInit <- WDI::WDI(indicator = indicator, start = 1960, end = endYear)

  # ISO 3 codes used by countrycode
  iso3c <- na.omit(countrycode::codelist$iso3c)
  attributes(iso3c) <- NULL

  # remove rows where the col "country" is associated with a region (e.g., "Small states")
  # lead to (relevant yet ignored) duplication warnings otherwise when using as.magpie
  noIso <- unique(wdiInit$country[!wdiInit$iso3c %in% iso3c])

  missingIso <- countrycode::countrycode(noIso, origin = "country.name", destination = "iso3c", warn = FALSE)

  # Create lookup table with only countries that have valid ISO codes
  lookupTable <- data.frame(country = noIso, iso3c = missingIso) %>%
    dplyr::filter(!is.na(iso3c))
  # Add additional mappings for special cases not included in `countrycode`
  # Note that "Channel Islands" is map to "Jersey" and "Netherlands Antilles" to "BES"
  # Explanation for the "Netherlands Antilles":
  # "ANT" (Netherlands Antilles) was an ISO 3166-1 alpha-3 code that was
  # officially deprecated in 2010 when the Netherlands Antilles was dissolved as
  # a political entity. The territories that made up the Netherlands Antilles
  # were reassigned to new political statuses and corresponding ISO codes:
  # Curaçao → CUW, Sint Maarten → SXM, Bonaire, Sint Eustatius, and Saba → BES
  # Yet, in `countrycode`, we have "Caribbean Netherlands" → BES
  lookupTable <- rbind(lookupTable,
                       data.frame(country = c("Kosovo", "Netherlands Antilles", "Channel Islands"),
                         iso3c = c("XKK", "BES", "JEY"),
                         stringsAsFactors = FALSE
                       ))

  # Update wdi with the valid ISO codes
  wdi <- wdiInit %>%
    dplyr::rows_update(lookupTable, by = "country", unmatched = "ignore")

  notCountries <- setdiff(noIso, lookupTable$country)

  if (verbose) {
    cat("=================================================================/n")
    cat("These countries do not appear in the official World Development Indicators database: \n")
    print(countrycode::countrycode(iso3c[!iso3c %in% c(unique(wdi$iso3c), "GGY")],
                                   origin = "iso3c",
                                   destination = "country.name"))
    cat("\nThese entities were not included in the WDI.Rds file are they represent regions and not countries:\n")
    print(notCountries)
    cat("=================================================================/n")
  }

  wdi <- dplyr::filter(wdi, !(country %in% notCountries))

  readr::write_rds(wdi, "WDI2.Rds")

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


