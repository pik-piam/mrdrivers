#' Read-in data from the International Monetary Fund's (IMF) Economic Outlook
#'
#' Read-in data from the IMF's World Economic Outlook.
#' Currently reading GDP per capita (in constant 2021 Int$PPP), current account balance data and population.
#'
#' @inherit madrat::readSource return
#' @seealso [madrat::readSource()] and [madrat::downloadSource()]
#' @examples
#' \dontrun{
#' readSource("IMF")
#' }
#' @order 2
readIMF <- function() {
  # Define what data ("INDICATOR.ID") to keep: here GDPpc in PPP (NGDPRPPPPC), current account balance (BCA) and
  # population (LP)
  myIndicatorIDs <- c("NGDPRPPPPC", "BCA", "LP")

  readxl::read_xlsx("WEOApr2026all.xlsx", sheet = "Countries", progress = FALSE) %>%
    suppressWarnings() %>%
    dplyr::filter(.data$INDICATOR.ID %in% myIndicatorIDs, !is.na(.data$SCALE)) %>%
    tidyr::unite("tmp", c("SCALE", "UNIT"), sep = " ") %>%
    dplyr::mutate(tmp = paste0("[", .data$tmp, "]")) %>%
    tidyr::unite("INDICATOR", c("INDICATOR", "tmp"), sep = " ") %>%
    dplyr::select("iso3c" = "COUNTRY.ID", "INDICATOR", tidyselect::starts_with(c("1", "2"))) %>%
    tidyr::pivot_longer(tidyselect::starts_with(c("1", "2")),
                        names_to = "year",
                        names_transform = as.numeric,
                        values_transform = as.numeric) %>%
    tidyr::replace_na(list(value = 0)) %>%
    as.magpie(spatial = "iso3c", temporal = "year", tidy = TRUE)
}

#' @rdname readIMF
#' @param x MAgPIE object returned by readIMF
#' @param subtype Use to filter the IMF data
#' @order 3
convertIMF <- function(x, subtype = "all") {
  if (!subtype %in% c("all", "gdppc", "gdp", "BCA")) {
    stop("Bad input for readIMF. Invalid 'subtype' argument. Available subtypes are 'all', 'gdppc', 'gdp', and 'BCA'.")
  }

  # Add Kosovo to Serbia
  x["SRB", , ] <- dimSums(x[c("SRB", "KOS"), , ], dim = 1, na.rm = TRUE)
  x <- x[getItems(x, dim = 1) != "KOS", , ]
  # Attribute WBG (west bank and gaza) to PSE (Palestine, State of)
  x <- add_columns(x, addnm = "PSE", dim = 1)
  x["PSE", , ] <- x["WBG", , ]
  x <- x[getItems(x, dim = 1) != "WBG", , ]

  # Use convert function to filter
  if (subtype == "gdppc") {
    h <- "Gross domestic product (GDP), Constant prices, Per capita, purchasing power parity (PPP) international dollar, ICP benchmark 2021 [Units NA]" # nolint: line_length_linter.
    x <- x[, , h]
  }
  if (subtype == "gdp") {
    h <- "Gross domestic product (GDP), Constant prices, Per capita, purchasing power parity (PPP) international dollar, ICP benchmark 2021 [Units NA]" # nolint: line_length_linter.
    h2 <- "Population, Persons for countries / Index for country groups [Millions NA]"
    x <- x[, , h] * x[, , h2]
  }
  if (subtype == "BCA") x <- x[, , "Current account balance (credit less debit), US dollar [Billions US dollar]"]

  x <- toolGeneralConvert(x)

  if (subtype %in% c("gdppc", "gdp")) {
    x <- GDPuc::toolConvertGDP(x,
                               unit_in = "constant 2021 Int$PPP",
                               unit_out = toolGetUnitDollar(inPPP = TRUE),
                               replace_NAs = "no_conversion")
  }

  x
}


#' @rdname readIMF
#' @order 1
downloadIMF <- function() {
  stop("Manual download of IMF data required!")
  url <- "https://data.imf.org/-/media/iData/External-Storage/Documents/2F78EE59F79143A7921E5E203D3AAA80/en/WEOApr2026all.xlsx"
  utils::download.file(url, basename(url), quiet = TRUE)

  # Compose meta data
  list(url           = url,
       doi           = "-",
       title         = "World Economic Outlook database of the IMF",
       description   = "World Economic Outlook database of the International Monetary Fund",
       unit          = "-",
       author        = "International Monetary Fund",
       release_date  = "April 2026",
       license       = "-",
       comment       = "-")
}
