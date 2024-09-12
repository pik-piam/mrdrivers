#' Read IMF
#'
#' Read-in data from the IMF's World Economic Outlook.
#' Currently reading GDP per capita and current account balance data.
#'
#' @inherit madrat::readSource return
#' @seealso [madrat::readSource()] and [madrat::downloadSource()]
#' @examples
#' \dontrun{
#' readSource("IMF")
#' }
#' @order 2
readIMF <- function() {
  # Define what data, i.e.which "WEO subject codes", to keep: here GDPpc and current account balance
  myWEOCodes <- c("NGDPRPPPPC", "BCA")

  myLocale <- readr::default_locale()
  myLocale$encoding <- "UTF-16LE"

  readr::read_tsv("WEOApr2024all.ashx",
                  col_types = c(.default = "c"),
                  locale = myLocale,
                  na = c("", "n/a", "--"),
                  progress = FALSE) %>%
    dplyr::filter(.data$`WEO Subject Code` %in% myWEOCodes) %>%
    tidyr::unite("tmp", c("Scale", "Units"), sep = " ") %>%
    dplyr::mutate(tmp = sub("NA ", "", .data$tmp),
                  tmp = paste0("[", .data$tmp, "]")) %>%
    tidyr::unite("Subject Descriptor", c("Subject Descriptor", "tmp"), sep = " ") %>%
    dplyr::select("ISO", "Subject Descriptor", tidyselect::starts_with(c("1", "2"))) %>%
    tidyr::pivot_longer(tidyselect::starts_with(c("1", "2")),
                        names_to = "year",
                        names_transform = as.numeric,
                        values_transform = as.numeric) %>%
    tidyr::replace_na(list(value = 0)) %>%
    as.magpie(spatial = "ISO", temporal = "year", tidy = TRUE)
}

#' @rdname readIMF
#' @param x MAgPIE object returned by readIMF
#' @param subtype Use to filter the IMF data
#' @order 3
convertIMF <- function(x, subtype = "all") {
  if (!subtype %in% c("all", "GDPpc", "BCA")) {
    stop("Bad input for readIMF. Invalid 'subtype' argument. Available subtypes are 'all', 'GDPpc', and 'BCA'.")
  }

  # Use convert function to filter
  if (subtype == "GDPpc") {
    h <- "Gross domestic product per capita, constant prices [Units Purchasing power parity; 2017 international dollar]"
    x <- x[, , h]
  }
  if (subtype == "BCA") x <- x[, , "Current account balance [Billions U.S. dollars]"]

  toolGeneralConvert(x, no_remove_warning = c("UVK", "WBG"), note = FALSE)
}


#' @rdname readIMF
#' @order 1
downloadIMF <- function() {
  url <- "https://www.imf.org/-/media/Files/Publications/WEO/WEO-Database/2024/April/WEOApr2024all.ashx"
  utils::download.file(url, basename(url), quiet = TRUE)

  # Compose meta data
  list(url           = url,
       doi           = "-",
       title         = "World Economic Outlook database of the IMF",
       description   = "World Economic Outlook database of the International Monetary Fund",
       unit          = "-",
       author        = "International Monetary Fund",
       release_date  = "April 2024",
       license       = "-",
       comment       = "-")
}
