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

  my_locale <- readr::default_locale()
  my_locale$encoding <- "UTF-16LE"

  weoData <- readr::read_tsv("WEOApr2024all.ashx",
                             col_types = c(.default = "c"),
                             locale = my_locale,
                             na = c("", "n/a", "--"),
                             progress = FALSE) %>%
    dplyr::filter(.data$`WEO Subject Code` %in% myWEOCodes) %>%
    tidyr::unite("tmp", c("Scale", "Units"), sep = " ") %>%
    dplyr::mutate(tmp = sub("NA ", "", .data$tmp),
                  tmp = paste0("[", .data$tmp, "]")) %>%
    tidyr::unite("Subject Descriptor", c("Subject Descriptor", "tmp"), sep = " ") %>%
    dplyr::select("iso3c" = "ISO", "Subject Descriptor", tidyselect::starts_with(c("1", "2"))) %>%
    tidyr::pivot_longer(tidyselect::starts_with(c("1", "2")),
                        names_to = "year",
                        names_transform = as.numeric,
                        values_transform = as.numeric) %>%
    tidyr::replace_na(list(value = 0)) %>%
    tidyr::pivot_wider(names_from = "Subject Descriptor")

  # Transform to magpie
  as.magpie(weoData)
}

#' @rdname readIMF
#' @param x MAgPIE object returned by readIMF
#' @param subtype Use to filter the IMF data
#' @order 3
convertIMF <- function(x, subtype = "all") {
  # Use convert function to filter
  if (subtype == "GDPpc") {
    h <-"Gross domestic product per capita, constant prices [Units Purchasing power parity; 2017 international dollar]"
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
