#' Read IMF
#'
#' Read-in iMF data
#'
#' @param subtype Either "current_account" or "GDPpc"
#' @return magpie object of the data
#'
#' @seealso [madrat::readSource()]
#' @family "Past" GDPpc functions
#' @family "Future" GDPpc functions
#' @family IMF functions
#'
#' @examples
#' \dontrun{
#' a <- readSource(type = "IMF")
#' }
readIMF <- function(subtype = "current_account") {
  # Check function input
  if (!subtype %in% c("current_account", "GDPpc")) {
    stop("Bad input for readiMD. Invalid 'subtype' argument.")
  }

  # Define source file
  sourceFile <- "WEOall.xls"

  # Define what data, i.e.which "WEO subject codes", to keep
  myWEOCodes <- if (subtype == "GDPpc") c("NGDPRPPPPC") else "BCA"

  weoData <- readr::read_tsv(sourceFile, col_types = c(.default = "c")) %>%
    dplyr::filter(.data$`WEO Subject Code` %in% myWEOCodes) %>%
    tidyr::unite("tmp", .data$Scale, .data$Units, sep = " ") %>%
    dplyr::mutate(tmp = sub("NA ", "", .data$tmp),
                  tmp = paste0("[", .data$tmp, "]")) %>%
    tidyr::unite("Subject Descriptor", .data$`Subject Descriptor`, .data$tmp, sep = " ") %>%
    dplyr::select("iso3c" = .data$ISO, .data$`Subject Descriptor`, tidyselect::starts_with(c("1", "2"))) %>%
    tidyr::pivot_longer(tidyselect::starts_with(c("1", "2")), names_to = "year") %>%
    dplyr::mutate(value = gsub(",", "", .data$value),
                  dplyr::across(.cols = c(.data$year, .data$value),
                                ~ suppressWarnings(as.double(.x))),
                                # The warnings that are being suppressed above, come from
                                # character strings that can't be converted to numeric, and
                                # are thus returned as NA.
                  value = tidyr::replace_na(.data$value, 0)) %>%
    tidyr::pivot_wider(names_from = .data$`Subject Descriptor`)

  # Transform to magpie
  out <- as.magpie(weoData)

  # TMP! Give names
  if (subtype == "current_account") {
    getNames(out) <- "current account [billion U.S. dollar]"
  }

  return(out)
}
