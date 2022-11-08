#' Read IMF
#'
#' Read-in IMF data
#'
#' @param subtype Either "current_account" or "GDPpc"
#' @param subset Either "WEOall.xls" or "WEOallOct2019.xls"
#' @return magpie object of the data
#'
#' @seealso [madrat::readSource()]
#' @seealso [downloadIMF()]
#'
#' @examples
#' \dontrun{
#' a <- readSource(type = "IMF")
#' }
readIMF <- function(subtype = "current_account", subset = "WEOall.xls") {
  # Check function input
  if (!subtype %in% c("current_account", "GDPpc")) {
    stop("Bad input for readiMD. Invalid 'subtype' argument.")
  }

  # Define source file
  sourceFile <- subset

  # Define what data, i.e.which "WEO subject codes", to keep
  myWEOCodes <- if (subtype == "GDPpc") "NGDPRPPPPC" else "BCA"

  weoData <- readr::read_tsv(sourceFile, col_types = c(.default = "c")) %>%
    dplyr::filter(.data$`WEO Subject Code` %in% myWEOCodes) %>%
    tidyr::unite("tmp", c("Scale", "Units"), sep = " ") %>%
    dplyr::mutate(tmp = sub("NA ", "", .data$tmp),
                  tmp = paste0("[", .data$tmp, "]")) %>%
    tidyr::unite("Subject Descriptor", c("Subject Descriptor", "tmp"), sep = " ") %>%
    dplyr::select("iso3c" = "ISO", "Subject Descriptor", tidyselect::starts_with(c("1", "2"))) %>%
    tidyr::pivot_longer(tidyselect::starts_with(c("1", "2")), names_to = "year") %>%
    dplyr::mutate(value = gsub(",", "", .data$value),
                  dplyr::across(.cols = c("year", "value"),
                                ~ suppressWarnings(as.double(.x))),
                                # The warnings that are being suppressed above, come from
                                # character strings that can't be converted to numeric, and
                                # are thus returned as NA.
                  value = tidyr::replace_na(.data$value, 0)) %>%
    tidyr::pivot_wider(names_from = "Subject Descriptor")

  # Transform to magpie
  out <- as.magpie(weoData)

  # TMP! Give names
  if (subtype == "current_account") {
    getNames(out) <- "current account [billion U.S. dollar]"
  }

  return(out)
}

#' @describeIn readIMF Convert IMF data
#' @param x MAgPIE object returned by readIMF
convertIMF <- function(x, subtype = "current_account") {
  if (subtype == "current_account") {
    # delete "World"
    x <- x["World", , , invert = TRUE]
    # delete Kosovo
    x <- x["KOS", , , invert = TRUE]

    ### allocate global current account to the countries
    # calculate global sum which is not 0
    xSum <- -dimSums(x, dim = 1, na.rm = TRUE)
    # calculate global absolute share of current account
    xAbs     <-  abs(x)
    xAbsSum <- dimSums(xAbs, dim = 1, na.rm = TRUE)
    # calculate additional value for each country
    xRest <- xAbs / xAbsSum * xSum
    # add global rest to the countries
    x <- x + xRest
  }

  toolGeneralConvert(x, no_remove_warning = c("UVK", "WBG"), warn = FALSE, note = FALSE)
}
