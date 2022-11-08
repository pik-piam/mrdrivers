#' Read Population Estimates And Projections from the World Bank
#'
#' Read-in xlsx file from the World Bank's Population Estimates And Projections
#'
#' @seealso [madrat::readSource()]
#' @family "Future" population functions
#' @return magpie object
readPEAP <- function() {
  file <- "Data_Extract_From_Population_estimates_and_projections.xlsx"
  x <- readxl::read_xlsx(file, sheet = 1, range = "B1:CQ260") %>%
    tidyr::pivot_longer(cols = tidyselect::starts_with(c("1", "2")), names_to = "year") %>%
    dplyr::select("iso3c" = "Country Code", "year", "value") %>%
    dplyr::mutate(variable = "population",
                  year = as.integer(sub(" .*", "", .data$year)),
                  value = suppressWarnings(as.numeric(.data$value))) %>%
                  # The warnings that are being suppressed above, come from
                  # character strings that can't be converted to numeric, and
                  # are thus returned as NA.
    dplyr::relocate("value", .after = tidyselect::last_col()) %>%
    as.magpie(tidy = TRUE)
}

#' @describeIn readPEAP Convert data from the World Bank's Population Estimates And Projections dataset
#' @param x MAgPIE object returned by readPEAP
convertPEAP <- function(x) {
  toolGeneralConvert(x, warn = FALSE, note = FALSE)
}
