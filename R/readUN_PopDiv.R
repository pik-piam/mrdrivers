#' Read UN Population Division Data
#'
#' Read past UN population data.
#'
#' @param subtype String indicating version and sheet
#' @inherit madrat::readSource return
#' @seealso [madrat::readSource()] and [madrat::downloadSource()]
#' @order 2
readUN_PopDiv <- function(subtype = "estimates") { # nolint
  # Check function input
  if (!subtype %in% c("estimates", "medium")) {
    stop("Bad input for readUN_PopDiv. Invalid 'subtype' argument.")
  }
  file <- "WPP2022_POP_F01_1_POPULATION_SINGLE_AGE_BOTH_SEXES.xlsx"
  sheet <- if (subtype == "estimates") "Estimates" else "Medium variant"
  readxl::read_xlsx(file, sheet = sheet, skip = 16, col_types = "text", progress = FALSE) %>%
    dplyr::select("ISO3 Alpha-code", "year" = "Year", dplyr::matches("^[0-9]*$")) %>%
    dplyr::filter(!is.na(.data$`ISO3 Alpha-code`)) %>%
    tidyr::pivot_longer(cols = dplyr::matches("^[0-9]*$"),
                        names_to = "age",
                        values_transform = c("value" = as.numeric)) %>%
    dplyr::group_by(.data$`ISO3 Alpha-code`, .data$year) %>%
    dplyr::summarise(value = sum(.data$value), .groups = "drop") %>%
    as.magpie(spatial = "ISO3 Alpha-code")
}



#' @rdname readUN_PopDiv
#' @order 3
#' @param x MAgPIE object returned from readUN_PopDiv
convertUN_PopDiv <- function(x) {
  toolGeneralConvert(x, no_remove_warning = "XKX")
}

#' @rdname readUN_PopDiv
#' @order 1
downloadUN_PopDiv <- function() { # nolint
  url <- "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/2_Population/WPP2022_POP_F01_1_POPULATION_SINGLE_AGE_BOTH_SEXES.xlsx"
  utils::download.file(url, basename(url), quiet = TRUE)

  # Compose meta data
  list(url           = url,
       doi           = "-",
       title         = "World Population Prospects",
       description   = "World Population Prospects from the United Nations Deparment on Economic and Social Affairs",
       unit          = "Population in thousands",
       author        = "United Nations Deparment on Economic and Social Affairs",
       release_date  = "2022",
       license       = "-",
       comment       = "-")
}
