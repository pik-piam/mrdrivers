#' Read-in data from the United Nations Population Division (UN_PopDiv) database
#'
#' Read-in UN population data.
#'
#' @param subtype Either "pop" or "lab".
#' @param subset Either "estimates" or "medium".
#' @inherit madrat::readSource return
#' @seealso [madrat::readSource()] and [madrat::downloadSource()]
#' @order 2
readUN_PopDiv <- function(subtype, subset = "estimates") { # nolint: object_name_linter.
  # Check function input
  if (!subtype %in% c("pop", "lab")) {
    stop("Bad input for readUN_PopDiv. Invalid 'subtype' argument.")
  }
  if (!subset %in% c("estimates", "medium")) {
    stop("Bad input for readUN_PopDiv. Invalid 'subset' argument.")
  }

  file <- "WPP2022_POP_F01_1_POPULATION_SINGLE_AGE_BOTH_SEXES.xlsx"
  sheet <- if (subset == "estimates") "Estimates" else "Medium variant"

  ageRange <- if (subtype == "pop") c(0:99, "100+") else 15:64

  readxl::read_xlsx(file, sheet = sheet, skip = 16, col_types = "text", progress = FALSE) %>%
    dplyr::select("Variant", "ISO3 Alpha-code", "year" = "Year", dplyr::matches("^[0-9]")) %>%
    dplyr::filter(!is.na(.data$`ISO3 Alpha-code`)) %>%
    tidyr::pivot_longer(cols = dplyr::matches("^[0-9]"),
                        names_to = "age",
                        values_transform = c("value" = as.numeric)) %>%
    dplyr::filter(.data$age %in% ageRange) %>%
    dplyr::summarise(value = sum(.data$value), .by = c("Variant", "ISO3 Alpha-code", "year")) %>%
    as.magpie(spatial = "ISO3 Alpha-code", temporal = "year", tidy = TRUE)
}



#' @rdname readUN_PopDiv
#' @order 3
#' @param x MAgPIE object returned from readUN_PopDiv
convertUN_PopDiv <- function(x) { # nolint: object_name_linter.
  # Convert from thousands to millions
  x <- x * 1e-3
  toolGeneralConvert(x, no_remove_warning = "XKX")
}

#' @rdname readUN_PopDiv
#' @order 1
downloadUN_PopDiv <- function() { # nolint: object_name_linter.
  url <- "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/2_Population/WPP2022_POP_F01_1_POPULATION_SINGLE_AGE_BOTH_SEXES.xlsx" # nolint: line_length_linter.
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
