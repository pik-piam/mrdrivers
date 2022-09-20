#' Read UN Population Division Data
#'
#' Read past UN population data. Covers 1950 to 2015 yearly and per M.49 area.
#' See \emph{United Nations, Department of Economic and Social Affairs,
#' Population Division} "World Population Prospects: The 2015 Revision"
#' (\href{https://esa.un.org/unpd/wpp/}{website}).
#'
#' @param subtype String indicating version and sheet
#' @return \code{magclass} object; population in thousands.
#' @seealso [madrat::readSource()]
#' @seealso [downloadUN_PopDiv()]
#' @order 1
readUN_PopDiv <- function(subtype = "WPP2019_estimates") { # nolint
  # Check function input
  if (!subtype %in% c("WPP2019_estimates", "WPP2019_medium", "WPP2015_estimates")) {
    stop("Bad input for readUN_PopDiv. Invalid 'subtype' argument.")
  }

  if (grepl("WPP2019", subtype)) {
     sheet <- if (subtype == "WPP2019_estimates") "ESTIMATES" else "MEDIUM VARIANT"
     readxl::read_xlsx(path.expand("WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.xlsx"),
                                   sheet = sheet,
                                   skip = 16) %>%
       dplyr::select("Country code", "Type", dplyr::matches("^[0-9]{4}$")) %>%
       dplyr::filter(.data$Type == "Country/Area") %>%
       tidyr::pivot_longer(cols = dplyr::matches("^[0-9]{4}$"),
                           names_to = "year",
                           values_transform = c("value" = as.numeric)) %>%
       dplyr::select(-"Type") %>%
       as.magpie(spatial = "Country code")
  } else {
     readxl::read_xls(path.expand("WPP2015_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.XLS"),
                                  sheet = "ESTIMATES",
                                  skip = 16) %>%
      dplyr::select("Country code", dplyr::matches("^[0-9]{4}$")) %>%
      tidyr::pivot_longer(cols = dplyr::matches("^[0-9]{4}$"), names_to = "year") %>%
      as.magpie(spatial = "Country code")
  }
}
