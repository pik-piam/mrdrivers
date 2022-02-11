#' @title downloadUN_PopDiv
#'
#' @description  Download World Population Prospects data from the United Nations Deparment on Economic and Social
#'   Affairs
#'
#' @seealso  [downloadSource()]
#' @family UN_PopDiv functions
#' @examples \dontrun{
#' downloadSource("UN_PopDiv")
#' }
downloadUN_PopDiv <- function() {
  url <- "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/EXCEL_FILES/1_Population/WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.xlsx" # nolint
  utils::download.file(url, "WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.xlsx", quiet = TRUE)

  # Compose meta data
  list(url           = url,
       doi           = "-",
       title         = "World Population Prospects",
       description   = "World Population Prospects from the United Nations Deparment on Economic and Social Affairs",
       unit          = "Population in thousands",
       author        = "United Nations Deparment on Economic and Social Affairs",
       release_date  = "2019",
       license       = "-",
       comment       = "-")
}
