#' Read PWT
#' 
#' Read-in an PWT data .xlsx file as magclass object
#
#' @family "Past" GDPpc functions
#' @family PWT functions
#' 
#' @return Magpie object of the PWT data
readPWT<- function() {
  pwt <- readxl::read_excel("pwt80.xlsx", sheet = "Data")
  # Remove "country", "currency_unit" and indicator ("i_") columns
  pwt <- pwt[, !grepl("(^country$|^currency_unit$|^i_)", names(pwt))]
  # Transform to magpie
  as.magpie(pwt)
}
