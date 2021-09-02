#' Read PWT
#' 
#' Read-in an PWT data .xlsx file as magclass object
#' 
#' @return magpie object of the PWT data
readPWT<- function() {
  pwt <- as.data.frame(readxl::read_excel("pwt81.xlsx", sheet = "Data"))
  
  # remove country and currency_unit columns. should be done better?
  pwt <- pwt[, c(-2, -3)]                       
  # remove indicator columns, as they are text based and don't mix well with floating values
  pwt <- pwt[, !grepl("i_", names(pwt))] 

  pwt <- as.magpie(pwt, datacol = 3)        
  pwt
}
