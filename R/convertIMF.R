#' @title convertIMF
#' @description Converts data from IMF
#' @param x MAgPIE object returned by readIMF
#' @inheritParams readIMF
#' @inherit readIMF return
#' @family IMF functions
convertIMF <- function(x, subtype = "current_account") {
  if (subtype == "current_account") {
    # delete "World"
    x <- x["World",,,invert=TRUE]
    # delete Kosovo
    x <- x["KOS",,,invert=TRUE]
    
    ### allocate global current account to the countries
    # calculate global sum which is not 0
    x_sum <- - dimSums(x, dim=1, na.rm=TRUE)
    # calculate global absolute share of current account
    x_abs     <-  abs(x)
    x_abs_sum <- dimSums(x_abs, dim=1, na.rm=TRUE)
    # calculate additional value for each country
    x_rest <- x_abs / x_abs_sum * x_sum
    # add global rest to the countries
    x <- x + x_rest
  }
  
  toolGeneralConvert(x, no_remove_warning = c("UVK", "WBG"))
}
