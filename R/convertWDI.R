#' Convert WDI
#'
#' Convert WDI converts data from readWDI() to ISO country level. Adds Taiwan
#' as difference from global total.
#'
#' @param x MAgPIE object containing WDI data region resolution
#' @inheritParams readWDI
#' 
#' @return MAgPIE object of the WDI data disaggregated to country level
#' 
#' @family WDI functions
#' 
convertWDI <- function(x, subtype){

  getSets(x)[3] <- "variable"

  # changing scale of indicators
  if (subtype %in% c("SP.POP.TOTL",
                     "NY.GDP.MKTP.PP.KD",
                     "NY.GDP.MKTP.PP.CD",
                     "NY.GDP.MKTP.CD", 
                     "NY.GDP.MKTP.CN", 
                     "NY.GDP.MKTP.KD",
                     "NY.GDP.MKTP.KN", 
                     "NV.AGR.TOTL.KD", 
                     "NV.AGR.TOTL.CD")) {
    x <- x / 1e+6

    # Kosovo added to Serbia
    x["RS",,] <- dimSums(x[c("RS", "XK"),,], dim=1, na.rm = TRUE)
    } else if (subtype %in%  WDI::WDI_data$series[,"indicator"]){
    # include c("SP.URB.TOTL.IN.ZS", "EN.POP.DNST", "AG.SRF.TOTL.K2", "NE.CON.PRVT.PC.KD", "NE.CON.PRVT.PP.CD","NE.CON.PRVT.PP.KD")
    vcat("Warning: Kosovo left out of conversion and has differing population values from FAO", verbosity=2)
  } else {
    stop("subtype does not exist in the dataset!")
  }
  y <- x

  getCells(y) <- countrycode::countrycode(getCells(y), "iso2c", "iso3c", custom_match = c("JG" = "JEY"))
  y <- y[!is.na(getCells(y)),,]
  getSets(y)[1] <- "iso3c"
  y <- clean_magpie(y)

  y <- y["ANT",,,invert=TRUE]

  y <- toolCountryFill(y, fill = 0)
  y[is.na(y)] <- 0
  #remove years which only contain 0s as entries
  y <- y[,!apply(y, 2, function(x) return(all(x == 0))),]
  
  ## taiwan only listed in global totals, not explicetly
  #world<- colSums(y,na.rm=T)
  #taiwan<-x["1W",,] - world
  #y["TWN",,]<-colSums(taiwan,na.rm=T)
  y <- y[,sort(getYears(y)),]
  return(y)
}
