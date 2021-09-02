harmonizeTransition <- function(past, future, yEnd) {
  # yEnd = end of transisiton, from this time on the future values are used
  # generate past data for all future scenarios
  firstyear <- min(getYears(future, as.integer = TRUE))
  if (firstyear > min(getYears(past, as.integer = TRUE))) {                                                 
    years_past <- getYears(past)[which(getYears(past, as.integer = TRUE) <= firstyear)]
    tmpPast <- setNames(past[,years_past,rep(1,ndata(future))], getNames(future))
    years_trans <- getYears(future, as.integer = TRUE)[which(getYears(future, as.integer = TRUE) >= firstyear 
                                                             & getYears(future, as.integer = TRUE) <= yEnd)]
    tmpTrans    <- new.magpie(getRegions(future), years_trans, getNames(future), fill=0)
    for(t in years_trans) {
      tmpTrans[,t,] <- (  (max(years_trans) - t)/(max(years_trans) - min(years_trans)) * setYears(tmpPast[,firstyear,],t) 
                        + (t - min(years_trans))/(max(years_trans) - min(years_trans)) * setYears(future[,yEnd,],t)       )
    }  
    combined   <- mbind(tmpPast[,which(getYears(tmpPast,as.integer=TRUE) < firstyear),],
                        tmpTrans,
                        future[,which(getYears(future,as.integer=TRUE) > yEnd),])
  } else {
    stop("The past and future data need to have some overlap")
  }
  combined
}
