harmonizePastTransition <- function(past, future, yEnd) {
  # yEnd = end of transisiton, from this time on the future values are used
  firstyear <- min(getYears(future, as.integer = TRUE))
  # last year of past data, that also exist in future data
  lastyear <- max(intersect(getYears(past, as.integer = TRUE), getYears(future, as.integer = TRUE)))
  # generate past data for all future scenarios
  if (firstyear < max(getYears(past, as.integer = TRUE))) {                                                 
    years_past  <- getYears(past)[which(getYears(past,as.integer=TRUE) <= lastyear)]
    tmpPast     <- setNames(past[,years_past,rep(1, ndata(future))], getNames(future))
    years_trans <- getYears(future,as.integer=TRUE)[which(getYears(future,as.integer=TRUE) >= lastyear 
                                                          & getYears(future,as.integer=TRUE) <= yEnd)]
    diff_in_lastyear <- tmpPast[,lastyear,] - future[,lastyear,]
    tmpTrans <- new.magpie(getRegions(future), years_trans, getNames(future), fill=0)
    for(t in years_trans) {
      tmpTrans[,t,] <- future[,t,] + setYears(diff_in_lastyear, t) * ( (max(years_trans) - t)/(max(years_trans) - min(years_trans)) )  
    }  
    combined   <- mbind(tmpPast[,which(getYears(tmpPast, as.integer = TRUE) < lastyear),],
                        tmpTrans,
                        future[,which(getYears(future, as.integer = TRUE) > yEnd),])
  } else {  
    stop("The past and future data need to have some overlap")
  }
  combined
}
