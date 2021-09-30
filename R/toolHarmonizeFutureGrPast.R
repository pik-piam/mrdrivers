toolHarmonizeFutureGrPast <- function(past, future) {
  firstFutureYear <- min(intersect(getYears(past, as.integer = TRUE),
                                   getYears(future, as.integer = TRUE)))
  lastPastYear <- max(getYears(past, as.integer = TRUE))
  if (lastPastYear < firstFutureYear) {
    stop("The past and future data need to have some overlap")
  }

  # Create future data for all past scenarios
  years_future <- getYears(future)[which(getYears(future, as.integer = TRUE) >= firstFutureYear)]
  tmpFuture <- future[, years_future, rep(1, ndata(past))]
  tmpFuture <- setNames(tmpFuture, getNames(past))
  tmpFuture[is.nan(tmpFuture)] <- 0

  # Create transition magpie object for all future scenarios
  years_past <- getYears(past)[which(getYears(past, as.integer = TRUE) < firstFutureYear)]
  tmpPast <- new.magpie(getRegions(past), years_past, getNames(past), fill = 0)

  # Use growth rates of future object
  tmpPast[,,] <- tmpFuture[,firstFutureYear,] * past[,years_past,] / past[,firstFutureYear,]
  tmpPast[is.nan(tmpPast)] <- 0

  mbind(tmpPast, tmpFuture)
}
