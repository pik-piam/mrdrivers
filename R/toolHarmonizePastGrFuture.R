toolHarmonizePastGrFuture <- function(past, future) {

  lastPastYear <- max(getYears(past, as.integer = TRUE))
  firstFutureYear <- min(getYears(future, as.integer = TRUE))
  if (lastPastYear < firstFutureYear) {
    stop("The past and future data need to have some overlap")
  }

  # If lastPastYear is not in future data, then create future data for lastPastYear
  # by linear interpolation. That way the return object really has all the past data.
  if (!lastPastYear %in% getYears(future, as.integer = TRUE)) {
     future <- toolAddInterpolatedYear(future, lastPastYear)
  }

  # Create past data for all future scenarios
  tmpPast <- past[, , rep(1, ndata(future))]
  tmpPast <- setNames(tmpPast, getNames(future))
  tmpPast[is.nan(tmpPast)] <- 0

  # Create magpie object for all future scenarios
  yearsFuture <- getYears(future)[which(getYears(future, as.integer = TRUE) > lastPastYear)]
  tmpFuture <- new.magpie(getItems(future, 1), yearsFuture, getNames(future), fill = 0)

  # Use growth rates of future object
  tmpFuture[, , ] <- tmpPast[, lastPastYear, ] * future[, yearsFuture, ] / future[, lastPastYear, ]
  tmpFuture[is.nan(tmpFuture)] <- 0

  mbind(tmpPast, tmpFuture)
}
