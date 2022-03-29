#' Harmonization tool PastGrFuture
#'
#' Like all harmonization tools in mrdrivers, toolHarmonizePastGrFuture takes two magpie objects, 'past' and 'future',
#' and returns a single magpie object, i.e. the harmonized time-series. In this case, the harmonized time-series is
#' equal to 'past', in the years of 'past', and then follows the same growth rates as 'future', for the years of
#' 'future'.
#'
#' @details # Dimensions of 'past' and 'future'
#'  If the 'future' object has multiple scenarios/datatypes, i.e. the length of the third dimension is larger than 1,
#'  then the a harmonized time-series is created for every scenario/datatype in future. The same 'past' object is used
#'  in every case - hence the requirement that 'past' only have one scenario/datatype.
#'
#' @param past A magpie object with only one scenario/datatype, i.e. the length of the third dimension should be 1.
#' @param future A magpie object.
#'
#' @return A magpie object with the same dimensions as 'future'.
toolHarmonizePastGrFuture <- function(past, future) {
  # Check dimensions of past
  if (dim(past)[3] != 1) {
    stop("The past data may only have one datatype, i.e. dim(past)[3] needs to be 1.")
  }

  # Check time overlap
  lastPastYear <- max(getYears(past, as.integer = TRUE))
  firstFutureYear <- min(getYears(future, as.integer = TRUE))
  if (lastPastYear < firstFutureYear) {
    stop("The past and future data need to have some overlap.")
  }

  # If lastPastYear is not in future data, then create future data for lastPastYear
  # by linear interpolation. That way the return object really has all the past data.
  if (!lastPastYear %in% getYears(future, as.integer = TRUE)) {
     future <- magclass::time_interpolate(future, lastPastYear, integrate_interpolated_years = TRUE)
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
