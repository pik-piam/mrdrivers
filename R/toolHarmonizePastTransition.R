#' Harmonization tool PastTransition
#'
#' Like all harmonization tools in mrdrivers, toolHarmonizePastTransition takes two magpie objects, 'past' and 'future',
#' and returns a single magpie object, i.e. the harmonized time-series. In this case, the harmonized time-series is
#' equal to 'past', in the years of 'past', and then transitions to 'future' by the year 'yEnd'. After yEnd, the
#' harmonized time-series is equal to 'future'. In the transition phase, a share of the absolute difference between
#' 'past' and future' in the last year of past' is added to future. This share starts at 1 in the last year of 'past'
#' and decreases linearly to 0 by yEnd.
#'
#' @param yEnd An integer designating the last year of the transition period.
#'
#' @inheritParams toolHarmonizePastGrFuture
#' @inherit toolHarmonizePastGrFuture return
#' @inheritSection toolHarmonizePastGrFuture Dimensions of 'past' and 'future'
toolHarmonizePastTransition <- function(past, future, yEnd) {

  lastPastYear <- max(getYears(past, as.integer = TRUE))
  firstFutureYear <- min(getYears(future, as.integer = TRUE))
  if (lastPastYear < firstFutureYear) {
    stop("The past and future data need to have some overlap")
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

  # Defined transition period and create combined scenario
  yearsFuture <- getYears(future, as.integer = TRUE)
  yearsTrans <- yearsFuture[which(yearsFuture >= lastPastYear & yearsFuture <= yEnd)]
  diffInlastPastYear <- tmpPast[, lastPastYear, ] - future[, lastPastYear, ]

  tmpTrans <- new.magpie(getItems(future, 1), yearsTrans, getNames(future), fill = 0)

  for (y in yearsTrans) {
    tmpTrans[, y, ] <- future[, y, ] +
      setYears(diffInlastPastYear, y) * ((max(yearsTrans) - y) / (max(yearsTrans) - min(yearsTrans)))
  }

  # The final harmonized object, is the combination of the tmpPast, the tmpTrans and future objects.
  combined <- mbind(tmpPast,
                    tmpTrans[, which(yearsTrans > lastPastYear), ],
                    future[, which(yearsFuture > yEnd), ]) %>%
    suppressWarnings()
  # Above warning sometimes appears: You are trying to mbind an empty magclass object. Is that really intended?
  # Answer is yes!

  combined
}
