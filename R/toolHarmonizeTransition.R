toolHarmonizeTransition <- function(past, future, yEnd) {
  # yEnd = end of transisiton, from this time on the future values are used
  # generate past data for all future scenarios
  firstyear <- min(getYears(future, as.integer = TRUE))
  if (firstyear > min(getYears(past, as.integer = TRUE))) {
    yearsPast <- getYears(past)[which(getYears(past, as.integer = TRUE) <= firstyear)]
    tmpPast <- setNames(past[, yearsPast, rep(1, ndata(future))], getNames(future))
    yearsTrans <- getYears(future, as.integer = TRUE)[which(
      getYears(future, as.integer = TRUE) >= firstyear & getYears(future, as.integer = TRUE) <= yEnd
    )]
    tmpTrans    <- new.magpie(getItems(future, 1), yearsTrans, getNames(future), fill = 0)
    for (t in yearsTrans) {
      tmpTrans[, t, ] <- (
        (max(yearsTrans) - t) / (max(yearsTrans) - min(yearsTrans)) * setYears(tmpPast[, firstyear, ], t)
      + (t - min(yearsTrans)) / (max(yearsTrans) - min(yearsTrans)) * setYears(future[, yEnd, ], t)
      )
    }
    combined   <- mbind(tmpPast[, which(getYears(tmpPast, as.integer = TRUE) < firstyear), ],
                        tmpTrans,
                        future[, which(getYears(future, as.integer = TRUE) > yEnd), ]) %>%
      suppressWarnings()
      # Above warning sometimes appears: You are trying to mbind an empty magclass object. Is that really intended?
      # Answer is yes!
  } else {
    stop("The past and future data need to have some overlap")
  }
  combined
}
