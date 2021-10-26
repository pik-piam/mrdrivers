toolHarmonizePastTransition <- function(past, future, yEnd) {
  # yEnd = end of transisiton, from this time on the future values are used
  firstyear <- min(getYears(future, as.integer = TRUE))
  # last year of past data, that also exist in future data
  lastyear <- max(intersect(getYears(past, as.integer = TRUE), getYears(future, as.integer = TRUE)))
  # generate past data for all future scenarios
  if (firstyear < max(getYears(past, as.integer = TRUE))) {
    yearsPast  <- getYears(past)[which(getYears(past, as.integer = TRUE) <= lastyear)]
    tmpPast     <- setNames(past[, yearsPast, rep(1, ndata(future))], getNames(future))
    yearsTrans <- getYears(future, as.integer = TRUE)[which(
      getYears(future, as.integer = TRUE) >= lastyear & getYears(future, as.integer = TRUE) <= yEnd
    )]
    diffInLastyear <- tmpPast[, lastyear, ] - future[, lastyear, ]
    tmpTrans <- new.magpie(getItems(future, 1), yearsTrans, getNames(future), fill = 0)
    for (t in yearsTrans) {
      tmpTrans[, t, ] <- future[, t, ] +
        setYears(diffInLastyear, t) * ((max(yearsTrans) - t) / (max(yearsTrans) - min(yearsTrans)))
    }
    combined   <- mbind(tmpPast[, which(getYears(tmpPast, as.integer = TRUE) < lastyear), ],
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
