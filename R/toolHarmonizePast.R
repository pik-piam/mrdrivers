#' Harmonization tool Past
#'
#' Like all harmonization tools in mrdrivers, toolHarmonizePast takes two magpie objects, 'past' and 'future',
#' and returns a single magpie object, i.e. the harmonized time-series. In this case, the harmonized time-series is
#' always equal to 'past', in the years of 'past'. After that the harmonized time-series depends on the 'method'
#' argument chosen.
#'
#' @details # Dimensions of 'past' and 'future'
#'  If the 'future' object has multiple scenarios/datatypes, i.e. the length of the third dimension is larger than 1,
#'  then the a harmonized time-series is created for every scenario/datatype in future. The same 'past' object is used
#'  in every case - hence the requirement that 'past' only have one scenario/datatype.
#'
#' @param past A magpie object with only one scenario/datatype, i.e. the length of the third dimension should be 1.
#' @param future A magpie object.
#' @param method A string defining the harmonization method:
#'  \itemize{
#'    \item "level": the harmonized time-series is exactly equal to 'future' in the years after the last year
#'           of 'past'.
#'    \item "growth": the harmonized time-series follows the same growth rates as 'future', in the years
#'          after the last year of 'past'.
#'    \item "transition": the harmonized time-series transitions to 'future' by the year 'yEnd'. After yEnd, the
#'          harmonized time-series is equal to 'future'. In the transition phase, a share of the absolute difference
#'          between past' and future' in the last year of past' is added to future. This share starts at 1 in the
#'          last year of 'past' and decreases linearly to 0 by yEnd.
#'  }
#' @param yEnd Additional input for "transition" method. Year by which the transition period is completed.
#'
#' @return A magpie object with the same dimensions as 'future'.
#' @export
#'
#' @examples \dontrun{
#' toolHarmonizePast(past, future)
#' }
toolHarmonizePast <- function(past, future, method = "level", yEnd = 2100) {
  if (!is.magpie(past) && !is.magpie(future)) {
    pastDescription <- past$description
    past <- past$x
    futureDescription <- future$description
    future <- future$x
  }

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
    future <- time_interpolate(future, lastPastYear, integrate_interpolated_years = TRUE)
  }

  # Create past data for all future scenarios
  tmpPast <- past[, , rep(1, ndata(future))]
  tmpPast <- setNames(tmpPast, getNames(future))
  tmpPast[is.nan(tmpPast)] <- 0

  # Create magpie object for all future scenarios
  yearsFuture <- getYears(future, as.integer = TRUE)[which(getYears(future, as.integer = TRUE) > lastPastYear)]
  tmpFuture <- new.magpie(getItems(future, 1), yearsFuture, getNames(future), fill = 0)

  if (method == "level") {
    tmpFuture[, , ] <- future[, yearsFuture, ]
  }
  if (method == "growth") {
    tmpFuture[, , ] <- tmpPast[, lastPastYear, ] * future[, yearsFuture, ] / future[, lastPastYear, ]
    tmpFuture[is.nan(tmpFuture)] <- 0
  }
  if (method == "parallel") {
    tmpFuture[, , ] <- future[, yearsFuture, ] + (tmpPast[, lastPastYear, ] - future[, lastPastYear, ])
  }
  if (method == "transition") {
    yearsTrans <- yearsFuture[which(yearsFuture <= yEnd)]
    tmpTrans <- new.magpie(getItems(future, 1), yearsTrans, getNames(future), fill = 0)

    diffInlastPastYear <- tmpPast[, lastPastYear, ] - future[, lastPastYear, ]

    for (y in yearsTrans) {
      tmpTrans[, y, ] <- future[, y, ] + setYears(diffInlastPastYear, y) * ((yEnd - y) / (yEnd - lastPastYear))
    }

    yearsPostTrans <- yearsFuture[which(yearsFuture > yEnd)]
    tmpFuture <- mbind(tmpTrans, future[, yearsPostTrans, ]) %>% suppressWarnings()
    # Above warning sometimes appears: You are trying to mbind an empty magclass object. Is that really intended?
    # Answer is yes!
  }

  x <- mbind(tmpPast, tmpFuture)

  if (!(exists("pastDescription", inherits = FALSE) && exists("futureDescription", inherits = FALSE))) {
    return(x)
  }

  description <- switch(
    method,
    "level" = glue("use {pastDescription} until {max(getYears(past, as.integer = TRUE))} and then switch directly \\
                   to {futureDescription}."),
    "growth" = glue("use {pastDescription} until {max(getYears(past, as.integer = TRUE))} and then follow the \\
                    growth of {futureDescription}."),
    "transition" = glue("use {pastDescription} until {max(getYears(past, as.integer = TRUE))} and converge towards \\
                         {futureDescription} by {yEnd}."),
    stop("Unkwown method.")
  )

  list(x = x, description = description)
}
