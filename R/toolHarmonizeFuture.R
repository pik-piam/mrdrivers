#' Harmonization tool Future
#'
#' Like all harmonization tools in mrdrivers, toolHarmonizeFuture takes two magpie objects, 'past' and 'future',
#' and returns a single magpie object, i.e. the harmonized time-series. In this case, the harmonized time-series is
#' always equal to 'future', in the years of 'future'. After that the harmonized time-series depends on the 'method'
#' argument chosen.
#'
#' @details # Dimensions of 'past' and 'future'
#'  If the 'past' object has multiple scenarios/datatypes, i.e. the length of the third dimension is larger than 1,
#'  then the a harmonized time-series is created for every scenario/datatype in past. The same 'future' object is used
#'  in every case - hence the requirement that 'future' only have one scenario/datatype.
#'
#' @param past A magpie object.
#' @param future A magpie object with only one scenario/datatype, i.e. the length of the third dimension should be 1.
#' @param method A string defining the harmonization method:
#'  \itemize{
#'    \item "level": the harmonized time-series is exactly equal to 'past' in the years before the first year
#'           of 'future'.
#'    \item "growth": the harmonized time-series follows the same growth rates as 'past', in the years
#'          before the first year of 'future'.
#'  }
#'
#' @return A magpie object with the same dimensions as 'past'.
#' @export
#'
#' @examples \dontrun{
#' toolHarmonizePast(past, future)
#' }
toolHarmonizeFuture <- function(past, future, method = "level") {
  if (!is.magpie(past) && !is.magpie(future)) {
    pastDescription <- past$description
    past <- past$x
    futureDescription <- future$description
    future <- future$x
  }

  # Check dimensions of past
  if (dim(future)[3] != 1) {
    stop("The future data may only have one datatype, i.e. dim(future)[3] needs to be 1.")
  }

  # Check time overlap
  lastPastYear <- max(getYears(past, as.integer = TRUE))
  firstFutureYear <- min(getYears(future, as.integer = TRUE))
  if (lastPastYear < firstFutureYear) {
    stop("The past and future data need to have some overlap.")
  }

  # If firstFutureYear is not in future data, then create past data for firstFutureYear
  # by linear interpolation. That way the return object really has all the past data.
  if (!firstFutureYear %in% getYears(past, as.integer = TRUE)) {
    past <- time_interpolate(past, firstFutureYear, integrate_interpolated_years = TRUE)
  }

  # Create future data for all past scenarios
  tmpFuture <- future[, , 1:ndata(past)]
  tmpFuture <- setNames(tmpFuture, getNames(past))
  tmpFuture[is.nan(tmpFuture)] <- 0

  # Create transition magpie object for all past scenarios
  yearsPast <- getYears(past, as.integer = TRUE)[which(getYears(past, as.integer = TRUE) < firstFutureYear)]
  tmpPast <- new.magpie(getItems(past, 1), yearsPast, getNames(past), fill = 0)

  if (method == "level") {
    tmpPast[, , ] <- past[, yearsPast, ]
  }
  if (method == "growth") {
    tmpPast[, , ] <- tmpFuture[, firstFutureYear, ] * past[, yearsPast, ] / past[, firstFutureYear, ]
    tmpPast[is.nan(tmpPast)] <- 0
  }

  x <- mbind(tmpFuture, tmpPast)
  x <- x[, sort(getYears(x)), ]

  if (!(exists("pastDescription", inherits = FALSE) && exists("futureDescription", inherits = FALSE))) {
    return(x)
  }

  description <- switch(
    method,
    "level" = glue("use {futureDescription} after {min(getYears(future, as.integer = TRUE))} and {pastDescription} \\
                    before then."),
    "growth" = glue("use {futureDescription} after {min(getYears(future, as.integer = TRUE))} and follow the growth \\
                    of {pastDescription} before then."),
    stop("Unkwown method.")
  )

  list(x = x, description = description)
}
