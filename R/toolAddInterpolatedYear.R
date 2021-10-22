toolAddInterpolatedYear <- function(x, year) {
  lastYear <- max(getYears(x, as.integer = TRUE))
  firstYear <- min(getYears(x, as.integer = TRUE))
  if (year < firstYear || year > lastYear) {
     stop("The year must lie within the range given by the years in x.")
  }
  if (year %in% getYears(x, as.integer = TRUE)) {
     return(x)
  }

  tmp <- new.magpie(getRegions(x), 
                    years = c(getYears(x, as.integer = TRUE), year), 
                    names = getNames(x), 
                    sets = getSets(x),
                    fill = 0)
  tmp <- tmp[, order(getYears(tmp)), ]
  tmp[, getYears(x), ] <- x
  tmp[, year, ] <- time_interpolate(dataset = x, interpolated_year = year)
  tmp
}
