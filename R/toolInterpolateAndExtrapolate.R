#' toolInterpolateAndExtrapolate
#'
#' Fill in years of partially missing countries through inter- and extrapolation.
#'
#' @param data A magpie object
#' @param extrapolate TRUE or FALSE
#' @keywords internal
#' @return list
toolInterpolateAndExtrapolate  <- function(data, extrapolate = TRUE) {
  if (!is.magpie(data)) {
    dataHelper <- data
    data <- data$x
  }

  # Get countries with partially missing values (exclude those that are still completely missing)
  stillMissing <- where(dimSums(data, dim = 2) == 0)$true$regions
  partiallyMissing <- setdiff(where(data == 0)$true$regions, stillMissing)
  # Interpolate and extrapolate
  for (i in partiallyMissing) {
    missingyears <- where(data[i, , ] == 0)$true$years
    if (!extrapolate) {
      # If not extrapolating, then confine the years to those between years with non 0 values
      missingyears <- missingyears[missingyears > min(where(data[i, , ] != 0)$true$years) &
                                     missingyears < max(where(data[i, , ] != 0)$true$years)]
      if (rlang::is_empty(missingyears)) next
    }
    data[i, missingyears, ] <- time_interpolate(dataset = data[i, , ][, missingyears, , invert = TRUE],
                                                interpolated_year = missingyears,
                                                extrapolation_type = "constant")
  }

  if (!(exists("dataHelper", inherits = FALSE))) {
    return(data)
  }

  list(x = data, weight = dataHelper$weight, unit = dataHelper$unit, description = dataHelper$description)
}
