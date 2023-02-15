toolInterpolateAndExtrapolate  <- function(data, extrapolate = TRUE) {
  # Get countries with partially missing values (exclude those that are still completely missing)
  stillMissing <- where(setYears(dimSums(data, dim = 2), "y0000") == 0)$true$regions
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
  data
}
