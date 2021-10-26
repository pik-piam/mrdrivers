toolInterpolateAndExtrapolate  <- function(data) {
  # Get countries with partially missing values (exclude those that are still completely missing)
  stillMissing <- where(setYears(dimSums(data, dim = 2), "y0000") == 0)$true$region
  partiallyMissing <- setdiff(where(data == 0)$true$region, stillMissing)
  # Interpolate and extrapolate
  for (i in partiallyMissing) {
    missingyears <- where(data[i, , ] == 0)$true$years
    data[i, missingyears, ] <- time_interpolate(dataset = data[i, , ][, missingyears, , invert = TRUE],
                                               interpolated_year = missingyears,
                                               extrapolation_type = "constant")
  }
  data
}
