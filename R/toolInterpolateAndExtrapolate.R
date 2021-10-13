toolInterpolateAndExtrapolate  <- function(data) {
  # Get countries with partially missing values (exclude those that are still completely missing)
  still_missing <- where(setYears(dimSums(data, dim = 2), "y0000") == 0)$true$region
  partially_missing <- setdiff(where(data == 0)$true$region, still_missing)
  # Interpolate and extrapolate 
  for(i in partially_missing){
    missingyears <- where(data[i,,] == 0)$true$years
    data[i, missingyears,] <- time_interpolate(dataset = data[i,,][,missingyears,,invert = TRUE],
                                               interpolated_year = missingyears,
                                               extrapolation_type = "constant")
  }
  data
}
