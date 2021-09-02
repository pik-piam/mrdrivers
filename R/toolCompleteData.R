# Complete GDP or population data with data from "fill", and by time-interpolation
completeData <- function(data, fill) {
  fill <- time_interpolate(fill, interpolated_year = getYears(data), extrapolation_type = "constant")
  missing <- where(setYears(dimSums(data, dim = 2), "y0000") == 0)$true$region
  
  if (!all(missing %in% getRegions(fill))) {
     missing <- intersect(missing, getRegions(fill))
     warning("Some countries with missing data can't be filled.")
  }

  data[missing,,] <- fill[missing,,]

  still_missing <- where(setYears(dimSums(data, dim = 2), "y0000") == 0)$true$region
  partially_missing <- setdiff(where(data == 0)$true$region, still_missing)
  for(i in partially_missing){
    missingyears <- where(data[i,,] == 0)$true$years
    data[i, missingyears,] <- time_interpolate(dataset = data[i,,][,missingyears,,invert = TRUE],
                                               interpolated_year = missingyears,
                                               extrapolation_type = "constant")
  }
  data
}
