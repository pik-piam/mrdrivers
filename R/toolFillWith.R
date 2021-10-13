# Complete GDP or population data with data from "fill"
toolFillWith <- function(data, fill) {
  # Return if one of the inputs is null
  if (is.null(fill) || is.null(data)) {
    return(data)
  }

  # Fill out the "fill" object
  fill <- time_interpolate(fill, interpolated_year = getYears(data), extrapolation_type = "constant")
  # Get countries with missing data (only zeros)
  missing <- where(setYears(dimSums(data, dim = 2), "y0000") == 0)$true$region
  
  if (!all(missing %in% getRegions(fill))) {
     left_over <- setdiff(missing, getRegions(fill))
     message(glue("NOTE: The following countries have not been filled: \\
                  {paste0(left_over, collapse = ', ')}."))
     missing <- intersect(missing, getRegions(fill))
  }

  # Use fill for countries with only zeros
  data[missing,,] <- fill[missing,,]
  data
}
