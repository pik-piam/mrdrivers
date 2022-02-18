# For countries in "data" with missing values, complete with values from "fill"
toolFillWith <- function(data, fill) {
  # Return if one of the inputs is null
  if (is.null(fill) || is.null(data)) {
    return(data)
  }

  # Fill out the "fill" object
  fill <- time_interpolate(fill, interpolated_year = getYears(data), extrapolation_type = "constant")
  # Get countries with missing data (only zeros)
  missing <- where(setYears(dimSums(data, dim = 2), "y0000") == 0)$true$region

  if (!all(missing %in% getItems(fill, 1))) {
     leftOver <- setdiff(missing, getItems(fill, 1)) # nolint
     message(glue("NOTE: The following countries have not been filled: {paste0(leftOver, collapse = ', ')}."))
     missing <- intersect(missing, getItems(fill, 1))
  }

  # Use fill for countries with only zeros
  data[missing, , ] <- fill[missing, , ]
  data
}
