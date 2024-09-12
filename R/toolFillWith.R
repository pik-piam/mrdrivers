#' toolListFillWith
#'
#' Reduce list by filling the magpie objects with the following element of the list.
#'
#' @param x list of objects returned by caclOutput(..., supplementary = TRUE)
#' @keywords internal
#' @return The completed magpie object
toolListFillWith <- function(x) {
  if (length(x) == 1) {
    return(x[[1]])
  }

  sep <- if (length(x) == 2) " (completed with" else c(" (completed with", c(rep(",", length(x) - 3), " and"))
  closer <- c(rep("", length(sep) - 1), ")")
  helper <- purrr::map2(sep, closer, c)

  purrr::reduce2(x, helper,
                 ~ list(x = toolFillWith(.x$x, .y$x, .x$description, .y$description, verbose = TRUE),
                        weight = if (!is.null(.x$weight)) toolFillWith(.x$weight, .y$weight),
                        unit = glue("{.x$unit}"),
                        description = glue("{.x$description}{..3[1]} {.y$description}{..3[2]}")))
}


#' toolFillWith
#'
#' Fill in countries in "data" with no values, with values from "fill". Only countries with no data at all get filled
#' in!
#'
#' @param data magpie object to fill
#' @param fill magpie object to fill data with
#' @keywords internal
#' @return The completed magpie object
toolFillWith <- function(data, fill, dataName = "data", fillName = "fill", verbose = FALSE) {

  # Get countries with missing data (only zeros)
  missing <- where(dimSums(data, dim = 2) == 0)$true$regions

  # Get countries with data (not only zeros)
  withData <- where(dimSums(fill, dim = 2) != 0)$true$regions

  replace <- intersect(missing, withData)

  if (verbose) {
    message(glue("Data for the following countries is missing in {dataName}, and is taken instead from {fillName}: \\
                 {paste0(replace, collapse = ', ')}."))
  }

  # Use fill for countries with only zeros
  years <- intersect(getYears(data), getYears(fill))
  data[replace, years, ] <- fill[replace, years, ]
  data
}
