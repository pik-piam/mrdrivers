#' Extend until 2150 in 5 year time steps
#'
#' toolExtend2150 extends a magpit object until 2150 in 5 year time steps. Either with bezierExtension or constant.
#'
#' @param data A list with "x", a magpie object, and "description" elements
#' @inheritParams calcDriver
#' @keywords internal
#' @return The modified data list.
toolExtend2150 <- function(data, extension2150) {
  # The bezier extension is only possible if there is data until 2100, and only affects years between 2100 and 2150.
  # It extends the time series in such a way as for the slope in 2105 to be half of that in 2100.
  if (extension2150 == "bezier" && "y2100" %in% getYears(data$x)) {
    data$x <- toolBezierExtension(data$x, seq(2105, 2150, 5))
    data$description <- glue("{data$description} Extended from 2100 to 2150 using bezier curves, resulting in a \\
                               smooth flattening of the scenario (the slope in 2150 is equal to half of that in \\
                               2100).")
  } else {
    helper <- getSets(data$x)
    data$x <- time_interpolate(data$x,
                               seq(2005, 2150, 5),
                               extrapolation_type = "constant",
                               integrate_interpolated_years = TRUE)
    # Time_interpolate destroys the setNames for some reason.
    getSets(data$x) <- helper
    data$description <- glue("{data$description} Extended from 2100 to 2150 using the constant 2100 value.")
  }
  data
}

toolBezierExtension <- function(data, timeExtend) {
  # Define bezier coordinates
  bc <- new.magpie(getItems(data, 1), c(2100, 2110, 2140, 2150), getNames(data), fill = 0)

  slopeStart <- (data[, 2100, ] - data[, 2095, ]) / 5
  slopeEnd <- slopeStart / 2

  bc[, 2100, ] <- data[, 2100, ]
  bc[, 2110, ] <- data[, 2100, ] + slopeStart * 10
  bc[, 2140, ] <- data[, 2100, ] + slopeEnd * 40
  bc[, 2150, ] <- data[, 2100, ] + slopeEnd * 50

  nr <- nregions(data)
  nd <- ndata(data)

  # If Bezier extension would lead to negative GDP, set bezier coordinates equal to start point
  # (comes down to a constant extension instead).
  # Magpie implementation would look like this:
  # for (i in 1:nr) for (j in 1:nd) if (bc[i, 2100, j] == 0 || bc[i, 2150, j] < 0) bc[i, , j] <- bc[i, 2100, j] # nolint
  # but it is faster to transform into tibble, do the operation there, and transform back
  bc <- bc %>%
    tibble::as_tibble() %>%
    tidyr::pivot_wider(names_from = "year", names_prefix = "y") %>%
    dplyr::mutate(dplyr::across(tidyselect::starts_with("y"),
                                ~dplyr::if_else(.data$y2150 < 0 | .data$y2100 == 0, .data$y2100, .x))) %>%
    tidyr::pivot_longer(tidyselect::starts_with("y"), names_to = "year", names_transform = ~sub("y", "", .x)) %>%
    as.magpie(spatial = "region", temporal = "year", tidy = TRUE)

  x <- rep(c(2100, 2110, 2140, 2150), nr * nd)
  y <- purrr::reduce(purrr::map(1:nd, ~purrr::reduce(purrr::map(1:nr, function(y) bc[y, , .x]), c)), c)
  z <- purrr::reduce(purrr::map(1:(nr * nd), ~rep(.x, 4)), c)

  # grid:: operations are super fast. Set the graphics device manually so that grid::bezierPoints returns the same
  # number of points on different devices, with potentially different default graphics devices.
  grDevices::pdf(NULL)
  bezierPoints <- grid::bezierGrob(x, y, id = z, default.units = "inches") %>%
    grid::bezierPoints() %>%
    purrr::map(~ list("x" = as.numeric(.x$x), "y" = as.numeric(.x$y)))
  grDevices::dev.off()

  id <- paste(purrr::reduce(purrr::map(getNames(data), ~rep(.x, nr)), c),
              rep(getItems(data, 1), nd),
              sep = "-")

  closestYear <- function(x) purrr::map_int(x, ~timeExtend[which.min(abs(.x - timeExtend))])

  extension <- purrr::map2(bezierPoints, id,
                           ~.x %>%
                             tibble::as_tibble() %>%
                             dplyr::mutate(id = .y)) %>%
    purrr::list_rbind() %>%
    # Complicated / elegant use of function factories to get closest points to timeExtend coordinates
    # First create columns with distance to timeExtend points
    dplyr::mutate(dplyr::across("x", purrr::map(timeExtend, ~ function(y) abs(y - .x)))) %>%
    # Then keep only the rows with the min values of the newly created columns
    dplyr::filter(dplyr::if_any(tidyselect::contains("_"), ~ .x == min(.x)), .by = "id") %>%
    dplyr::mutate(year = closestYear(x)) %>%
    tidyr::separate_wider_delim("id", names = c("data", "iso3c"), delim = "-") %>%
    dplyr::select("iso3c", "year", "data", "y") %>%
    as.magpie(spatial = "iso3c", temporal = "year", tidy = TRUE)

  mbind(data, extension)
}
