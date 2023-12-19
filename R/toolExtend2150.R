# Extend until 2150 in 5 year time steps. Either with bezierExtension or constant.
toolExtend2150 <- function(data, extension2150) {
  if (extension2150 != "none") {
    # The bezier extension is only possible if there is data until 2100, and only affects years between 2100 and 2150.
    # It extends the time series in such a way as for the slope in 2105 to be half of that in 2100.
    if (extension2150 == "bezier" && "y2100" %in% getYears(data)) {
      data <- bezierExtension(data, seq(2105, 2150, 5))
    } else {
      helper <- getSets(data)
      data <- time_interpolate(data,
                               seq(2005, 2150, 5),
                               extrapolation_type = "constant",
                               integrate_interpolated_years = TRUE)
      # Time_interpolate destroys the setNames for some reason...
      getSets(data) <- helper
    }
  }
  data
}


bezierExtension <- function(data, timeExtend) {
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
  # (comes down to a constant extension instead)
  for (i in 1:nr) for (j in 1:nd) if (bc[i, 2100, j] == 0 || bc[i, 2150, j] < 0) bc[i, , j] <- bc[i, 2100, j]

  x <- rep(c(2100, 2110, 2140, 2150), nr * nd)
  y <- purrr::reduce(purrr::map(1:nd, ~purrr::reduce(purrr::map(1:nr, function(y) bc[y, , .x]), c)), c)
  z <- purrr::reduce(purrr::map(1:(nr * nd), ~rep(.x, 4)), c)

  # grid operations are super fast. Have to convert the results of bezierpoints from inches back to values.
  bezierPoints <- grid::bezierGrob(x, y, id = z) %>%
    grid::bezierPoints() %>%
    purrr::map(~ list("x" = grid::convertX(.x$x, "npc", valueOnly = TRUE),
                      "y" = grid::convertY(.x$y, "npc", valueOnly = TRUE)))

  id <- paste(purrr::reduce(purrr::map(getNames(data), ~rep(.x, nr)), c),
              rep(getItems(data, 1), nd),
              sep = "-")

  closestYear <- function(x) purrr::map_int(x, ~timeExtend[which.min(abs(.x - timeExtend))])

  extension <- purrr::map2(bezierPoints, id,
                           ~.x %>% tibble::as_tibble() %>% dplyr::mutate(id = .y)) %>%
    purrr::list_rbind() %>%
    # Complicated / elegant use of function factories to get closest points to timeExtend coordinates
    # First create columns with distance to timeExtend points
    dplyr::mutate(dplyr::across("x", purrr::map(timeExtend, ~ function(y) abs(y - .x)))) %>%
    # Then keep only the rows with the min values of the newly created columns
    dplyr::filter(dplyr::if_any(tidyselect::contains("_"), ~ .x == min(.x)), .by = "id") %>%
    dplyr::mutate(year = closestYear(x)) %>%
    tidyr::separate_wider_delim("id", names = c("data", "iso3c"), delim = "-") %>%
    dplyr::select("iso3c", "year", "data", "y") %>%
    as.magpie()

  mbind(data, extension)
}
