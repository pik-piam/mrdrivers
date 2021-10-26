# Extend beyond 2100 in 5 year time steps. Either with bezierExtension or constant.
toolExtend2150 <- function(data, extension2150) {
  if (extension2150 != "none") {
    timeExtend <- seq(2105, 2150, 5)
    if (extension2150 == "bezier") {
      data <- bezierExtension(data, timeExtend)
    } else {
      helper <- getSets(data)
      data <- time_interpolate(data,
                               timeExtend,
                               extrapolation_type = "constant",
                               integrate_interpolated_years = TRUE)
      # Time_interpolate destroys the setNames for some reason...
      getSets(data) <- helper
    }
  }
  data
}

bezierExtension <- function(data, timeExtend) {

  extension <- new.magpie(getItems(data, 1), timeExtend, getNames(data), fill = 0)

  slope <- (data[, 2100, ] - data[, 2095, ]) / 5

  nr <- nregions(data)

  for (scen in getNames(data)) {
    yStart <- data[, 2100, scen] %>% as.matrix()
    xStart <- matrix(2100, nrow = 249, ncol = 1)
    slopeStart <- slope[, , scen] %>% as.matrix()

    slopeEnd <- slopeStart / 2
    yEnd <- yStart + slopeEnd * 50
    xEnd <- matrix(2150, nrow = nr, ncol = 1)

    x1 <- matrix(2110, nrow = nr, ncol = 1)
    y1 <- yStart + slopeStart * 10
    x2 <- matrix(2140, nrow = nr, ncol = 1)
    y2 <- yEnd - slopeEnd * 10

    for (i in 1:nr) {
      if (yStart[i] == 0 || yEnd[i] < 0) {
        next
      }

      p <- matrix(c(xStart[i], yStart[i], x1[i], y1[i], x2[i], y2[i], xEnd[i], yEnd[i]),
                  nrow = 4,
                  ncol = 2,
                  byrow = TRUE)

      # Get Bezier curve (Use max_dist method because it's super fast)
      # Keep this code for later: bp <- bezier::bezier(t = seq(0, 1, length = 10), p = p)
      pob <- bezier::pointsOnBezier(p = p,
                                    method = "max_dist",
                                    max.dist = max(abs(yEnd[i] - yStart[i]), 50) / 100,
                                    print.progress = FALSE) %>%
       suppressWarnings()
      # The above Warnings which are suppressed are
      # : "essentially perfect fit: summary may be unreliable"

      # Get y coordinates of points with x coordinates = timeExtend
      myBezierOutflow <- pob$points %>%
        tibble::as_tibble(.name_repair = ~ paste0("V", seq_along(.x))) %>%
        # Complicatd / elegant use of function factories to get closest points to timeExtend coordinates
        dplyr::mutate(dplyr::across(.data$V1, purrr::map(timeExtend, ~ function(y) {
abs(y - .x)
}))) %>%
        dplyr::filter(dplyr::if_any(tidyselect::contains("_"), ~ .x == min(.x))) %>%
        dplyr::pull(.data$V2)

      extension[i, , scen] <- myBezierOutflow
    }
  }

  mbind(data, extension)
}
