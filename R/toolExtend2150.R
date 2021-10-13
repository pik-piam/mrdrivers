# Extend beyond 2100 in 5 year time steps. Either with bezierExtension or constant.
# TODO: generalize the dates
toolExtend2150 <- function (data, extension2150){
  if (extension2150 != "none") {
    time_extend <- seq(2105, 2150, 5)
    if (extension2150 == "bezier") {
      data <- bezierExtension(data, time_extend)
    } else {
      helper <- getSets(data)
      data <- time_interpolate(data,
                               time_extend,
                               extrapolation_type = "constant",
                               integrate_interpolated_years = TRUE)
      # Time_interpolate destroys the setNames for some reason...
      getSets(data) <- helper
    }
  }
  data
}
  
bezierExtension <- function(data, time_extend) {

  extension <- new.magpie(getRegions(data), time_extend, getNames(data), fill = 0)

  slope = (data[,2100,] - data[,2095,]) / 5

  nr <- nregions(data)

  for (scen in getNames(data)) {
    y_start <- data[,2100,scen] %>% as.matrix()
    x_start <- matrix(2100, nrow = 249, ncol = 1)
    slope_start <- slope[,,scen] %>% as.matrix()

    slope_end <- slope_start / 2
    y_end <- y_start + slope_end * 50
    x_end <- matrix(2150, nrow = nr, ncol = 1)

    x_1 <- matrix(2110, nrow = nr, ncol = 1)
    y_1 <- y_start + slope_start * 10
    x_2 <- matrix(2140, nrow = nr, ncol = 1)
    y_2 <- y_end - slope_end * 10

    for (i in 1:nr) {
      if (y_start[i] == 0 || y_end[i] < 0) {
        next
      }

      p <- matrix(c(x_start[i],y_start[i], x_1[i],y_1[i], x_2[i],y_2[i], x_end[i],y_end[i]),
                  nrow = 4,
                  ncol = 2,
                  byrow = TRUE)

      # Get Bezier curve (Use max_dist method because it's super fast)
      bp <- bezier::bezier(t = seq(0, 1, length=10), p = p)
      pob <- bezier::pointsOnBezier(p = p, 
                                    method = "max_dist", 
                                    max.dist = max(abs(y_end[i] - y_start[i]), 50) / 100,
                                    print.progress = TRUE) %>% 
       suppressWarnings()
      # The above Warnings which are suppressed are
      #: "essentially perfect fit: summary may be unreliable"

      # Get y coordinates of points with x coordinates = time_extend 
      my_bezier_outflow <- pob$points %>% 
        tibble::as_tibble(.name_repair = ~ paste0("V", seq_along(.x))) %>% 
        # Complicatd / elegant use of function factories to get closest points to time_extend coordinates
        dplyr::mutate(dplyr::across(.data$V1, purrr::map(time_extend, ~ function(y){abs(y - .x)}) )) %>% 
        dplyr::filter(dplyr::if_any(tidyselect::contains("_"), ~.x == min(.x))) %>% 
        dplyr::pull(.data$V2)

      extension[i,,scen] <- my_bezier_outflow
    }
  }

  mbind(data, extension)
}
