toolReduce <- function(x, mbindOrFillWith = "mbind") {
  # Combine list elements using mbind and glue.
  if (mbindOrFillWith == "mbind") {
    x %>%
       purrr::reduce(~ list(x = mbind(.x$x, .y$x),
                            weight = mbind(.x$weight, .y$weight),
                            unit = glue("{.x$unit} || {.y$unit}"),
                            description = glue("{.x$description} || {.y$description}")))
  } else if (mbindOrFillWith == "fillWith") {
    x %>%
       purrr::reduce(~ list(x = toolFillWith(.x$x, .y$x),
                            weight = toolFillWith(.x$weight, .y$weight),
                            unit = glue("{.x$unit}"),
                            description = glue("{.x$description} completed with {.y$description}")))
  }
}
