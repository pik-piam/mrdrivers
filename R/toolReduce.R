toolReduce <- function(x, mbindOrFillWith = "mbind") {
  # Combine list elements using mbind and glue.
  if (mbindOrFillWith == "mbind") {
    x %>%
       purrr::reduce(~ list(x = mbind(.x$x, .y$x),
                            weight = mbind(.x$weight, .y$weight),
                            unit = glue("{.x$unit} || {.y$unit}"),
                            description = glue("{.x$description} || {.y$description}")))
  } else if (mbindOrFillWith == "fillWith") {

    if (length(x) > 1) {
      sep <- if (length(x) == 2) " (completed with" else c(" (completed with", c(rep(",", length(x) - 3), " and"))
      closer <- c(rep("", length(sep) - 1), ")")
      helper <- purrr::map2(sep, closer, c)
    } else helper <- NULL

    purrr::reduce2(x, helper,
                   ~ list(x = toolFillWith(.x$x, .y$x),
                          weight = toolFillWith(.x$weight, .y$weight),
                          unit = glue("{.x$unit}"),
                          description = glue("{.x$description}{..3[1]} {.y$description}{..3[2]}")))
  }
}
