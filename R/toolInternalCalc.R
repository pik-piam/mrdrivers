toolInternalCalc <- function(driver, args) {
  # Check parallel map-reduce compatibility
  if (any(purrr::map_lgl(args, ~ length(.x) != 1 && 
                                 length(.x) != max(purrr::map_dbl(args, length))))) {
    stop(glue("Arguments to calc{driver} need to be either length 1 \\
               or equal to the length of the longest argument."))
  }

  # Map-reduce over the function arguments and call appropriate internal_calc 
  # function. Combine the returns using mbind and glue.
  purrr::pmap(args, get(glue("internal_calc{driver}"))) %>% 
    purrr::reduce(~ list(x = mbind(.x$x, .y$x),
                         weight = mbind(.x$weight, .y$weight),
                         unit = glue("{.x$unit} || {.y$unit}"),
                         description = glue("{.x$description} || {.y$description}")))
}
