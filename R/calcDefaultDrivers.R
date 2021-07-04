#' Collect Default Model Drivers
#' @param drivers Which default drivers to collect.
calcDefaultDrivers <- function(drivers = c("pop", "GDP")) {
  
  if (!all(drivers %in% c("pop", "urban_pop", "GDP"))) {
     stop("Bad input for DefaultDrivers. Invalid 'drivers' argument.")
  }

  purrr::map(drivers, internal_calcDefaultDrivers) %>%
  purrr::reduce(~ list(x = mbind(.x$x, .y$x),
                       weight = NULL,
                       unit = glue::glue("{.x$unit} \n {.y$unit}"),
                       description = glue::glue("{.x$description} \n {.y$description}")))
}

internal_calcDefaultDrivers <- function (driver) {

  d <- switch(driver, 
             "pop" = calcOutput("Population", aggregate = FALSE, supplementary = TRUE),
             "urban_pop" = {urban_share <- calcOutput("Urban", aggregate = FALSE) 
                            urban <- urban_share * calcOutput("Population", aggregate = FALSE)[,, getNames(urban_share)]
                            getNames(urban, dim = 1) <- "urban"},
             "GDP" = calcOutput("GDPppp", aggregate = FALSE, supplementary = TRUE))

  return(list(x = d$x,
              weight = NULL,
              unit = d$unit,
              description = d$description))
}
