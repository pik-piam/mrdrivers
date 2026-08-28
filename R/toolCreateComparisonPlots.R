#' Plot differences between two versions
#'
#' Create line plots comparing two different versions of the scenarios
#'
#' @param new description
#' @param old description
#' @param tCutOff description
#'
#' @returns An invisible list of plots
#' @keywords internal
toolCreateComparisonPlots <- function(new = "current", old = "7.2.1", tCutOff = 2050) {
  rlang::check_installed("ggplot2")

  my_plot <- function(new_list, old_list, i) {
    new_drivers_i <- purrr::map(new_list, ~ tibble::as_tibble(.x[i, , ])) %>%
      purrr::list_rbind(names_to = "driver") %>%
      dplyr::mutate(v = "new")

    old_drivers_i <-  purrr::map(old_list, ~ tibble::as_tibble(.x[i, , ])) %>%
      purrr::list_rbind(names_to = "driver") %>%
      dplyr::mutate(v = "old")

    old_drivers_i %>%
      dplyr::bind_rows(new_drivers_i) %>%
      dplyr::rename("scen" = "variable") %>%
      dplyr::mutate(driver = factor(.data$driver, levels = c("pop", "gdp", "gdppc", "lab", "urb"))) %>%
      dplyr::filter(dplyr::between(.data$year, 2015, tCutOff)) %>%
      ggplot2::ggplot() +
      ggplot2::geom_line(ggplot2::aes(.data$year, .data$value, col = .data$scen, linetype = .data$v)) +
      ggplot2::facet_wrap(~.data$driver, ncol = 3, scales = "free_y") +
      ggplot2::ylab(NULL) +
      ggplot2::theme(legend.position = "inside",
                     legend.position.inside = c(0.82, 0.23),
                     legend.justification = c(0.5, 0.5),
                     legend.box = "horizontal",
                     legend.box.just = "center",
                     legend.title = ggplot2::element_blank())
  }

  new_list <- toolGetAllDrivers(new)$regions
  new_list_countries <- toolGetAllDrivers(new)$countries

  old_list <- toolGetAllDrivers(old)$regions
  old_list_countries <- toolGetAllDrivers(old)$countries

  plots <- NULL
  for (i in getItems(new_list$pop, 1)) plots[[i]] <- my_plot(new_list, old_list, i)
  filename <- glue::glue("compare_versions/plots/comparisonPlots_{old}_vs_{new}_reg{tCutOff}.Rds")
  readr::write_rds(plots, file = filename)

  plots2 <- NULL
  for (i in getItems(new_list_countries$pop, 1)) plots2[[i]] <- my_plot(new_list_countries, old_list_countries, i)
  filename <- glue::glue("compare_versions/plots/comparisonPlots_{old}_vs_{new}_{tCutOff}.Rds")
  readr::write_rds(plots2, file = filename)

  invisible(c(plots, plots2))
}
