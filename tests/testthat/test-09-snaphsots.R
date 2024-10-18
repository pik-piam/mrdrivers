# # skip(message = "Skipped for clarity. Comment out line 1 in test-snapshots.R to run the test.") # nolint
skip_on_ci()
skip_on_covr()
skip_on_cran()
skip_if_not(dir.exists(madrat_mainfolder),
            glue("Skipped, because the madrat mainfolder {madrat_mainfolder} \\
                  defined in 'tests/setup_madrat_config.R' could not be found.
                  If you wish to run this test please configure the file appropriately."))

# Suppress messages for all tests
withr::local_file("tmp_messages.txt")
withr::local_message_sink(new = file("tmp_messages.txt", "w"), append = TRUE)

fh <- function(x) {
  tibble::as_tibble(x) %>%
    dplyr::filter(.data$year %in% c(1960, 1990, 2020, 2050, 2100, 2150)) %>%
    dplyr::mutate(value = signif(.data$value, 6)) %>%
    tidyr::unite(tidyselect::everything(), col = "all")
}

test_that("Default calcOutput Population calls", {
  expect_snapshot_value(fh(calcOutput("Population")), style = "json2")
})

test_that("Default calcOutput GDP calls", {
  expect_snapshot_value(fh(calcOutput("GDP")), style = "json2")
})

test_that("Default calcOutput GDPpc calls", {
  expect_snapshot_value(fh(calcOutput("GDPpc")), style = "json2")
})

test_that("Default calcOutput Urban calls", {
  expect_snapshot_value(fh(calcOutput("Urban")), style = "json2")
})

test_that("Default calcOutput Labour calls", {
  expect_snapshot_value(fh(calcOutput("Labour")), style = "json2")
})

test_that("Default calcOutput RatioPPP2MER call", {
  expect_snapshot_value(calcOutput("RatioPPP2MER"), style = "json2")
})
