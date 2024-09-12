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

expectCorrectOutput <- function(x) {
  # All positive, no Inf, and no NA
  expect_equal(where(x < 0)$summary[["TRUE"]], 0)
  expect_equal(where(is.infinite(x))$summary[["TRUE"]], 0)
  expect_equal(where(is.na(x))$summary[["TRUE"]], 0)
}

test_that("All scenarios work", {
  purrr::map(toolGetScenarioDefinition("GDPpc")$scenario,
             ~expectCorrectOutput(calcOutput("GDPpc", scenario = .x, extension2150 = "none")))
  purrr::map(toolGetScenarioDefinition("Population")$scenario,
             ~expectCorrectOutput(calcOutput("Population", scenario = .x, extension2150 = "none")))
  purrr::map(toolGetScenarioDefinition("GDP")$scenario,
             ~expectCorrectOutput(calcOutput("GDP", scenario = .x, extension2150 = "none")))
  purrr::map(toolGetScenarioDefinition("Labour")$scenario,
             ~expectCorrectOutput(calcOutput("Labour", scenario = .x, extension2150 = "none")))
  purrr::map(toolGetScenarioDefinition("Urban")$scenario,
             ~expectCorrectOutput(calcOutput("Urban", scenario = .x, extension2150 = "none")))
})
