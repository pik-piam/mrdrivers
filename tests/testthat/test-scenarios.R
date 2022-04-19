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
  sd <- toolGetScenarioDefinition()
  purrr::map(sd$GDPpc$scenario, ~expectCorrectOutput(calcOutput("GDPpc", scenario = .x)))
  purrr::map(sd$Population$scenario,
             ~expectCorrectOutput(calcOutput("Population", scenario = .x)))
  # SSPsOld is not compatible with average2020 (throws warning)
  gdp1 <- sd$GDP$scenario[sd$GDP$scenario != "SSPsOld"]
  purrr::map(gdp1, ~expectCorrectOutput(calcOutput("GDP", scenario = .x)))
  expectCorrectOutput(calcOutput("GDP", scenario = "SSPsOld", average2020 = FALSE))
})


