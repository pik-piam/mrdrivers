test_that("set names", {
  skip_on_ci()
  skip_on_covr()

  source("../../tests/madrat_config.R")

  skip_if(! dir.exists(getConfig()$mainfolder),
          glue("Skipped, because the madrat mainfolder {getConfig()$mainfolder} \\
               defined in 'tests/madrat_config.R' could not be found.
               If you wish to run this test please configure the file appropriately."))

  expect_correct_set_names <- function(x) {
    correct_sets <- c("d1.1" = "iso3c", "d2.1" = "year", "d3.1" = "variable")
    expect_equal(getSets(x), correct_sets)
  }

  expect_correct_set_names(suppressMessages(calcOutput("PopulationPast")))
  expect_correct_set_names(suppressMessages(calcOutput("PopulationFuture")))
  expect_correct_set_names(suppressMessages(calcOutput("Population")))
  expect_correct_set_names(suppressMessages(calcOutput("UrbanPast")))
  expect_correct_set_names(suppressMessages(calcOutput("UrbanFuture")))
  expect_correct_set_names(suppressMessages(calcOutput("Urban")))
  expect_correct_set_names(suppressMessages(calcOutput("UrbanPop")))
  expect_correct_set_names(suppressMessages(calcOutput("GDPpcPast")))
  expect_correct_set_names(suppressMessages(calcOutput("GDPpcFuture")))
  expect_correct_set_names(suppressMessages(calcOutput("GDPpc")))
  expect_correct_set_names(suppressMessages(calcOutput("GDPPast")))
  expect_correct_set_names(suppressMessages(calcOutput("GDPFuture")))
  expect_correct_set_names(suppressMessages(calcOutput("GDP")))
  expect_correct_set_names(suppressMessages(calcOutput("DefaultDrivers")))
})
