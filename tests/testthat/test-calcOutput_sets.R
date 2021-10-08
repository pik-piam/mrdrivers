skip_on_ci()
skip_on_covr()
skip_if_not(dir.exists(getOption("madrat_cfg")$mainfolder),
            glue("Skipped, because the madrat mainfolder {getOption('madrat_cfg')$mainfolder} \\
                  defined in 'tests/setup_madrat_config.R' could not be found.
                  If you wish to run this test please configure the file appropriately."))

test_that("set names", {

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
  expect_correct_set_names(suppressMessages(calcOutput("Labour")))
})


test_that("variable names", {

  expect_correct_variable_names <- function(x, y) {
    correct_names <- paste0(y, "_",
                            c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5",
                              "SDP", "SDP_EI", "SDP_MC", "SDP_RC", "SSP2EU"))
    expect_equal(getNames(x), correct_names)
  }

  expect_correct_variable_names(suppressMessages(calcOutput("Population")), "pop")
  expect_correct_variable_names(suppressMessages(calcOutput("Urban")), "urb")
  expect_correct_variable_names(suppressMessages(calcOutput("UrbanPop")), "urb")
  expect_correct_variable_names(suppressMessages(calcOutput("GDPpc")), "gdppc")
  expect_correct_variable_names(suppressMessages(calcOutput("GDP")), "gdp")
  expect_correct_variable_names(suppressMessages(calcOutput("Labour")), "lab")
})
