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


test_that("Default calcOutput Population calls", {
  expect_snapshot_value(calcOutput("PopulationFuture"), style = "json2")
  expect_snapshot_value(calcOutput("PopulationPast"), style = "json2")
  expect_snapshot_value(calcOutput("Population"), style = "json2")
})

test_that("Default calcOutput GDP calls", {
  expect_snapshot_value(calcOutput("GDPFuture"), style = "json2")
  expect_snapshot_value(calcOutput("GDPPast"), style = "json2")
  expect_snapshot_value(calcOutput("GDP"), style = "json2")
})

test_that("Default calcOutput GDPpc calls", {
  expect_snapshot_value(calcOutput("GDPpcFuture"), style = "json2")
  expect_snapshot_value(calcOutput("GDPpcPast"), style = "json2")
  expect_snapshot_value(calcOutput("GDPpc"), style = "json2")
})

test_that("Default calcOutput Urban calls", {
  expect_snapshot_value(calcOutput("UrbanPast"), style = "json2")
  expect_snapshot_value(calcOutput("UrbanFuture"), style = "json2")
  expect_snapshot_value(calcOutput("Urban"), style = "json2")
})

test_that("Default calcOutput Labour calls", {
  expect_snapshot_value(calcOutput("LabourPast"), style = "json2")
  expect_snapshot_value(calcOutput("LabourFuture"), style = "json2")
  expect_snapshot_value(calcOutput("Labour"), style = "json2")
})

test_that("Default calcOutput RatioPPP2MER call", {
  expect_snapshot_value(calcOutput("RatioPPP2MER"), style = "json2")
})
