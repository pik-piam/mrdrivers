skip_on_ci()
skip_on_covr()
skip_if_not(dir.exists(getOption("madrat_cfg")$mainfolder),
            glue("Skipped, because the madrat mainfolder {getOption('madrat_cfg')$mainfolder} \\
                  defined in 'tests/setup_madrat_config.R' could not be found.
                  If you wish to run this test please configure the file appropriately."))

test_that("Default calcOutput calls vs snapshots", {
  expect_snapshot_value(suppressMessages(calcOutput("PopulationFuture")), style = "serialize")
  expect_snapshot_value(suppressMessages(calcOutput("PopulationPast")), style = "serialize")
  expect_snapshot_value(suppressMessages(calcOutput("Population")), style = "serialize")
  expect_snapshot_value(suppressMessages(calcOutput("GDPFuture")), style = "serialize")
  expect_snapshot_value(suppressMessages(calcOutput("GDPPast")), style = "serialize")
  expect_snapshot_value(suppressMessages(calcOutput("GDP")), style = "serialize")
  expect_snapshot_value(suppressMessages(calcOutput("GDPpcFuture")), style = "serialize")
  expect_snapshot_value(suppressMessages(calcOutput("GDPpcPast")), style = "serialize")
  expect_snapshot_value(suppressMessages(calcOutput("GDPpc")), style = "serialize")
  expect_snapshot_value(suppressMessages(calcOutput("UrbanPast")), style = "serialize")
  expect_snapshot_value(suppressMessages(calcOutput("UrbanFuture")), style = "serialize")
  expect_snapshot_value(suppressMessages(calcOutput("Urban")), style = "serialize")
  expect_snapshot_value(suppressMessages(calcOutput("UrbanPop")), style = "serialize")
  expect_snapshot_value(suppressMessages(calcOutput("DefaultDrivers")), style = "serialize")
  expect_snapshot_value(suppressMessages(calcOutput("Labour")), style = "serialize")
  expect_snapshot_value(suppressMessages(calcOutput("RatioPPP2MER")), style = "serialize")
})
