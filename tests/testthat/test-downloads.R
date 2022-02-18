skip("skip")
skip_on_ci()
skip_on_covr()
skip_if_not(dir.exists(getOption("madrat_cfg")$mainfolder),
            glue("Skipped, because the madrat mainfolder {getOption('madrat_cfg')$mainfolder} \\
                  defined in 'tests/setup_madrat_config.R' could not be found.
                  If you wish to run this test please configure the file appropriately."))

localMadratCfg <- getOption("madrat_cfg")
localMadratCfg$sourcefolder <- paste0(localMadratCfg$mainfolder, "/tmp_test_download")
localMadratCfg$enablecache <- FALSE

unlink(localMadratCfg$sourcefolder, recursive = TRUE)

# Suppress messages for all tests
withr::local_file("tmp_messages.txt")
withr::local_message_sink(new = file("tmp_messages.txt", "w"), append = TRUE)


test_that("Download IMF", {
  withr::local_options(madrat_cfg = localMadratCfg)
  downloadSource("IMF")
  expect_true(file.exists(glue("{localMadratCfg$sourcefolder}/IMF/WEOall.xls")))
})

test_that("Download WDI", {
  withr::local_options(madrat_cfg = localMadratCfg)
  downloadSource("WDI")
  expect_true(file.exists(glue("{localMadratCfg$sourcefolder}/WDI/WDI.Rds")))
})

test_that("Download MissingIslands", {
  withr::local_options(madrat_cfg = localMadratCfg)
  downloadSource("MissingIslands")
  expect_true(all(file.exists(
    glue("{localMadratCfg$sourcefolder}/MissingIslands/pop_past_missing.csv"),
    glue("{localMadratCfg$sourcefolder}/MissingIslands/gdp_past_missing.csv")
  )))
})

test_that("Download UN_PopDiv", {
  withr::local_options(madrat_cfg = localMadratCfg)
  downloadSource("UN_PopDiv")
  expect_true(file.exists(
    glue("{localMadratCfg$sourcefolder}/UN_PopDiv/WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.xlsx")
  ))
})

unlink(localMadratCfg$sourcefolder, recursive = TRUE)
