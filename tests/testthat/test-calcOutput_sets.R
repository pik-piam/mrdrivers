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
  expect_correct_set_names(suppressMessages(calcOutput("GDPPast")))
  expect_correct_set_names(suppressMessages(calcOutput("GDPFuture")))
  expect_correct_set_names(suppressMessages(calcOutput("GDPpcPast")))
  expect_correct_set_names(suppressMessages(calcOutput("GDPpcFuture")))
  expect_correct_set_names(suppressMessages(calcOutput("GDPpc")))
  expect_correct_set_names(suppressMessages(calcOutput("GDP")))
  expect_correct_set_names(suppressMessages(calcOutput("DefaultDrivers")))
  expect_correct_set_names(suppressMessages(calcOutput("Labour")))
  #expect_correct_set_names(suppressMessages(calcOutput("RatioPPP2MER")))
})

# Save all calcOutputs for later use
calcs <- list("PopulationPast" = suppressMessages(calcOutput("PopulationPast")),
              "PopulationFuture" = suppressMessages(calcOutput("PopulationFuture")),
              "Population" = suppressMessages(calcOutput("Population")),
              "UrbanPast" = suppressMessages(calcOutput("UrbanPast")),
              "UrbanFuture" = suppressMessages(calcOutput("UrbanFuture")),
              "Urban" = suppressMessages(calcOutput("Urban")),
              "UrbanPop" = suppressMessages(calcOutput("UrbanPop")),
              "GDPpcPast" = suppressMessages(calcOutput("GDPpcPast")),
              "GDPpcFuture" = suppressMessages(calcOutput("GDPpcFuture")),
              "GDPpc" = suppressMessages(calcOutput("GDPpc")),
              "GDPPast" = suppressMessages(calcOutput("GDPPast")),
              "GDPFuture" = suppressMessages(calcOutput("GDPFuture")),
              "GDP" = suppressMessages(calcOutput("GDP")),
              "DefaultDrivers" = suppressMessages(calcOutput("DefaultDrivers")),
              "Labour" = suppressMessages(calcOutput("Labour")))

calcs2 <- list("PopulationPast" = suppressMessages(calcOutput("PopulationPast", aggregate = FALSE)),
               "PopulationFuture" = suppressMessages(calcOutput("PopulationFuture", aggregate = FALSE)),
               "Population" = suppressMessages(calcOutput("Population", aggregate = FALSE)),
               "UrbanPast" = suppressMessages(calcOutput("UrbanPast", aggregate = FALSE)),
               "UrbanFuture" = suppressMessages(calcOutput("UrbanFuture", aggregate = FALSE)),
               "Urban" = suppressMessages(calcOutput("Urban", aggregate = FALSE)),
               "UrbanPop" = suppressMessages(calcOutput("UrbanPop", aggregate = FALSE)),
               "GDPpcPast" = suppressMessages(calcOutput("GDPpcPast", aggregate = FALSE)),
               "GDPpcFuture" = suppressMessages(calcOutput("GDPpcFuture", aggregate = FALSE)),
               "GDPpc" = suppressMessages(calcOutput("GDPpc", aggregate = FALSE)),
               "GDPPast" = suppressMessages(calcOutput("GDPPast", aggregate = FALSE)),
               "GDPFuture" = suppressMessages(calcOutput("GDPFuture", aggregate = FALSE)),
               "GDP" = suppressMessages(calcOutput("GDP", aggregate = FALSE)),
               "DefaultDrivers" = suppressMessages(calcOutput("DefaultDrivers", aggregate = FALSE)),
               "Labour" = suppressMessages(calcOutput("Labour", aggregate = FALSE)))

calcs3 <- list("PopulationPast" = suppressMessages(calcOutput("PopulationPast", supplementary = TRUE)),
               "PopulationFuture" = suppressMessages(calcOutput("PopulationFuture", supplementary = TRUE)),
               "Population" = suppressMessages(calcOutput("Population", supplementary = TRUE)),
               "UrbanPast" = suppressMessages(calcOutput("UrbanPast", supplementary = TRUE)),
               "UrbanFuture" = suppressMessages(calcOutput("UrbanFuture", supplementary = TRUE)),
               "Urban" = suppressMessages(calcOutput("Urban", supplementary = TRUE)),
               "UrbanPop" = suppressMessages(calcOutput("UrbanPop", supplementary = TRUE)),
               "GDPpcPast" = suppressMessages(calcOutput("GDPpcPast", supplementary = TRUE)),
               "GDPpcFuture" = suppressMessages(calcOutput("GDPpcFuture", supplementary = TRUE)),
               "GDPpc" = suppressMessages(calcOutput("GDPpc", supplementary = TRUE)),
               "GDPPast" = suppressMessages(calcOutput("GDPPast", supplementary = TRUE)),
               "GDPFuture" = suppressMessages(calcOutput("GDPFuture", supplementary = TRUE)),
               "GDP" = suppressMessages(calcOutput("GDP", supplementary = TRUE)),
               "DefaultDrivers" = suppressMessages(calcOutput("DefaultDrivers", supplementary = TRUE)),
               "Labour" = suppressMessages(calcOutput("Labour", supplementary = TRUE)))


test_that("variable names", {

  expect_correct_variable_names <- function(x, y) {
    correct_names <- paste0(y, "_",
                            c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5",
                              "SDP", "SDP_EI", "SDP_MC", "SDP_RC", "SSP2EU"))
    expect_equal(getNames(calcs[[x]]), correct_names)
    expect_equal(getNames(calcs2[[x]]), correct_names)
  }

  purrr::map2(c("Population", "Urban", "UrbanPop", "GDPpc", "GDP", "Labour"),
              c("pop",        "urb",   "urb",      "gdppc", "gdp", "lab"),
              expect_correct_variable_names)
})

test_that("all positive", {
  purrr::map(calcs, ~ expect_equal(where(.x < 0)$summary[["TRUE"]], 0))
  purrr::map(calcs2, ~ expect_equal(where(.x < 0)$summary[["TRUE"]], 0))
})

test_that("no INF", {
  purrr::map(calcs, ~ expect_equal(where(is.infinite(.x))$summary[["TRUE"]], 0))
  purrr::map(calcs2, ~ expect_equal(where(is.infinite(.x))$summary[["TRUE"]], 0))
})

test_that("no NA", {
  purrr::map(calcs, ~ expect_equal(where(is.na(.x))$summary[["TRUE"]], 0))
  purrr::map(calcs2, ~ expect_equal(where(is.na(.x))$summary[["TRUE"]], 0))
})

test_that("no newline in descriptions", {
  purrr::map(calcs3, ~ expect_true(!grepl("\n", .x$description)))
})



