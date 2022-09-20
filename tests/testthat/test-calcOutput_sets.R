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

test_that("set names", {
  expectCorrectSetNames <- function(x) {
    correctSets <- c("d1.1" = "iso3c", "d2.1" = "year", "d3.1" = "variable")
    expect_equal(getSets(x), correctSets)
  }

  expectCorrectSetNames(calcOutput("PopulationPast"))
  expectCorrectSetNames(calcOutput("PopulationFuture"))
  expectCorrectSetNames(calcOutput("Population"))
  expectCorrectSetNames(calcOutput("UrbanPast"))
  expectCorrectSetNames(calcOutput("UrbanFuture"))
  expectCorrectSetNames(calcOutput("Urban"))
  expectCorrectSetNames(calcOutput("UrbanPop"))
  expectCorrectSetNames(calcOutput("GDPPast"))
  expectCorrectSetNames(calcOutput("GDPFuture"))
  expectCorrectSetNames(calcOutput("GDPpcPast"))
  expectCorrectSetNames(calcOutput("GDPpcFuture"))
  expectCorrectSetNames(calcOutput("GDPpc"))
  expectCorrectSetNames(calcOutput("GDP"))
  expectCorrectSetNames(calcOutput("DefaultDrivers"))
  expectCorrectSetNames(calcOutput("Labour"))
  # I want to keep; expectCorrectSetNames(calcOutput("RatioPPP2MER"))
})


# Save all calcOutputs for later use
calcs <- list("PopulationPast" = calcOutput("PopulationPast"),
              "PopulationFuture" = calcOutput("PopulationFuture"),
              "Population" = calcOutput("Population"),
              "UrbanPast" = calcOutput("UrbanPast"),
              "UrbanFuture" = calcOutput("UrbanFuture"),
              "Urban" = calcOutput("Urban"),
              "UrbanPop" = calcOutput("UrbanPop"),
              "GDPpcPast" = calcOutput("GDPpcPast"),
              "GDPpcFuture" = calcOutput("GDPpcFuture"),
              "GDPpc" = calcOutput("GDPpc"),
              "GDPPast" = calcOutput("GDPPast"),
              "GDPFuture" = calcOutput("GDPFuture"),
              "GDP" = calcOutput("GDP"),
              "DefaultDrivers" = calcOutput("DefaultDrivers"),
              "Labour" = calcOutput("Labour"))

calcs2 <- list("PopulationPast" = calcOutput("PopulationPast", aggregate = FALSE),
               "PopulationFuture" = calcOutput("PopulationFuture", aggregate = FALSE),
               "Population" = calcOutput("Population", aggregate = FALSE),
               "UrbanPast" = calcOutput("UrbanPast", aggregate = FALSE),
               "UrbanFuture" = calcOutput("UrbanFuture", aggregate = FALSE),
               "Urban" = calcOutput("Urban", aggregate = FALSE),
               "UrbanPop" = calcOutput("UrbanPop", aggregate = FALSE),
               "GDPpcPast" = calcOutput("GDPpcPast", aggregate = FALSE),
               "GDPpcFuture" = calcOutput("GDPpcFuture", aggregate = FALSE),
               "GDPpc" = calcOutput("GDPpc", aggregate = FALSE),
               "GDPPast" = calcOutput("GDPPast", aggregate = FALSE),
               "GDPFuture" = calcOutput("GDPFuture", aggregate = FALSE),
               "GDP" = calcOutput("GDP", aggregate = FALSE),
               "DefaultDrivers" = calcOutput("DefaultDrivers", aggregate = FALSE),
               "Labour" = calcOutput("Labour", aggregate = FALSE))

calcs3 <- list("PopulationPast" = calcOutput("PopulationPast", supplementary = TRUE),
               "PopulationFuture" = calcOutput("PopulationFuture", supplementary = TRUE),
               "Population" = calcOutput("Population", supplementary = TRUE),
               "UrbanPast" = calcOutput("UrbanPast", supplementary = TRUE),
               "UrbanFuture" = calcOutput("UrbanFuture", supplementary = TRUE),
               "Urban" = calcOutput("Urban", supplementary = TRUE),
               "UrbanPop" = calcOutput("UrbanPop", supplementary = TRUE),
               "GDPpcPast" = calcOutput("GDPpcPast", supplementary = TRUE),
               "GDPpcFuture" = calcOutput("GDPpcFuture", supplementary = TRUE),
               "GDPpc" = calcOutput("GDPpc", supplementary = TRUE),
               "GDPPast" = calcOutput("GDPPast", supplementary = TRUE),
               "GDPFuture" = calcOutput("GDPFuture", supplementary = TRUE),
               "GDP" = calcOutput("GDP", supplementary = TRUE),
               "DefaultDrivers" = calcOutput("DefaultDrivers", supplementary = TRUE),
               "Labour" = calcOutput("Labour", supplementary = TRUE))


test_that("variable names", {

  expectCorrectVariableNames <- function(x, y) {
    correctNames <- paste0(y, "_",
                            c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5",
                              "SDP", "SDP_EI", "SDP_MC", "SDP_RC", "SSP2EU"))
    expect_equal(getNames(calcs[[x]]), correctNames)
    expect_equal(getNames(calcs2[[x]]), correctNames)
  }

  purrr::map2(c("Population", "Urban", "UrbanPop", "GDPpc", "GDP", "Labour"),
              c("pop",        "urb",   "urb",      "gdppc", "gdp", "pop"),
              expectCorrectVariableNames)
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

test_that("GDPpc is equal to GDP divided by pop", {
  x <- calcOutput("GDPpc", extension2150 = "none", naming = "scenario", aggregate = FALSE)
  y <- {
    calcOutput("GDP", extension2150 = "none", naming = "scenario", aggregate = FALSE) /
    calcOutput("Population", extension2150 = "none", naming = "scenario", aggregate = FALSE)
    }
  # Remove comments before comparing
  comment(x) <- NULL
  expect_equal(x, y)
})


test_that("GDPpc factored by its weight is equal to GDP", {
  l <- calcOutput("GDPpc", extension2150 = "none", naming = "scenario", aggregate = FALSE, supplementary = TRUE)
  x <- l$x * l$weight

  y <- calcOutput("GDP", extension2150 = "none", naming = "scenario", aggregate = FALSE)

  # Remove comments before comparing
  comment(x) <- NULL
  expect_equal(x, y)
})

test_that("average2020 works", {
  x <-  calcOutput("GDP", extension2150 = "none", FiveYearSteps = FALSE)
  y <-  calcOutput("GDP", extension2150 = "none", average2020 = FALSE, FiveYearSteps = FALSE)
  yNew2020 <- (y[, 2018, ] + y[, 2019, ] + y[, 2020, ] + y[, 2021, ] + y[, 2022, ]) / 5
  getYears(yNew2020) <- 2020
  getSets(yNew2020) <- getSets(y)
  y[, 2020, ] <- yNew2020

  expect_equal(x, y)
})

test_that("average2020 is consistent", {
  x <- calcOutput("GDPpc", extension2150 = "none", naming = "scenario", aggregate = FALSE)
  y <- {
    calcOutput("GDP", extension2150 = "none", naming = "scenario", aggregate = FALSE) /
      calcOutput("Population", extension2150 = "none", naming = "scenario", aggregate = FALSE)
  }
  # Remove comments before comparing
  comment(x) <- NULL
  expect_equal(x, y)
})
