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
  expectCorrectSetNames(calcOutput("Population", extension2150 = "none"))
  expectCorrectSetNames(calcOutput("UrbanPast"))
  expectCorrectSetNames(calcOutput("UrbanFuture"))
  expectCorrectSetNames(calcOutput("Urban", extension2150 = "none"))
  expectCorrectSetNames(calcOutput("Labour", extension2150 = "none"))
  expectCorrectSetNames(calcOutput("GDPPast"))
  expectCorrectSetNames(calcOutput("GDPFuture", GDPFuture = "SSPs", unit = "constant 2017 Int$PPP"))
  expectCorrectSetNames(calcOutput("GDPpcPast"))
  expectCorrectSetNames(calcOutput("GDPpcFuture", GDPpcFuture = "SSPs", unit = "constant 2017 Int$PPP"))
  expectCorrectSetNames(calcOutput("GDP", extension2150 = "none"))
  expectCorrectSetNames(calcOutput("GDPpc", extension2150 = "none"))
  # I want to keep; expectCorrectSetNames(calcOutput("RatioPPP2MER"))
})


# Save all calcOutputs for later use
calcs <- list("PopulationPast" = calcOutput("PopulationPast"),
              "PopulationFuture" = calcOutput("PopulationFuture"),
              "Population" = calcOutput("Population", extension2150 = "none"),
              "UrbanPast" = calcOutput("UrbanPast"),
              "UrbanFuture" = calcOutput("UrbanFuture"),
              "Urban" = calcOutput("Urban", extension2150 = "none"),
              "GDPpcPast" = calcOutput("GDPpcPast"),
              "GDPpcFuture" = calcOutput("GDPpcFuture", GDPpcFuture = "SSPs", unit = "constant 2017 Int$PPP"),
              "GDPpc" = calcOutput("GDPpc", extension2150 = "none"),
              "GDPPast" = calcOutput("GDPPast"),
              "GDPFuture" = calcOutput("GDPFuture", GDPFuture = "SSPs", unit = "constant 2017 Int$PPP"),
              "GDP" = calcOutput("GDP", extension2150 = "none"),
              "Labour" = calcOutput("Labour", extension2150 = "none"))

calcs2 <- list("PopulationPast" = calcOutput("PopulationPast", aggregate = FALSE),
               "PopulationFuture" = calcOutput("PopulationFuture", aggregate = FALSE),
               "Population" = calcOutput("Population", extension2150 = "none", aggregate = FALSE),
               "UrbanPast" = calcOutput("UrbanPast", aggregate = FALSE),
               "UrbanFuture" = calcOutput("UrbanFuture", aggregate = FALSE),
               "Urban" = calcOutput("Urban", extension2150 = "none", aggregate = FALSE),
               "GDPpcPast" = calcOutput("GDPpcPast", aggregate = FALSE),
               "GDPpcFuture" = calcOutput("GDPpcFuture", GDPpcFuture = "SSPs", unit = "constant 2017 Int$PPP", aggregate = FALSE),
               "GDPpc" = calcOutput("GDPpc", extension2150 = "none", aggregate = FALSE),
               "GDPPast" = calcOutput("GDPPast", aggregate = FALSE),
               "GDPFuture" = calcOutput("GDPFuture", GDPFuture = "SSPs", unit = "constant 2017 Int$PPP", aggregate = FALSE),
               "GDP" = calcOutput("GDP", extension2150 = "none", aggregate = FALSE),
               "Labour" = calcOutput("Labour", extension2150 = "none", aggregate = FALSE))

calcs3 <- list("PopulationPast" = calcOutput("PopulationPast", supplementary = TRUE),
               "PopulationFuture" = calcOutput("PopulationFuture", supplementary = TRUE),
               "Population" = calcOutput("Population", extension2150 = "none", supplementary = TRUE),
               "UrbanPast" = calcOutput("UrbanPast", supplementary = TRUE),
               "UrbanFuture" = calcOutput("UrbanFuture", supplementary = TRUE),
               "Urban" = calcOutput("Urban", extension2150 = "none", supplementary = TRUE),
               "GDPpcPast" = calcOutput("GDPpcPast", supplementary = TRUE),
               "GDPpcFuture" = calcOutput("GDPpcFuture", GDPpcFuture = "SSPs", unit = "constant 2017 Int$PPP", supplementary = TRUE),
               "GDPpc" = calcOutput("GDPpc", extension2150 = "none", supplementary = TRUE),
               "GDPPast" = calcOutput("GDPPast", supplementary = TRUE),
               "GDPFuture" = calcOutput("GDPFuture", GDPFuture = "SSPs", unit = "constant 2017 Int$PPP", supplementary = TRUE),
               "GDP" = calcOutput("GDP", extension2150 = "none", supplementary = TRUE),
               "Labour" = calcOutput("Labour", extension2150 = "none", supplementary = TRUE))


test_that("variable names", {

  expectCorrectVariableNames <- function(x, y) {
    correctNames <- paste0(y, "_",
                            c("SSP1", "SSP2", "SSP3", "SSP4", "SSP5",
                              "SDP", "SDP_EI", "SDP_MC", "SDP_RC", "SSP2EU"))
    expect_equal(getNames(calcs[[x]]), correctNames)
    expect_equal(getNames(calcs2[[x]]), correctNames)
  }

  purrr::map2(c("Population", "Urban", "GDPpc", "GDP", "Labour"),
              c("pop", "urb", "gdppc", "gdp", "pop"),
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
    calcOutput("Population", extension2150 = "none", naming = "scenario", aggregate = FALSE, years = getYears(x))
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
  x <-  calcOutput("GDP", extension2150 = "none")
  y <-  calcOutput("GDP", extension2150 = "none", average2020 = FALSE)
  yNew2020 <- (y[, 2018, ] + y[, 2019, ] + y[, 2020, ] + y[, 2021, ] + y[, 2022, ]) / 5
  getYears(yNew2020) <- 2020
  getSets(yNew2020) <- getSets(y)
  y[, 2020, ] <- yNew2020

  expect_equal(x, y[, getYears(x), ])
})

test_that("average2020 is consistent", {
  x <- calcOutput("GDPpc", extension2150 = "none", naming = "scenario", aggregate = FALSE)
  y <- {
    calcOutput("GDP", extension2150 = "none", naming = "scenario", aggregate = FALSE) /
      calcOutput("Population",
                 extension2150 = "none",
                 naming = "scenario",
                 aggregate = FALSE,
                 years = seq(1965, 2100, 5))
  }
  # Remove comments before comparing
  comment(x) <- NULL
  expect_equal(x, y)
})

test_that("GDPpc factored by its weight is equal to GDP, in MER", {
  l <- calcOutput("GDPpc",
                  extension2150 = "none",
                  naming = "scenario",
                  unit = "constant 2005 US$MER",
                  aggregate = FALSE,
                  supplementary = TRUE)
  x <- l$x * l$weight

  y <- calcOutput("GDP",
                  extension2150 = "none",
                  naming = "scenario",
                  unit = "constant 2005 US$MER",
                  aggregate = FALSE)

  # Remove comments before comparing
  comment(y) <- NULL
  expect_equal(x, y)
})

test_that("ppp2mer is consistent with GDP in PPP and MER", {
  gdp1 <- calcOutput("GDP", unit = "constant 2017 US$MER", aggregate = FALSE)
  gdp2 <- calcOutput("GDP", unit = "constant 2017 Int$PPP", aggregate = FALSE)
  ppp2mer_def <- calcOutput("RatioPPP2MER", aggregate = FALSE)

  diff <- gdp1 / gdp2 - ppp2mer_def

  expect_lt(max(diff), 1e-12)

  # On regional level
  gdp1 <- calcOutput("GDP", unit = "constant 2017 US$MER")
  gdp2 <- calcOutput("GDP", unit = "constant 2017 Int$PPP")
  ppp2mer_def <- calcOutput("RatioPPP2MER")

  diff <- gdp1 / gdp2 - ppp2mer_def

  expect_lt(max(diff[, 2020, ]), 1e-2)
})
