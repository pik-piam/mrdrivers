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
  expectCorrectSetNames(calcOutput("PopulationFuture", futureData = "SSPs"))
  expectCorrectSetNames(calcOutput("Population", scenario = "SSPs", extension2150 = "none"))
  expectCorrectSetNames(calcOutput("UrbanPast"))
  expectCorrectSetNames(calcOutput("UrbanFuture", futureData = "SSPs"))
  expectCorrectSetNames(calcOutput("Urban", scenario = "SSPs", extension2150 = "none"))
  expectCorrectSetNames(calcOutput("Labour", scenario = "SSPs", extension2150 = "none"))
  expectCorrectSetNames(calcOutput("GDPPast"))
  expectCorrectSetNames(calcOutput("GDPFuture", futureData = "SSPs"))
  expectCorrectSetNames(calcOutput("GDPpcPast"))
  expectCorrectSetNames(calcOutput("GDPpcFuture", scenario = "SSPs"))
  expectCorrectSetNames(calcOutput("GDP", scenario = "SSPs", extension2150 = "none"))
  expectCorrectSetNames(calcOutput("GDPpc", scenario = "SSPs", extension2150 = "none"))
})


# Save all calcOutputs for later use
calcs <- list(
  "Population" = calcOutput("Population", scenario = "SSPs", extension2150 = "none"),
  "Labour" = calcOutput("Labour", scenario = "SSPs", extension2150 = "none"),
  "Urban" = calcOutput("Urban", scenario = "SSPs", extension2150 = "none"),
  "GDPpc" = calcOutput("GDPpc", scenario = "SSPs", extension2150 = "none"),
  "GDP" = calcOutput("GDP", scenario = "SSPs", extension2150 = "none")
)

calcs2 <- list(
  "Population" = calcOutput("Population", scenario = "SSPs", extension2150 = "none", aggregate = FALSE),
  "Labour" = calcOutput("Labour", scenario = "SSPs", extension2150 = "none", aggregate = FALSE),
  "Urban" = calcOutput("Urban", scenario = "SSPs", extension2150 = "none", aggregate = FALSE),
  "GDPpc" = calcOutput("GDPpc", scenario = "SSPs", extension2150 = "none", aggregate = FALSE),
  "GDP" = calcOutput("GDP", scenario = "SSPs", extension2150 = "none", aggregate = FALSE)
)

calcs3 <- list(
  "Population" = calcOutput("Population", scenario = "SSPs", extension2150 = "none", supplementary = TRUE),
  "Labour" = calcOutput("Labour", scenario = "SSPs", extension2150 = "none", supplementary = TRUE),
  "Urban" = calcOutput("Urban", scenario = "SSPs", extension2150 = "none", supplementary = TRUE),
  "GDPpc" = calcOutput("GDPpc", scenario = "SSPs", extension2150 = "none", supplementary = TRUE),
  "GDP" = calcOutput("GDP", scenario = "SSPs", extension2150 = "none", supplementary = TRUE)
)

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
  x <- calcOutput("GDPpc", scenario = "SSPs", extension2150 = "none", aggregate = FALSE)
  y <- {
    calcOutput("GDP", scenario = "SSPs", extension2150 = "none", aggregate = FALSE) /
      calcOutput("Population", scenario = "SSPs", extension2150 = "none", aggregate = FALSE, years = getYears(x))
  }
  # Remove comments before comparing
  comment(x) <- NULL
  expect_equal(x, y)
})


test_that("GDPpc factored by its weight is equal to GDP", {
  l <- calcOutput("GDPpc", scenario = "SSPs", extension2150 = "none", aggregate = FALSE, supplementary = TRUE)
  x <- l$x * l$weight

  y <- calcOutput("GDP", scenario = "SSPs", extension2150 = "none", aggregate = FALSE)

  # Remove comments before comparing
  comment(x) <- NULL
  expect_equal(x, y)
})

test_that("average2020 works", {
  x <-  calcOutput("GDP", scenario = "SSPs", extension2150 = "none")
  y <-  calcOutput("GDP", scenario = "SSPs", extension2150 = "none", average2020 = FALSE)
  yNew2020 <- (y[, 2018, ] + y[, 2019, ] + y[, 2020, ] + y[, 2021, ] + y[, 2022, ]) / 5
  getYears(yNew2020) <- 2020
  getSets(yNew2020) <- getSets(y)
  y[, 2020, ] <- yNew2020

  expect_equal(x, y[, getYears(x), ])
})

test_that("average2020 is consistent", {
  x <- calcOutput("GDPpc", scenario = "SSPs", extension2150 = "none",  aggregate = FALSE)
  y <- {
    calcOutput("GDP", scenario = "SSPs", extension2150 = "none", aggregate = FALSE) /
      calcOutput("Population",
                 scenario = "SSPs",
                 extension2150 = "none",
                 aggregate = FALSE,
                 years = seq(1965, 2100, 5))
  }
  # Remove comments before comparing
  comment(x) <- NULL
  expect_equal(x, y)
})

test_that("GDPpc factored by its weight is equal to GDP, in MER", {
  l <- calcOutput("GDPpc",
                  scenario = "SSPs",
                  extension2150 = "none",
                  unit = "constant 2017 US$MER",
                  aggregate = FALSE,
                  supplementary = TRUE)
  x <- l$x * l$weight

  y <- calcOutput("GDP",
                  scenario = "SSPs",
                  extension2150 = "none",
                  unit = "constant 2017 US$MER",
                  aggregate = FALSE)

  # Remove comments before comparing
  comment(y) <- NULL
  expect_equal(x, y)
})
