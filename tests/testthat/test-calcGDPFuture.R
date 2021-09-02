# library(madrat)
# setConfig(mainfolder = "~/madrat_folder/")
#
# test_that("GDP", {
#   x <- calcOutput("GDP", extension2150 = "none", FiveYearSteps = FALSE, aggregate = FALSE)
#   expect_equal(getSets(x), c(d1.1 = "iso3c", d2.1 = "year", d3.1  = "scen"))
# })
# T
# test_that("GDPpc", {
#   x <- calcOutput("GDPpc", extension2150 = "none", FiveYearSteps = FALSE, aggregate = FALSE)
#   expect_equal(getSets(x), c(d1.1 = "iso3c", d2.1 = "year", d3.1  = "scen"))
# })
#
# test_that("Population", {
#   x <- calcOutput("Population", extension2150 = "none", FiveYearSteps = FALSE, aggregate = FALSE)
#   expect_equal(getSets(x), c(d1.1 = "iso3c", d2.1 = "year", d3.1  = "scen"))
# })
#
# test_that("Urban", {
#   x <- calcOutput("Urban", extension2150 = "none", FiveYearSteps = FALSE, aggregate = FALSE)
#   expect_equal(getSets(x), c(d1.1 = "iso3c", d2.1 = "year", d3.1  = "scen"))
# })
