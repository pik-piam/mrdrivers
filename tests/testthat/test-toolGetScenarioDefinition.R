test_that("toolGetScenarioDefinition works", {
  withr::local_file("tmp_messages.txt")
  withr::local_message_sink(new = file("tmp_messages.txt", "w"), append = TRUE)

  expect_type(toolGetScenarioDefinition(), "list")

  expect_error(toolGetScenarioDefinition(1))
  expect_error(toolGetScenarioDefinition("GDP", 1))

  expect_type(toolGetScenarioDefinition("GDP", "SSPs"), "list")
  expect_length(toolGetScenarioDefinition("GDP", "SSPs"), 1)
  expect_named(toolGetScenarioDefinition("GDP", "SSPs"), "GDP")

  expect_type(toolGetScenarioDefinition("GDP", "SSPs", unlist = TRUE), "list")
  expect_length(toolGetScenarioDefinition("GDP", "SSPs", unlist = TRUE), 3)
  expect_named(toolGetScenarioDefinition("GDP", "SSPs", unlist = TRUE), c("GDPPast", "GDPFuture", "GDPCalib"))

  expect_type(toolGetScenarioDefinition(c("GDP", "Population"), c("SDPs", "SSPs"), unlist = TRUE), "list")
  expect_length(toolGetScenarioDefinition(c("GDP", "Population"), c("SSPs", "SDPs"), unlist = TRUE), 6)
  expect_named(toolGetScenarioDefinition(c("GDP", "Population"), c("SSPs", "SDPs"), unlist = TRUE),
               c("GDPPast", "GDPFuture", "GDPCalib", "PopulationPast", "PopulationFuture", "PopulationCalib"))

  expect_type(toolGetScenarioDefinition(scen = "ISIMIP"), "list")
  expect_length(toolGetScenarioDefinition(scen = "ISIMIP"), 1)
  expect_named(toolGetScenarioDefinition(scen = "ISIMIP"), "Population")

  x <- toolGetScenarioDefinition("GDP", c("SSPs", "SDPs", "SSP2EU"), unlist = TRUE)
  expect_type(x, "list")
  expect_length(x, 3)
  expect_named(x, c("GDPPast", "GDPFuture", "GDPCalib"))
  expect_type(x[[1]], "character")
  expect_length(x[[1]], 3)
  expect_type(x[[2]], "character")
  expect_length(x[[2]], 3)
  expect_type(x[[3]], "character")
  expect_length(x[[3]], 3)

  expect_false(identical(toolGetScenarioDefinition("GDP", c("SSPs", "SDPs", "SSP2EU"), unlist = TRUE),
                         toolGetScenarioDefinition("GDP", c("SDPs", "SSPs", "SSP2EU"), unlist = TRUE)))
})
