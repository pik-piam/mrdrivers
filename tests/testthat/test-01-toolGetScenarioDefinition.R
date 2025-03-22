test_that("toolGetScenarioDefinition works", {
  withr::local_file("tmp_messages.txt")
  withr::local_message_sink(new = file("tmp_messages.txt", "w"), append = TRUE)

  expect_s3_class(toolGetScenarioDefinition(), "tbl")
  expect_error(toolGetScenarioDefinition(1))
  expect_error(toolGetScenarioDefinition("GDP", 1))
  expect_s3_class(toolGetScenarioDefinition("GDP", "SSPs"), "tbl")
  expect_type(toolGetScenarioDefinition("GDP", "SSPs", aslist = TRUE), "list")
  expect_length(toolGetScenarioDefinition("GDP", "SSPs", aslist = TRUE), 5)
  expect_named(toolGetScenarioDefinition("GDP", "SSPs", aslist = TRUE),
               c("driver", "scenario", "pastData", "futureData", "harmonization"))
  expect_type(toolGetScenarioDefinition(getGroupShortcuts = TRUE), "list")

  expect_type(toolGetScenarioDefinition(c("GDP", "Population"), c("SDPs", "SSPs"), aslist = TRUE), "list")
  expect_length(toolGetScenarioDefinition(c("GDP", "Population"), c("SSPs", "SDPs"), aslist = TRUE), 5)
  expect_named(toolGetScenarioDefinition(c("GDP", "Population"), c("SSPs", "SDPs"), aslist = TRUE),
               c("driver", "scenario", "pastData", "futureData", "harmonization"))
  expect_s3_class(toolGetScenarioDefinition(scen = "ISIMIP"), "tbl")

  x <- toolGetScenarioDefinition("GDP", c("SSPs", "SDPs"))
  expect_s3_class(x, "tbl")
  expect_length(x, 5)
  expect_named(x, c("driver", "scenario", "pastData", "futureData", "harmonization"))
  expect_type(x[[1]], "character")
  expect_length(x[[1]], 2)
  expect_type(x[[2]], "character")
  expect_length(x[[2]], 2)
  expect_type(x[[3]], "character")
  expect_length(x[[3]], 2)

  expect_false(identical(toolGetScenarioDefinition("GDP", c("SSPs", "SDPs"), aslist = TRUE),
                         toolGetScenarioDefinition("GDP", c("SDPs", "SSPs"), aslist = TRUE)))
})

test_that("toolReplaceShortcuts works", {
  expect_length(toolReplaceShortcuts("SSPs"), 5)
  expect_length(toolReplaceShortcuts(c("SSPs", "SSP2")), 6)
  expect_length(toolReplaceShortcuts(c("SSPs", "SSP2IndiaDEAs")), 7)
})
