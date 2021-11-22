toolCheckUserInput <- function(driver, args) {
  # Check 'unit' argument
  if ("unit" %in% names(args) && !grepl("^constant (2005|2017) ", args$unit)) {
     stop(glue("Bad argument to calc{driver}. Currently, only constant 2005 or 2017 dollars are accepted."))
  }

  # Check 'extension2150' argument
  if (!args$extension2150 %in% c("none", "bezier", "constant")) {
     stop(glue("Bad argument to calc{driver}. 'extension2150' argument unknown."))
  }

  # Check 'FiveYearSteps' argument
  if (!is.logical(args$FiveYearSteps)) {
     if (args$FiveYearSteps) {
        warning("FiveYearSteps will be deprecated in the next release. Use the `years` argument of calcOutput instead.")
     }
     stop(glue("Bad argument to calc{driver}. 'FiveYearSteps' must be TRUE or FALSE."))
  }

  # Check 'average2020' argument
  if ("average2020" %in% names(args) && !is.logical(args$average2020)) {
     stop(glue("Bad argument to calc{driver}. 'average2020' must be TRUE or FALSE."))
  }

  # Check 'naming' argument
  if (!args$naming %in% c("indicator_scenario", "indicator.scenario", "scenario")) {
     stop(glue("Bad argument to calc{driver}. 'naming' argument unknown."))
  }
}
