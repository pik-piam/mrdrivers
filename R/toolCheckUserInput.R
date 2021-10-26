toolCheckUserInput <- function(driver, args) {
  # Check 'extension2150' argument
  if ("unit" %in% names(args) &&
      !args$unit %in% c("constant 2005 Int$PPP", "constant 2017 Int$PPP")) {
     stop(glue("Bad argument to calc{driver}. 'unit' argument unknown."))
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

  # Check 'naming' argument
  if (!args$naming %in% c("indicator_scenario", "indicator.scenario")) {
     stop(glue("Bad argument to calc{driver}. 'naming' argument unknown."))
  }
}
