toolCheckUserInput <- function(driver, args) {
  # Check 'useMIData' argument
  if ("useMIData" %in% names(args) && !is.logical(args$useMIData)) {
     stop(glue("Bad argument to calc{driver}. 'useMIData' must be TRUE or FALSE."))
  }

  # Check 'extension2150' argument
  if (!args$extension2150 %in% c("none", "bezier", "constant")) {
     stop(glue("Bad argument to calc{driver}. 'extension2150' argument unknown."))
  }

  # Check 'FiveYearSteps' argument
  if (!is.logical(args$FiveYearSteps)) {
     stop(glue("Bad argument to calc{driver}. 'FiveYearSteps' must be TRUE or FALSE."))
  }

  # Check 'naming' argument
  if (!args$naming %in% c("indicator_scenario", "indicator.scenario")) {
     stop(glue("Bad argument to calc{driver}. 'naming' argument unknown."))
  }
}