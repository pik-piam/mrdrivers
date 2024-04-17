toolCheckUserInput <- function(driver, args) {
  permitted_args <- c("pastData", "futureData", "harmonization", "scenario", "unit", "yearEnd", "extension2150",
                      "average2020", "naming", "popAsWeight", "asShare", "GDPPast", "GDPFuture", "GDPpcPast", "GDPpcFuture",
                      "PopulationPast", "PopulationFuture", "LabourPast", "LabourFuture", "UrbanPast", "UrbanFuture")
  if (!all(names(args) %in% permitted_args)) {
    stop(glue("Bad argument to calc{driver}: '{names(args)[! names(args) %in% permitted_args]}'."))
  }

  # Check existence of pastData, futureData and harmonization
  if (!all(c("pastData", "futureData", "harmonization") %in% names(args)) &&
       any(c("pastData", "futureData", "harmonization") %in% names(args))) {
     stop("If you intend to overide the scenario argument, you must set pastData, futureData and hamrmonization.")
  }
  overrideScen <- if (all(c("pastData", "futureData", "harmonization") %in% names(args))) TRUE else FALSE

  # Check population scenario availability for any GDPpc scenario
  if (!overrideScen && driver == "GDPpc" &&
      !all(args$scenario %in% toolGetScenarioDefinition("Population")$scenario)) {
    stop("GDPpc scenarios require equivalent population scenarios to use as weight.")
  }

  # Check 'extension2150' argument
  if ("extension2150" %in% names(args) && !args$extension2150 %in% c("none", "bezier", "constant")) {
     stop(glue("Bad argument to calc{driver}. 'extension2150' has to be either 'none', 'bezier' or 'constant', \\
               not '{args$extension2150}'."))
  }

  # Check 'naming' argument
  if ("naming" %in% names(args) && !args$naming %in% c("indicator_scenario", "indicator.scenario", "scenario")) {
     stop(glue("Bad argument to calc{driver}. 'naming' has to be either 'indicator_scenario', 'indicator.scenario' \\
                or 'scenario', not '{args$naming}'."))
  }

  # Check 'unit' argument
  if ("unit" %in% names(args) && length(args$unit) != 1 && !grepl("^constant (2005|2017) ", args$unit)) {
     stop(glue("Bad argument to calc{driver}. Currently, only constant 2005 or 2017 dollars are accepted."))
  }

  # Check 'average2020' argument
  if ("average2020" %in% names(args) && length(args$average2020) != 1 && !is.logical(args$average2020)) {
     stop(glue("Bad argument to calc{driver}. 'average2020' has to be TRUE of FALSE."))
  }

  # Check parallel map-reduce compatibility
  if (any(purrr::map_lgl(args,
                         ~ !is.null(.x) &&
                         length(.x) != 1 &&
                         length(.x) != max(purrr::map_dbl(args, length))))) {
    stop(glue("Arguments to calc{driver} need to be either length 1 or equal to the length of the longest argument."))
  }
}
