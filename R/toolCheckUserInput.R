#' Check user input
#'
#' toolCheckUserInput checks the user input.
#'
#' @param args A list with all the arguments passed to the parent function
#' @inheritParams calcDriver
#' @keywords internal
#' @return TRUE
toolCheckUserInput <- function(driver, args) { # nolint: cyclocomp_linter.
  permittedArgs <- c("scenario", "unit", "extension2150", "extension1960", "average2020", "naming", "popAsWeight",
                     "asShare", "pastData", "futureData", "harmonization")
  if (!all(names(args) %in% permittedArgs)) {
    stop(glue("Bad argument to calc{driver}: '{names(args)[! names(args) %in% permittedArgs]}'."))
  }

  # Check population scenario availability for any GDPpc scenario
  if (driver == "GDPpc" && !all(args$scenario %in% toolGetScenarioDefinition("Population")$scenario)) {
    stop("GDPpc scenarios require equivalent population scenarios to use as weight.")
  }

  # Check 'extension2150' argument
  if ("extension2150" %in% names(args) &&
        (!args$extension2150 %in% c("none", "bezier", "constant") || length(args$extension2150) != 1)) {
    stop(glue("Bad argument to calc{driver}. 'extension2150' has to be either 'none', 'bezier' or 'constant', \\
               not '{args$extension2150}'."))
  }

  # Check 'extension1960' argument
  if ("extension1960" %in% names(args) && !args$extension1960 %in% c("none", "MI-James", "MI", "James")) {
    stop(glue("Bad argument to calc{driver}. 'extension1960' has to be either 'none', 'MI', 'James', or a \\
              combination of both, e.g. 'MI-James', not '{args$extension2150}'."))
  }

  # Check 'popAsWeight' argument
  if ("popAsWeight" %in% names(args) && (!is.logical(args$popAsWeight) || length(args$popAsWeight) != 1)) {
    stop(glue("Bad argument to calc{driver}. 'popAsWeight' has to be either TRUE or FALSE"))
  }

  # Check 'naming' argument
  if ("naming" %in% names(args)) {
    if (args$naming != "scenario") {
      stop(glue("Bad argument to calc{driver}. 'naming' has to be 'scenario', not '{args$naming}'."))
    }
    warning("The 'naming' argument is deprecated. Please drop 'naming = \"scenario\"' from the function call.")
  }

  # Check 'unit' argument. Only constant dollars are allowed.
  if ("unit" %in% names(args) && (length(args$unit) != 1 || !grepl("^constant ", args$unit))) {
    stop(glue("Bad argument to calc{driver}. Only constant dollars are accepted."))
  }

  # Check 'average2020' argument
  if ("average2020" %in% names(args) && (length(args$average2020) != 1 || !is.logical(args$average2020))) {
    stop(glue("Bad argument to calc{driver}. 'average2020' has to be TRUE of FALSE."))
  }

  # Check parallel map-reduce compatibility
  if (any(purrr::map_lgl(args,
                         ~ !is.null(.x) &&
                           length(.x) != 1 &&
                           length(.x) != max(purrr::map_dbl(args, length))))) {
    stop(glue("Arguments to calc{driver} need to be either length 1 or equal to the length of the longest argument."))
  }
  invisible(TRUE)
}
