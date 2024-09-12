#' Magpie proxy for waldo::compare
#'
#' Magpie proxy for waldo::compare
#'
#' @param x x
#' @param path "x"
#' @keywords internal
#' @return list
compare_proxy.magpie <- function(x, path = "x") { # nolint: object_name_linter.
  if (any(grepl("^ (origin:|creation date:)", comment(x)))) {
    comment(x) <- comment(x)[!grepl("^ (origin:|creation date:)", comment(x))]
  }

  list(object = as.vector(x), path = paste0("as.vector(", path, ")"))
}
