#' Magpie proxy for waldo::compare
#'
#' Magpie proxy for waldo::compare
#'
#' @param x x
#' @param path "x"
#' @keywords internal
#' @return list
# nolint next: object_name_linter
compare_proxy.magpie <- function(x, path = "x") {
  if (any(grepl("^ (origin:|creation date:)", comment(x)))) {
    comment(x) <- comment(x)[!grepl("^ (origin:|creation date:)", comment(x))]
  }

  list(object = as.vector(x), path = paste0("as.vector(", path, ")"))
}
