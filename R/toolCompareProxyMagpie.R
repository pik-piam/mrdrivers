compare_proxy.magpie <- function(x, path = "x") { # nolint
  if (any(grepl("^ (origin:|creation date:)", comment(x)))) {
    comment(x) <- comment(x)[!grepl("^ (origin:|creation date:)", comment(x))]
  }
  list(object = as.vector(x), path = paste0("as.vector(", path, ")"))
}
