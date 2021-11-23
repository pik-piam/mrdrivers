compare_proxy.magpie <- function(x, path = "x") {
  if (any(grepl("^ (origin:|creation date:)", comment(x)))) {
    comment(x) <- comment(x)[!grepl("^ (origin:|creation date:)", comment(x))]
  }
  list(object = x, path = path)
}
