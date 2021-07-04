#' downloadIMF
#'
#' @param subtype String
downloadIMF <- function(subtype) {
  url <- "https://www.imf.org/-/media/Files/Publications/WEO/WEO-Database/2020/02/WEOOct2020all.ashx"
  utils::download.file(url, "WEOOct2020all.xls")
}
