#' downloadIMF
#'
downloadIMF <- function() {
  url <- "https://www.imf.org/-/media/Files/Publications/WEO/WEO-Database/2021/WEOOct2021all.ashx"
  utils::download.file(url, "WEOall.xls")
}
