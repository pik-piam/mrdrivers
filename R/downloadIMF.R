#' @title downloadIMF
#'
#' @description  Download Wolrd Economic Outlook from the International Monetary Fund
#'
#' @seealso  [madrat::downloadSource()]
#' @seealso  [readIMF()]
#' @examples \dontrun{
#' downloadSource("IMF")
#' }
downloadIMF <- function() {
  url <- "https://www.imf.org/-/media/Files/Publications/WEO/WEO-Database/2021/WEOOct2021all.ashx"
  utils::download.file(url, "WEOall.xls", quiet = TRUE)

  # Compose meta data
  list(url           = url,
       doi           = "-",
       title         = "World Economic Outlook database of the IMF",
       description   = "World Economic Outlook database of the International Monetary Fund",
       unit          = "-",
       author        = "International Monetary Fund",
       release_date  = "October 2021",
       license       = "-",
       comment       = "-")
}
