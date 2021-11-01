#' Read WDI
#'
#' Read-in WDI (World development indicators) data .Rds file as magpie
#' object.
#'
#' @param subtype A string. Type of WDI data that should be read. Use the
#' worldbank indicator abbreviation. Available subtypes are:
#' \itemize{
#' \item \code{"SP.POP.TOTL"}: Population, total
#' \item \code{"NY.GDP.MKTP.PP.KD"}: GDP,PPP (constant 2017 international Dollar)
#' \item \code{"NY.GDP.MKTP.PP.CD"}: GDP, PPP (current international Dollar)
#' \item \code{"NY.GDP.MKTP.CD"}: GDP MER (current US Dollar)
#' \item \code{"NY.GDP.MKTP.KD"}: GDP MER (constant 2010 USDollar)
#' \item \code{"NY.GDP.MKTP.KN"}: GDP LCU (constant LCU)
#' \item \code{"NY.GDP.MKTP.CN"}: GDP LCU (current LCU)
#' \item \code{"NY.GDP.DEFL.KD.ZG"}: Country GDP deflator (annual %)
#' \item \code{"SP.URB.TOTL.IN.ZS"}: Urban population (percentage of total)
#' \item \code{"NY.GDP.PCAP.CN"}:GDP, LCU, per capita (current LCU)
#' \item \code{"NY.GDP.PCAP.PP.KD"}: GDP PPP, per capita (2017 international $)
#' \item \code{"NY.GDP.PCAP.KD"}: GDP, MER, per capita (2010 US$)
#' \item \code{"NV.AGR.TOTL.KD"}: Ag GDP, MER, (2010 US$)
#' \item \code{"NV.AGR.TOTL.CD"}: Ag GDP, MER, (current US$)
#' \item \code{"NY.GDP.PCAP.CD"}: GDP MER, per capita(current US$)
#' \item \code{"NY.GDP.PCAP.PP.CD"}: GDP PPP, per capita (current int$)
#' \item \code{"PA.NUS.PPPC.RF"}: Price level ratio of PPP conversion factor (GDP) to market exchange rate
#' }
#'
#' @return A magpie object of the WDI data
#'
#' @seealso [madrat::readSource()]
#' @family "Past" population functions
#' @family "Past" GDP functions
#' @family "Past" GDPpc functions
#' @family WDI functions
#'
#' @examples \dontrun{
#' library(mrdrivers)
#' readSource("WDI", subtype = "SP.POP.TOTL")
#' }
#'
readWDI <- function(subtype) {
  x <- readr::read_rds("WDI.Rds")
  possibleSubtypes <- colnames(x)[!colnames(x) %in% c("iso3c", "country", "year")]

  if (!subtype %in% possibleSubtypes) {
     stop(glue("Bad subtype. Possible subtypes are: \n {paste0(possibleSubtypes, collapse = '\n')}."))
  }

  x[, c("iso2c", "year", subtype)] %>%
    # Replace "." with "_" otherwise readSource messes up the data dim
    dplyr::rename_with(~ gsub(".", "_", .x, fixed = TRUE)) %>%
    as.magpie(spatial = 1, tidy = TRUE, replacement = ".")
}
