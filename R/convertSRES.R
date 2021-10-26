#' Convert SRES data
#'
#' Convert SRES data on ISO country level.
#'
#'
#' @param x MAgPIE object containing SRES data
#' @param subtype data subtype. "pop_a1","pop_a2","pop_b1","pop_b2" or
#' "gdp_a1","gdp_a2","gdp_b1","gdp_b2"
#' @return SRES data as MAgPIE object aggregated to country level, all models,
#' all SRES-scenarios, GDP+Population. Units:
#' @examples
#' \dontrun{
#' a <- convertSRES(x, subtype = "pop_a1")
#' }
#'
convertSRES <- function(x, subtype) {

  splitSeries <- function(split, by, from, to, splityear) {
    if (any(!to %in% getItems(by, 1))) {
      warning(paste0("region ", to[which(!to %in% getItems(by, 1))], " not in the by dataset."))
    }
    if (any(to %in% getItems(split, 1))) {
stop("countries in to already exist in split")
}
    by <- setYears(by[intersect(getItems(by, 1), to), splityear, ], NULL)
    out <- setNames(by / dimSums(by, dim = 1), NULL) * setCells(split[from, , ], nm = "GLO")
    out <- clean_magpie(mbind(split, out))
    out <- out[from, , , invert = T]
    return(out)
  }

  isoHistorical <- utils::read.csv2(system.file("extdata", "ISOhistorical.csv", package = "madrat"),
                                    stringsAsFactors = F)

  if (substring(subtype, nchar(subtype) - 2) == "pop") {
    # split according to JAMES PPP

    popWDI <- readSource(type = "WDI", subtype = "SP.POP.TOTL")

    regionFrom <- "SCG"
    countriesTo <- isoHistorical$toISO[which(isoHistorical$fromISO == regionFrom)]
    x <- splitSeries(split = x, by = popWDI, from = regionFrom, to = countriesTo, splityear = "y1990")

  } else if (substring(subtype, nchar(subtype) - 2) == "gdp") {

    gdpJames <- readSource(type = "James", subtype = "IHME_USD05_PPP_pc")

    regionFrom <- "SCG"
    countriesTo <- isoHistorical$toISO[which(isoHistorical$fromISO == regionFrom)]
    x <- splitSeries(split = x, by = gdpJames, from = regionFrom, to = countriesTo, splityear = "y1990")

  } else {
stop("unknown subtype")
}


  x <- toolCountryFill(x, fill = 0, no_remove_warning = "ANT")
  x[is.na(x)] <- 0
  x
}
