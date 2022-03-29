#' Read SRES
#'
#' @description
#' `r lifecycle::badge('deprecated')`
#'
#' Read-in an SRES data csv file as magclass object. Works for both population
#' and GDP
#'
#' @param subtype data subtype. "pop_a1","pop_a2","pop_b1","pop_b2" or
#' "gdp_a1","gdp_a2","gdp_b1","gdp_b2"
#' @return magpie object of the SRES data. Units are million people or USD1990
#' market exchange rate.
#' @seealso [madrat::readSource()]
#' @examples \dontrun{
#' readSource("SRES", subtype = "pop_a1")
#' }
#' @keywords internal
readSRES <- function(subtype) {


  ### read in sres population or gdp estimates (source is the CIESIN database)

  x <- utils::read.csv(paste0(subtype, ".csv"))
  UNcode <- x[, which(colnames(x) == c("UN.Code"))]
  x <- x[, -which(colnames(x) %in% c("MESSAGE.Region", "IMAGE2.1.Region", "UN.Code", "Name", "IIASA.11.Regions", "ASF.Region", "X.UN.11.Regions", "SRES.4.Regions", "Source", "AIM.Region", "SRES.Region"))]
  names(x) <- gsub(pattern = "X", replacement = "y", x = names(x))
  x <- as.array(as.matrix(x))
  dimnames(x)[[1]] <- UNcode
  x <- as.magpie(x, spatial = 1)

  dimnames(x)[[1]] <- as.integer(dimnames(x)[[1]])
  getItems(x, 1) <- countrycode::countrycode(getItems(x, 1),
    origin = "iso3n", destination = "iso3c",
    custom_match = c("530" = "ANT", "736" = "SDN", "891" = "YUG")
  )
  dimnames(x)[[1]][which(dimnames(x)[[1]] == "YUG")] <- "SCG"
  getSets(x) <- c("iso3c", "year", "variable")
  getNames(x) <- subtype
  sres <- clean_magpie(x)

  # data mistake
  if (subtype == "sres_b2_pop") {
    sres <- sres * 1000
  }

  sres <- sres / 1000000

  return(sres)
}

#' @describeIn readSRES convert function
#' @param x magpie object returned by readSRES
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
    out <- out[from, , , invert = TRUE]
    return(out)
  }

  isoHistorical <- utils::read.csv2(system.file("extdata", "ISOhistorical.csv", package = "madrat"),
    stringsAsFactors = FALSE
  )

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
