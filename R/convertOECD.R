#' Convert OECD GDP / ratioPM
#'
#' Convert GDP / ratioPM data from OECD to data on ISO country level.
#'
#'
#' @param x MAgPIE object returned from readOECD
#' @inheritParams readOECD
#' @inherit readOECD return examples
#' @family OECD functions
convertOECD <- function(x, subtype) {

  if (subtype == "gdp") {
    # disaggregate "ROW"
    l <- c("ATG", "DMA", "FSM", "GRD", "IMN", "KNA", "PLW", "SYC") # "TLS"
    # ... time and scenario dependent?
    w <- calcOutput("Population", aggregate = FALSE)[l, 2010, "pop_SSP2"]
    xROW <- toolAggregate(x[c("ROW"), , ], "OECD_ROW2ISO.csv", weight = w)
    # delete ROW entry
    deleteROW <- setdiff(getItems(x, 1), c("ROW"))
    x <- x[deleteROW, , ]
    # add data for ROW
    x <- mbind(x[, getYears(xROW), ], xROW)
    # fill all the rest with 0
    x <- toolCountryFill(x, fill = 0)

  } else if (subtype == "ratioPM") {
    # disaggregate "ROW"
    l <- c("ATG", "DMA", "FSM", "GRD", "IMN", "KNA", "PLW", "SYC") # "TLS"
    lROW <- rep(c("ROW"), times = length(l))
    xROW <- x[lROW, , ]
    getCells(xROW) <- l
    # delete ROW entry
    deleteROW <- setdiff(getItems(x, 1), c("ROW"))
    x <- x[deleteROW, , ]
    # add data for ROW
    x <- mbind(x[, getYears(xROW), ], xROW)
    # fill all the rest with 1
    x <- toolCountryFill(x, fill = 1)
  } else if (subtype == "riskClass") {
    # fill NA with 1
    x[is.na(x)] <- 0
    # fill all the rest with 1
    x <- toolCountryFill(x, fill = 0)
  }

  x
}
