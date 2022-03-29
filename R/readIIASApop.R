#' Read IIASApop
#' 
#' @description
#' `r lifecycle::badge('deprecated')`  
#' 
#' Read-in an population data csv file as magclass object
#' 
#' @return magpie object of the population data
#' @seealso \code{\link{readSource}}
#' @examples
#' \dontrun{ a <- readSource(type="IIASApop")}
#' @keywords internal
readIIASApop <- function() {
  data <- utils::read.csv("Data A1 Country total population-SSPs.csv")
  data <- data[, -ncol(data)]
  data[,"cc"] <- countrycode::countrycode(data[,"cc"], origin = "iso3n", destination = "iso3c", 
                                          custom_match = c("530" = "ANT", "736" = "SDN", "830" = "XAA"))
  data <- data[!is.na(data[, c("cc")]),]
  x <- as.magpie(data, datacol = 3)
  getNames(x) <- paste0("pop_", gsub("ssp", "SSP", getNames(x)))
  x
}  

# Convert IIASApop
#
# Convert population data to data on ISO country level.
#
#
# @param x MAgPIE object containing population data mixed country-region
# resolution
# @return population data as MAgPIE object aggregated to country level
convertIIASApop <- function(x) {

  #-------------------- allocation of aggretations -------------------------------
  # allocate Channel Islands, cc 830, XAA
  jey <- x["XAA", , ]
  ggy <- jey
  getItems(jey, 1)  <- "JEY"
  jey <- jey * 99000 / (99000 + 65228)   # population data from wikipedia as weight
  getItems(ggy, 1) <- "GGY"
  ggy <- ggy * 65228 / (99000 + 65228)   # population data from wikipedia as weight

  # allocate Netherland ANtills, cc 530, ANT
  cuw <- x["ANT", , ]
  bes <- cuw
  getItems(cuw, 1)  <- "CUW"
  cuw <- cuw * 150563 / (150563 + 18012)  # population data from wikipedia as weight
  getItems(bes, 1) <- "BES"
  bes <- bes * 18012 / (150563 + 18012)   # population data from wikipedia as weight (18012)
                                      # Bonaire-13389, Saba-1737, Sint Eustatius-2886

  # delete XAA und ANT entry
  deleteISO <- setdiff(getItems(x, 1), c("XAA", "ANT"))
  x <- x[deleteISO, , ]
  # add data for Jersey(JEY), Guernsey(GGY), Curacao(CUW) and BES
  x <- mbind(x, jey, ggy, cuw, bes)
  #--------------------------------------------------------------------------------

  #---------------------- add TWN data --------------------------------------------
  twn <- new.magpie("TWN", getYears(x), getNames(x))
  twnDataMedium <- dimSums(readSource("PopulationTWN", subtype = "medium")[, , "Year-end_Population", pmatch = TRUE]
                           [, , c("0-14 years",
                                  "15-64 years",
                                  "65+ years"), pmatch = TRUE], 3)
  twnDataHigh <- dimSums(readSource("PopulationTWN", subtype = "high")[, , "Year-end_Population", pmatch = TRUE]
                         [, , c("0-14 years",
                                "15-64 years",
                                "65+ years"), pmatch = TRUE], 3)
  twnDataLow <- dimSums(readSource("PopulationTWN", subtype = "low")[, , "Year-end_Population", pmatch = TRUE]
                        [, , c("0-14 years",
                               "15-64 years",
                               "65+ years"), pmatch = TRUE], 3)
  years <- intersect(getYears(twn), getYears(twnDataMedium))
  twn[, years, "pop_SSP1"] <- twnDataLow[, years, ]
  twn[, years, "pop_SSP2"] <- twnDataMedium[, years, ]
  twn[, years, "pop_SSP3"] <- twnDataHigh[, years, ]
  twn[, years, "pop_SSP4d"] <- twnDataMedium[, years, ]
  twn[, years, "pop_SSP5"] <- twnDataLow[, years, ]

  ####### projecting population until 2150
  grTWN <- new.magpie("TWN", getYears(twn), getNames(twn))
  # calculate growht rate from 2055 to 2060
  grTWN[, 2060, ] <- (twn[, 2060, ] / setYears(twn[, 2055, ], NULL)) - 1
  # calculating grow rates from 2060 to 2150 assuming zero growth in 2200
  for (t in seq(2065, 2100, 5)) {
    grTWN[, t, ] <- setYears(grTWN[, 2060, ], NULL) - (t - 2060) / 5 * setYears(grTWN[, 2060, ], NULL) / 28
  }
  # applying assumed growth rates to population matrices
  for (t in seq(2065, 2100, 5)) {
    twn[, t, ] <- setYears(twn[, t - 5, ], t) * (1 + grTWN[, t, ])
  }
  twn[, 2010, ] <- 23162.123
  x <- mbind(x, twn)      # LONGTERM woher die Daten für 2010??? historische Quelle
  #--------------------------------------------------------------------------------

  # check whether the country list agrees with the list of countries in the madrat library
  # remove unrequired data, add missing data
  x <- toolCountryFill(x, fill = 0)
  x
}
