#' Convert SSP data
#'
#' Convert SSP data on ISO country level.
#'
#' @param x MAgPIE object returned from readSSP
#' @inheritParams readSSP
#' @inherit readSSP return examples
#' @family SSP functions
convertSSP <- function(x, subtype) {
  if (subtype %in% c("all", "pop", "gdp", "urb")) {
    if (subtype == "all") x <- x %>% addTWNpop() %>% addTWNlab()
    if (subtype == "pop") x <- addTWNpop(x)
    x <- toolGeneralConvert(x)

  } else if (subtype == "pop2018Update") {

    # Sum over sex and agegrp
    x <- dimSums(x, dim = c(3.2, 3.3))
    # Add the Channel Islands (GB_CHA) to Great Britain (GBR)
    x["GBR", , ] <- x["GBR", , ] + x["GB_CHA", , ]
    x <- x["GB_CHA", , invert = TRUE]
    x <- toolGeneralConvert(x)

  } else if (subtype == "lab2018Update") {

    # Sum over sex and agegrp
    agegrps <- getNames(x)[grepl("\\.(15|2|3|4|50|55|60)", getNames(x))]
    x <- dimSums(x[, , agegrps], dim = c(3.2, 3.3))
    # Add the Channel Islands (GB_CHA) to Great Britain (GBR)
    x["GBR", , ] <- x["GBR", , ] + x["GB_CHA", , ]
    x <- x["GB_CHA", , invert = TRUE]
    x <- toolGeneralConvert(x)

  }
  x
}


addTWNpop <- function(x) {
  twnPop <- new.magpie("TWN",
                        getYears(x[, , "Population"][, , "IIASA-WiC POP"]),
                        getNames(x[, , "Population"][, , "IIASA-WiC POP"]))

  # Read in population data
  twnPopMedium <- readSource("PopulationTWN", subtype = "medium", convert = FALSE) %>%
    `[`(, , "Year-end_Population", pmatch = TRUE) %>%
    `[`(, , c("0-14 years", "15-64 years", "65+ years"), pmatch = TRUE) %>%
    dimSums(3)
  twnPopHigh <- readSource("PopulationTWN", subtype = "high", convert = FALSE) %>%
    `[`(, , "Year-end_Population", pmatch = TRUE) %>%
    `[`(, , c("0-14 years", "15-64 years", "65+ years"), pmatch = TRUE) %>%
    dimSums(3)
  twnPopLow <- readSource("PopulationTWN", subtype = "low", convert = FALSE) %>%
    `[`(, , "Year-end_Population", pmatch = TRUE) %>%
    `[`(, , c("0-14 years", "15-64 years", "65+ years"), pmatch = TRUE) %>%
    dimSums(3)

  # Transfer data until 2060
  years <- intersect(getYears(twnPop), getYears(twnPopMedium))
  twnPop[, years, "SSP1",  pmatch = TRUE] <- twnPopLow[, years, ]
  twnPop[, years, "SSP2",  pmatch = TRUE] <- twnPopMedium[, years, ]
  twnPop[, years, "SSP3",  pmatch = TRUE] <- twnPopHigh[, years, ]
  twnPop[, years, "SSP4d", pmatch = TRUE] <- twnPopMedium[, years, ]
  twnPop[, years, "SSP5",  pmatch = TRUE] <- twnPopLow[, years, ]

  ####### projecting population and labour until 2150
  grTWN <- new.magpie("TWN", getYears(twnPop), getNames(twnPop))
  # calculate growht rate from 2055 to 2060
  grTWN[, 2060, ] <- (twnPop[, 2060, ] / setYears(twnPop[, 2055, ], NULL)) - 1
  # calculating grow rates from 2060 to 2150 assuming zero growth in 2200
  for (t in seq(2065, 2100, 5)) {
    grTWN[, t, ] <- setYears(grTWN[, 2060, ], NULL) - (t - 2060) / 5 * setYears(grTWN[, 2060, ], NULL) / 28
  }
  # applying assumed growth rates to population and labour matrices
  for (t in seq(2065, 2100, 5)) {
    twnPop[, t, ] <- setYears(twnPop[, t - 5, ], t) * (1 + grTWN[, t, ])
  }
  twnPop[, 2010, ] <- 23162.123
  x[, , "IIASA-WiC POP"]["TWN", , "Population"] <- twnPop / 1000
  x
}

addTWNlab <- function(x) {
  aged <- purrr::cross3("Population",
                        c("Male", "Female"),
                        c("Aged15-19", "Aged20-24", "Aged25-29", "Aged30-34", "Aged35-39",
                          "Aged40-44", "Aged45-49", "Aged50-54", "Aged55-59", "Aged60-64")) %>%
    purrr::map_chr(paste, collapse = "|")
  twnLab <- new.magpie("TWN",
                        getYears(x[, , aged][, , "IIASA-WiC POP"]),
                        getNames(x[, , aged][, , "IIASA-WiC POP"]))

  # read in labour data
  twnLabMedium <- readSource("PopulationTWN", subtype = "medium", convert = FALSE) %>%
    `[`(, , "Year-end_Population", pmatch = TRUE)  %>%
    `[`(, , "15-64 years", pmatch = TRUE)
  twnLabHigh   <- readSource("PopulationTWN", subtype = "high", convert = FALSE) %>%
    `[`(, , "Year-end_Population", pmatch = TRUE)  %>%
    `[`(, , "15-64 years", pmatch = TRUE)
  twnLabLow    <- readSource("PopulationTWN", subtype = "low", convert = FALSE) %>%
    `[`(, , "Year-end_Population", pmatch = TRUE)  %>%
    `[`(, , "15-64 years", pmatch = TRUE)


  # Transfer data until 2060
  years <- intersect(getYears(twnLab), getYears(twnLabMedium))
  twnLab[, years, "SSP1",  pmatch = TRUE] <- twnLabLow[, years, ] / length(aged)
  twnLab[, years, "SSP2",  pmatch = TRUE] <- twnLabMedium[, years, ] / length(aged)
  twnLab[, years, "SSP3",  pmatch = TRUE] <- twnLabHigh[, years, ] / length(aged)
  twnLab[, years, "SSP4d", pmatch = TRUE] <- twnLabMedium[, years, ] / length(aged)
  twnLab[, years, "SSP5",  pmatch = TRUE] <- twnLabLow[, years, ] / length(aged)

  ####### projecting population and labour until 2150
  grTWN <- new.magpie("TWN", getYears(twnLab), getNames(twnLab))
  # calculate growht rate from 2055 to 2060
  grTWN[, 2060, ] <- (twnLab[, 2060, ] / setYears(twnLab[, 2055, ], NULL)) - 1
  # calculating grow rates from 2060 to 2150 assuming zero growth in 2200
  for (t in seq(2065, 2100, 5)) {
    grTWN[, t, ] <- setYears(grTWN[, 2060, ], NULL) - (t - 2060) / 5 * setYears(grTWN[, 2060, ], NULL) / 28
  }
  # applying assumed growth rates to population and labour matrices
  for (t in seq(2065, 2100, 5)) {
    twnLab[, t, ] <- setYears(twnLab[, t - 5, ], t) * (1 + grTWN[, t, ])
  }
  twnLab[, 2010, ] <- 16774.919195 / length(aged)
  x[, , "IIASA-WiC POP"]["TWN", , aged]         <- twnLab / 1000
  x
}
