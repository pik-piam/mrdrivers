#' Read SSP
#'
#' Read-in an SSP data csv.zip file as magclass object
#'
#' @param subtype A string, either "all", "gdp", "pop", "urb", "pop2018Update",
#'   "lab2018Update" or "ratioPM".
#' @inherit madrat::readSource return
#' @seealso [madrat::readSource()]
#' @examples \dontrun{
#' readSource("SSP", subtype = "gdp")
#' }
#' @order 1
readSSP <- function(subtype) {
  if (!subtype %in% c("all", "gdp", "pop", "urb", "pop2018Update", "lab2018Update")) {
     stop("Bad input for readSSP. Invalid 'subtype' argument.")
  }

  if (subtype %in% c("all", "gdp", "pop", "urb")) {

    x <- readr::read_csv("SspDb_country_data_2013-06-12.csv.zip",
                         col_types = list(.default = readr::col_character()),
                         progress = FALSE) %>%
      tidyr::unite("mod.variable", c("MODEL", "SCENARIO", "VARIABLE", "UNIT"), sep = ".") %>%
      dplyr::rename("iso3c" = "REGION") %>%
      # Drop columns with only NAs
      dplyr::select(tidyselect::vars_select_helpers$where(~ !all(is.na(.x)))) %>%
      tidyr::pivot_longer(cols = tidyselect::starts_with("2"), names_to = "year") %>%
      dplyr::mutate(value = as.double(.data$value)) %>%
      as.magpie(spatial = "iso3c", temporal = "year", tidy = TRUE, filter = FALSE)

    if (subtype == "gdp") x <- x[, , "GDP|PPP"][, , "OECD Env-Growth"]
    if (subtype == "pop") x <- x[, , "Population"][, , "IIASA-WiC POP"]
    if (subtype == "urb") x <- x[, , "Population|Urban|Share"][, , "NCAR"]

  } else if (subtype %in% c("pop2018Update", "lab2018Update")) {

    # Specifying the col_types quickens the read process
    myColTypes <- readr::cols(.default = "d", scenario = "c", vers = "_", sex = "c", agegrp = "c")
    x <- readr::read_csv("Population in 000 by Age and Sex, countries, SSPs 2018vers wide.csv",
                         col_types = myColTypes,
                         progress = FALSE) %>%
      tidyr::pivot_longer(5:205, names_to = "iso3c") %>%
      dplyr::rename("variable" = "scenario") %>%
      as.magpie(spatial = "iso3c", temporal = "year", tidy = TRUE)

  }
  x
}

#' @rdname readSSP
#' @order 2
#' @param x MAgPIE object returned from readSSP
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
  aged <- tidyr::expand_grid("Population",
                            c("Male", "Female"),
                            c("Aged15-19", "Aged20-24", "Aged25-29", "Aged30-34", "Aged35-39",
                              "Aged40-44", "Aged45-49", "Aged50-54", "Aged55-59", "Aged60-64")) %>%
    purrr::pmap_chr(paste, sep = "|")
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

