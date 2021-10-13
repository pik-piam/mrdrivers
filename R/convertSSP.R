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
    if (subtype == "all") x <- x %>% add_TWN_pop() %>% add_TWN_lab()
    if (subtype == "pop") x <- add_TWN_pop(x)
    x <- toolGeneralConvert(x)

  } else if (subtype == "pop2018Update") {

    # Sum over sex and agegrp
    x <- dimSums(x, dim = c(3.2, 3.3))
    # Add the Channel Islands (GB_CHA) to Great Britain (GBR)
    x["GBR",,] <- x["GBR",,] + x["GB_CHA",,]
    x <- x["GB_CHA",, invert = TRUE]
    x <- toolGeneralConvert(x)

  } else if (subtype == "lab2018Update") {

    # Sum over sex and agegrp
    agegrps <- getNames(x)[grepl("\\.(15|2|3|4|50|55|60)", getNames(x))]
    x <- dimSums(x[,, agegrps], dim = c(3.2, 3.3))
    # Add the Channel Islands (GB_CHA) to Great Britain (GBR)
    x["GBR",,] <- x["GBR",,] + x["GB_CHA",,]
    x <- x["GB_CHA",, invert = TRUE]
    x <- toolGeneralConvert(x)

  } else if (subtype == "ratioPM") {

    x <- toolGeneralConvert(x, countryFillWith = 1)
  }  

  x
}  


add_TWN_pop <- function(x) {
  TWN_pop <- new.magpie("TWN", 
                        getYears(x[,,"Population"][,,"IIASA-WiC POP"]), 
                        getNames(x[,,"Population"][,,"IIASA-WiC POP"]))
  
  # Read in population data
  twn_pop_medium <- readSource("PopulationTWN", subtype = "medium", convert = FALSE) %>% 
    `[`(,, "Year-end_Population", pmatch = TRUE) %>% 
    `[`(,, c("0-14 years", "15-64 years", "65+ years"), pmatch = TRUE) %>% 
    dimSums(3)
  twn_pop_high <- readSource("PopulationTWN", subtype = "high", convert = FALSE) %>% 
    `[`(,, "Year-end_Population", pmatch = TRUE) %>% 
    `[`(,, c("0-14 years", "15-64 years", "65+ years"), pmatch = TRUE) %>% 
    dimSums(3)
  twn_pop_low <- readSource("PopulationTWN", subtype = "low", convert = FALSE) %>% 
    `[`(,, "Year-end_Population", pmatch = TRUE) %>% 
    `[`(,, c("0-14 years", "15-64 years", "65+ years"), pmatch = TRUE) %>% 
    dimSums(3)

  # Transfer data until 2060      
  years <- intersect(getYears(TWN_pop), getYears(twn_pop_medium))
  TWN_pop[, years, "SSP1",  pmatch = TRUE] <- twn_pop_low[,years,]
  TWN_pop[, years, "SSP2",  pmatch = TRUE] <- twn_pop_medium[,years,]
  TWN_pop[, years, "SSP3",  pmatch = TRUE] <- twn_pop_high[,years,]
  TWN_pop[, years, "SSP4d", pmatch = TRUE] <- twn_pop_medium[,years,]
  TWN_pop[, years, "SSP5",  pmatch = TRUE] <- twn_pop_low[,years,]

  ####### projecting population and labour until 2150
  gr_TWN <- new.magpie("TWN", getYears(TWN_pop), getNames(TWN_pop))
  # calculate growht rate from 2055 to 2060
  gr_TWN[,2060,] <- (TWN_pop[,2060,] / setYears(TWN_pop[,2055,],NULL)) - 1
  # calculating grow rates from 2060 to 2150 assuming zero growth in 2200
  for (t in seq(2065,2100,5)) {
    gr_TWN[,t,] <- setYears(gr_TWN[,2060,],NULL) - (t-2060)/5 * setYears(gr_TWN[,2060,],NULL)/28    
  }  
  # applying assumed growth rates to population and labour matrices
  for (t in seq(2065,2100,5)) {
    TWN_pop[,t,] <- setYears( TWN_pop[,t-5,],t) * (1 + gr_TWN[,t,]) 
  }
  TWN_pop[,2010,] <- 23162.123
  x[,,"IIASA-WiC POP"]["TWN",,"Population"] <- TWN_pop / 1000 
  x
} 

add_TWN_lab <- function(x) {
  aged <- purrr::cross3("Population", 
                        c("Male", "Female"), 
                        c("Aged15-19", "Aged20-24", "Aged25-29", "Aged30-34", "Aged35-39", 
                          "Aged40-44", "Aged45-49", "Aged50-54", "Aged55-59", "Aged60-64")) %>% 
    purrr::map_chr(paste, collapse = "|")
  TWN_lab <- new.magpie("TWN", 
                        getYears(x[,, aged][,, "IIASA-WiC POP"]), 
                        getNames(x[,, aged][,, "IIASA-WiC POP"]))
  
  # read in labour data
  twn_lab_medium <- readSource("PopulationTWN", subtype = "medium", convert = FALSE) %>% 
    `[`(,, "Year-end_Population" , pmatch = TRUE)  %>% 
    `[`(,, "15-64 years", pmatch = TRUE)
  twn_lab_high   <- readSource("PopulationTWN", subtype = "high", convert = FALSE)%>% 
    `[`(,, "Year-end_Population" , pmatch = TRUE)  %>% 
    `[`(,, "15-64 years", pmatch = TRUE)
  twn_lab_low    <- readSource("PopulationTWN", subtype = "low", convert = FALSE)%>% 
    `[`(,, "Year-end_Population" , pmatch = TRUE)  %>% 
    `[`(,, "15-64 years", pmatch = TRUE)
    

  # Transfer data until 2060      
  years <- intersect(getYears(TWN_lab), getYears(twn_lab_medium))
  TWN_lab[, years, "SSP1",  pmatch = TRUE] <- twn_lab_low[,years,] / length(aged)
  TWN_lab[, years, "SSP2",  pmatch = TRUE] <- twn_lab_medium[,years,] / length(aged)
  TWN_lab[, years, "SSP3",  pmatch = TRUE] <- twn_lab_high[,years,] / length(aged)
  TWN_lab[, years, "SSP4d", pmatch = TRUE] <- twn_lab_medium[,years,] / length(aged)
  TWN_lab[, years, "SSP5",  pmatch = TRUE] <- twn_lab_low[,years,] / length(aged)

  ####### projecting population and labour until 2150
  gr_TWN <- new.magpie("TWN", getYears(TWN_lab), getNames(TWN_lab))
  # calculate growht rate from 2055 to 2060
  gr_TWN[,2060,] <- (TWN_lab[,2060,] / setYears(TWN_lab[,2055,],NULL)) - 1
  # calculating grow rates from 2060 to 2150 assuming zero growth in 2200
  for (t in seq(2065,2100,5)) {
    gr_TWN[,t,] <- setYears(gr_TWN[,2060,],NULL) - (t-2060)/5 * setYears(gr_TWN[,2060,],NULL)/28    
  }  
  # applying assumed growth rates to population and labour matrices
  for (t in seq(2065,2100,5)) {
    TWN_lab[,t,] <- setYears( TWN_lab[,t-5,],t) * (1 + gr_TWN[,t,]) 
  }
  TWN_lab[,2010,] <- 16774.919195/length(aged)
  x[,,"IIASA-WiC POP"]["TWN",,aged]         <- TWN_lab / 1000  
  x
} 
