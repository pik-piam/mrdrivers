#' calcUrbanPast
#' 
#' Calculates a time series of urban shares
#' 
#' @inheritParams calcUrban
#' @return Urban shares in population
calcUrbanPast <- function(UrbanPast = "WDI") {

  data <- switch(
    UrbanPast,
    "WDI" = readSource("WDI", "SP.URB.TOTL.IN.ZS") / 100,
    stop("Bad input for UrbanPast. Invalid 'UrbanPast' argument.")
  )

  getNames(data) <- "urbanPop"
  data <- finishingTouches(data)

  wp <- calcOutput("PopulationPast", PopulationPast = UrbanPast, useMIData = FALSE, aggregate = FALSE)
  
  data <- data[getRegions(wp), getYears(wp),]

  return(list(x = data,
              weight = wp,
              unit = "per 1",
              description = paste0("Urbanisation data from ", UrbanPast)))
}
