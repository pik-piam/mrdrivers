#' calcGDPpcPast
#' 
#' @inheritParams calcGDPpc
calcGDPpcPast <- function(GDPpcPast = "WDI", 
                          unit = "constant 2005 Int$PPP", 
                          useMIData = TRUE) {
  
  # Call appropriate calcGDPPast function. 
  data <- switch(GDPpcPast,
                 "WDI" = calcGDPpcPastWDI(),
                 stop("Bad input for calcGDPpcPast. Invalid 'GDPpcPast' argument."))

  if (useMIData) {  
    gdp <- readSource("MissingIslands", subtype = "gdp", convert = FALSE)
    pop <- readSource("MissingIslands", subtype = "pop", convert = FALSE)
    countries <- intersect(getRegions(gdp), getRegions(pop))
    fill <- gdp[countries,,] / pop[countries,,]
    data <- completeData(data, fill)
  }

  return(list(x = data,
              weight = NULL,
              unit = unit,
              description = glue("GDPpc data from {GDPpcPast}.")))
}


######################################################################################
# Functions
######################################################################################
calcGDPpcPastWDI <- function() {
  gdp <- calcOutput("GDPPast", GDPPast = "WDI", useMIData = FALSE, aggregate = FALSE)
  pop <- calcOutput("PopulationPast", PopulationPast = "WDI", useMIData = FALSE, aggregate = FALSE) 
  years <- intersect(getYears(gdp), getYears(pop))
  
  data <- gdp[, years,] / pop[, years,]
  data <- setNames(data, "gdppc_WDI")
  data[is.nan(data) | data == Inf] <- 0
  data 
}
