#' calcGDPpppPast
#' 
#' Calculates a time series of GDP in Purchase Power Parity (PPP) of million
#' International Dollars of the year 2005.  The source is selected in the
#' config file (getConfig()$calc$GDPpppPast). Different sources are available:
#' \itemize{ \item \code{WDI}: The PPP estimate from the World Development
#' Indicators (WDI) are supplemented by values for Argentina, Syria and Somalia
#' which are missing in the database. The values were taken from World Bank.
#' 2014. Purchasing Power Parities and the Real Size of World Economies: A
#' Comprehensive Report of the 2011 International Comparison Program. The World
#' Bank. http://elibrary.worldbank.org/doi/book/10.1596/978-1-4648-0329-1.
#' table 2.13 Then, the 2011 estimate is extrapolated with the GDP projection
#' in local currency units (LCU), as these growth rates should be approximately
#' what the growth models predict. The price index from 2011 was transformed
#' into 2005 equivalents using the inflation rate of the United States (US), as
#' the PPPs are in USDollar equvialents. 
#' \item \code{PWT}: Penn World Tables
#' \item \code{IHME_USD05_PPP_pc}: Publication: James, Spencer L., Paul Gubbins,
#' Christopher JL Murray, and Emmanuela Gakidou. 2012. "Developing a
#' Comprehensive Time Series of GDP per Capita for 210 Countries from 1950 to
#' 2015."" Population Health Metrics 10 (1): 12. doi:10.1186/1478-7954-10-12.
#' This publication also contains further interpolated indicators that can be
#' used. }
#' 
#' @param GDPpppPast GDPppp past data source
#' @return GDP PPP in million USD05 equivalents
#' @author Lavinia Baumstark, Benjamin Bodirsky, Johannes Koch

calcGDPpppPast <- function(GDPpppPast = "WDI_completed") {

  # Check input argument
  valid_inputs <- c(
    "WDI", 
    "WDI_completed",
    "PWT", 
    "Eurostat_WDI",
    "Eurostat_WDI_completed",
    "IHME_USD05_PPP_pc_completed",
    "IHME_USD05_MER_pc_completed",
    "IHME_USD05_PPP_pc",
    "IHME_USD05_MER_pc",
    "IMF_USD05_PPP_pc",
    "PENN_USD05_PPP_pc",
    "WB_USD05_PPP_pc",
    "MADDISON_USD05_PPP_pc",
    "WB_USD05_MER_pc",
    "IMF_USD05_MER_pc",
    "UN_USD05_MER_pc"
  )
  if (!GDPpppPast %in% valid_inputs) {
    stop("Bad input for calcGDPpppPast. Invalid 'GDPpppPast' argument.")
  }

  # Look for "_completed" tag. Remove if found.
  if (grepl("_completed$", GDPpppPast)) {
    GDPpppPast <- gsub("_completed", "", GDPpppPast)
    complete <- TRUE
  } else {
    complete <- FALSE
  }
  
  # Call appropriate calcGDPpppPast function. 
  data <- switch(GDPpppPast,
                 "PWT" = calcGDPpppPastPWT(),
                 "WDI" = calcGDPpppPastWDI(complete),
                 "Eurostat_WDI" = calcGDPpppPastEurostatWDI(complete),
                 calcGDPpppPastJames(GDPpppPast, complete))

  return(list(x = data,
              weight = NULL,
              unit = "Million US Dollar 2005 equivalents in PPP",
              description = "GDP in PPP with baseyear in 2005. PPP may come either from ICP 2005 or 2011."))
}








######################################################################################
# Functions
######################################################################################
calcGDPpppPastPWT <- function() {
  data <- readSource("PWT")[,,"rgdpna"]
  getNames(data) <- "GDPppp_PWT"
  data
}

calcGDPpppPastWDI <- function(complete) {
  # "NY.GDP.MKTP.PP.KD" = GDP in constant 2017 Int$PPP (as of time of writing this function)
  data <- readSource("WDI", "NY.GDP.MKTP.PP.KD") %>% 
    GDPuc::convertGDP("constant 2017 Int$PPP", "constant 2005 Int$PPP")
  # TODO: Decide if use JAMES 2019 data for NA countries, e.g. DJI (as is now) or 
  # convert using regional averages and use JAMES 2019 growth rates
  data[is.na(data)] <- 0 
  
  # There is no PPP data available before 1990, so we shall extrapolate back using constant LCU growth rates 
  # data <- harmonizeFutureGrPast(past = readSource("WDI", "NY.GDP.MKTP.KN"),
  #                               future = data)
  
  # Use the James2019  WB_USD05_PPP_pc series to fill in past data.
  # Using growth rates, since conversion of James2019 data into 2005 Int$PPP not certain to be correct.
  gdppc <- readSource("James2019", "WB_USD05_PPP_pc")
  pop <- readSource("WDI", "SP.POP.TOTL")
  cy <- intersect(getYears(gdppc), getYears(pop))
  past_gdp <- gdppc[, cy,] * pop[, cy,]
  getSets(past_gdp) <- c("iso3c", "year", "data")
  getNames(past_gdp) <- "WB_USD05_PPP"
  
  x <- new.magpie(getRegions(data), getYears(past_gdp), getNames(data), fill = 0)
  for (i in getRegions(data)) {
    tmp <- data[i, getYears(data)[data[i,,] != 0], ]
    ihme_data <- past_gdp[i, getYears(past_gdp)[past_gdp[i,,] != 0], ]
    
    if (length(tmp) == 0 && length(ihme_data) == 0) {
      next
    } else if (length(tmp) != 0 && length(ihme_data) == 0) {
      x[i, getYears(tmp),] <- tmp
    } else if (length(tmp) == 0 && length(ihme_data) != 0) {
      x[i, getYears(past_gdp),] <- toolFillYears(ihme_data, getYears(past_gdp))
    } else {
      r <- harmonizeFutureGrPast(past = toolFillYears(ihme_data, getYears(past_gdp)), 
                                 future = tmp)
      x[i, getYears(r),] <- r
    }
  }
  data <- x

  if (complete) {  
    fill <- readSource("MissingIslands", subtype = "gdp", convert = FALSE)
    data <- completeData(data, fill)
  }
  
  getNames(data) <- "gdp in constant 2005 Int$PPP" 
  data
}

calcGDPpppPastJames <- function(type, complete) {
  PPP_pc <- readSource(type = "James", subtype = type)
  pop <- readSource("WDI", subtype = "SP.POP.TOTL")
  years <- intersect(getYears(PPP_pc), getYears(pop))
  data <- PPP_pc[,years,] * pop[,years,]
  getNames(data) <- substr(type, 1, (nchar(type) - 3))
  
  if (complete) {  
    fill <- readSource("MissingIslands", subtype = "gdp", convert = FALSE)
    data <- completeData(data, fill)
    getNames(data) <- "gdp" #??
  }
  
  data
}

calcGDPpppPastEurostatWDI <- function(complete) {
  data_eurostat <- readSource("Eurostat", "GDP")
  data_wdi <- readSource("WDI", "NY.GDP.MKTP.PP.KD") %>% 
    GDPuc::convertGDP("constant 2017 Int$PPP", "constant 2005 Int$PPP")
  data_wdi[is.na(data_wdi)] <- 0

  # Get EUR countries. 
  EUR_countries <- toolGetMapping("regionmappingH12.csv") %>% 
    tibble::as_tibble() %>% 
    dplyr::filter(.data$RegionCode == "EUR") %>% 
    dplyr::pull(.data$CountryCode)
  
  # Use WDI data for everything but the EUR_countries. Use Eurostat stat for those.
  data <- data_wdi
  cy <- intersect(getYears(data_wdi),  getYears(data_eurostat))
  data[EUR_countries, cy,] <- data_eurostat[EUR_countries, cy,]

  # There is no PPP data available before 1990, so we shall extrapolate back using constant LCU growth rates 
  data <- harmonizeFutureGrPast(past = readSource("WDI", "NY.GDP.MKTP.KN"),
                                future = data)

  if (complete) {
    fill <- readSource("MissingIslands", subtype = "gdp", convert = FALSE)
    data <- completeData(data, fill)
  }

  getNames(data) <- "gdp in constant 2005 Int$PPP" 
  data
}
