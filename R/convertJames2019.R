#' Convert updated James data
#'
#' 
#' @param x MAgPIE object returned by readJames2019
#' @inheritParams readJames2019
#' @inherit readJames2019 return
#' 
#' @family James2019 functions
#' 
convertJames2019 <- function(x, subtype) {
  x <- toolGeneralConvert(x, no_remove_warning = c("CHN_354", "CHN_361", "USSR_FRMR"), note = FALSE)
 
  #fill missing islands not in MissingIslands, using older James
  old <- readSource("James", subtype = subtype)
  missing <- time_interpolate(old[c("ABW","PYF","NCL"),,], interpolated_year = c(1950:2019))
  x[c("ABW","PYF","NCL"),,] <- missing
  
  # use old HKG and MAC shares, and subtract from CHN
  # new james has much higher (double in earliest time steps)
  # historical china values
  pop <- readSource("WDI", subtype = "SP.POP.TOTL")[c("CHN","HKG","MAC"),,]
  # Drop WDI years not in James2019
  pop <- pop[, intersect(getYears(pop), getYears(x)),]
  
  oldyears <- intersect(getYears(pop), getYears(old))
  old <- pop[,oldyears,]*old[c("CHN","HKG","MAC"),oldyears,]
  old <- collapseNames(old)
  shr <- old[c("HKG","MAC"),,]/dimSums(old[c("CHN","HKG","MAC"),,], dim=1)
  
  shr <- time_interpolate(shr, getYears(pop))
  x1 <- x[c("CHN","HKG","MAC"),getYears(pop),]*pop[c("CHN","HKG","MAC"), getYears(pop),] 
  x1[c("HKG","MAC"),,] <- shr*x1["CHN",,]
  x1["CHN",,] <- x1["CHN",,]-dimSums(x1[c("HKG","MAC"),,], dim=1)

  #fill 1950-1959 HKG and MAC with 1960 values, don't subtract from CHINA for these years because no population data to convert to totals, but very small differnece anyways
  x[c("CHN","HKG","MAC"),getYears(x1),] <- x1/pop[c("CHN","HKG","MAC"),,]
  x[c("HKG","MAC"),1950:1959,] <- setYears(x[c("HKG","MAC"),1960,],NULL)
  
  # South Sudan values are very large, likely inaccurate. In their stead, the Missing island gdp values and WDI pop numbers are used.
  ssd_gdp <- readSource("MissingIslands", "gdp")["SSD",,]
  ssd_pop <- readSource("WDI", subtype = "SP.POP.TOTL")["SSD",,] %>% dimReduce()
  
  ssd_gdp <- time_interpolate(ssd_gdp, c(1950:2019)) 
  ssd_pop <- time_interpolate(ssd_pop, c(1950:2019)) 
  
  ssd_gdppc <- ssd_gdp/ssd_pop
  
  x["SSD",,] <- ssd_gdppc

  x
}  
