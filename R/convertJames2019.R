#' Convert updated James data
#'
#' 
#' @param x MAgPIE object containing James data region resolution
#' @param subtype subtype of GDP indicator to be selected
#' @return GDP per capita in USD05 in PPP or MER as magpie object
#' @author David CHen 
#' @seealso \code{\link{readJames}}
#' @examples
#' 
#' \dontrun{ a <- convertSource("James2019","IHME_USD05_PPP_pc")
#' }
convertJames2019 <- function(x, subtype) {
  x <- x[c("USSR_FRMR","CHN_354","CHN_361"),,,invert = TRUE] #Macao and HKG and Former USSR have 0 values in the dataset
  x <- toolCountryFill(x[,,subtype], fill = 0) 
  
  #fill missing islands not in MissingIslands, using older James
  old <- readSource("James", subtype = subtype)
  missing <- time_interpolate(old[c("ABW","PYF","NCL"),,], interpolated_year = c(1950:2019))
  x[c("ABW","PYF","NCL"),,] <- missing
  
  # use old HKG and MAC shares, and subtract from CHN
  # new james has much higher (double in earliest time steps)
  # historical china values
  pop <- readSource("WDI", subtype = "SP.POP.TOTL")[c("CHN","HKG","MAC"),,]
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
  
  #reset set name of year to Year
  getSets(x)[2] <- "Year"
  return(x)
}  
