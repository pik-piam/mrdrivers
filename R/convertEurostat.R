#' Converts Eurostat historical emissions 
#' 
#' @param x MAgPIE object to be converted
#' @param subtype emissions for original eurostat emissions split, MACCemi for MACC historical emissions, or
#' sectorEmi for sector specific emissions
#' @return A MAgPIE object containing the Eurostat historical emissions (MtCO2) 
#' @examples \dontrun{a <- convertEurostat(x, subtype = "emissions")}
convertEurostat <- function(x, subtype) {

  switch(
    subtype,
    "emissions"              = toolCountryFill(x, fill = 0, verbosity = 0),
    "sectorEmi"              = convertEurostatSectorEmi(x),
    "MACCemi"                = convertEurostatMACCemi(x),
    "population"             = convertEurostatPopulation(x),
    "population_projections" = convertEurostatPopulation(x),
    "GDP"                    = convertEurostatGDP(x),
    stop("Bad input for convertEurostat. Invalid 'subtype' argument.")
  )
}

######################################################################################
# Functions
######################################################################################
convertEurostatSectorEmi <- function(x) {
  x  <- toolCountryFill(x, fill = 0, verbosity = 0)
  # mapping eurostat to sector emissions
  mapping <- list("power"      =list("energy"  =c("Fuel combustion in public electricity and heat production")),
                  "refining"   =list("energy"  =c("Fuel combustion in petroleum refining")),
                  "solids"     =list("energy"  =c("Fuel combustion in manufacture of solid fuels and other energy industries",
                                                  "Other fuel combustion sectors n_e_c_")),
                  "extraction" =list("process" =c("Solid fuels - fugitive emissions",
                                                  "Oil, natural gas and other energy production - fugitive emissions")),
                  "build"      =list("energy"  =c("Fuel combustion in commercial and institutional sector",
                                                  "Fuel combustion by households")),
                  "indst"      =list("energy"  =c("Fuel combustion in manufacturing industries and construction"),
                                     "process" =c("Industrial processes and product use")),
                  "trans"      =list("energy"  =c("Fuel combustion in transport")),
                  "bunkers"    =list("energy"  =c("International bunkers (memo item)")),
                  "agriculture"=list("energy"  =c("Fuel combustion in agriculture, forestry and fishing"),
                                     "process" =c("Agriculture")),
                  "waste"      =list("process" =c("Waste management")),
                  "lulucf"     =list("process" =c("Land use, land use change, and forestry (LULUCF)")),
                  "cdr"        =list("process" =c("Transport and storage of CO2 (memo item)")),
                  "other"      =list("energy"  =c("Multilateral operations (memo item)"),
                                     "process" =c("Other sectors")),
                  "indirect"   =list("process" =c("Indirect CO2")))
    
  x <- mbind(lapply(names(mapping), function(var){
    mbind(lapply(names(mapping[[var]]), function(t){
      add_dimension(
        add_dimension(
          dimSums(x[,,mapping[[var]][[t]]],na.rm=T,dim = 3.2),
          dim = 3.2, add = "sector", nm = var),
        dim = 3.3, add = "type", nm = t)
    }))
  }))
}
 
 

convertEurostatMACCemi <- function(x) {
  x  <- toolCountryFill(x, fill = 0, verbosity = 0)
  # mapping eurostat to MACC emissions
  mapping <- list("ch4coal"=list("emi"="CH4","accounts"=c("Solid fuels - fugitive emissions")),
                  "ch4wstl"=list("emi"="CH4","accounts"=c("Solid waste disposal")),
                  "ch4wsts"=list("emi"="CH4","accounts"=c("Wastewater treatment and discharge")),
                  "ch4rice"=list("emi"="CH4","accounts"=c("Rice cultivation")),
                  "ch4animals"=list("emi"="CH4","accounts"=c("Enteric fermentation")),
                  "ch4anmlwst"=list("emi"="CH4","accounts"=c("Manure management")),
                  "ch4agwaste"=list("emi"="CH4","accounts"=c("Field burning of agricultural residues")),
                  "n2otrans"=list("emi"="N2O","accounts"=c("Fuel combustion in transport")),
                  "n2oadac"=list("emi"="N2O","accounts"=c("Adipic acid production")),
                  "n2onitac"=list("emi"="N2O","accounts"=c("Nitric acid production")),
                  "n2oagwaste"=list("emi"="N2O","accounts"=c("Field burning of agricultural residues")),
                  "n2owaste"=list("emi"="N2O","accounts"=c("Wastewater treatment and discharge")), # or full Waste management n2O?
                  "co2cement_process"=list("emi"="CO2","accounts"=c("Industrial processes and product use")),
                  "co2luc"=list("emi"="CO2","accounts"=c("Land use, land use change, and forestry (LULUCF)")))
  # other MACCs do not have a direct mapping to the eurostat data (use sector information instead)
    # ch4gas + ch4oil -> extraction.process.ch4 - ch4coal
    # ch4forest + ch4savan -> lulucf.process.ch4
    # n2ofertin + n2ofertcr + n2ofertsom + n2oanwstc + n2oanwstm + n2oanwstp -> agriculture.process.n2o - n2oagwaste
    # n2oforest + n2osavan -> lulucf.process.n2o
    # co2cement + co2chemicals + co2steel (=co2cement_process?) -> indst.process.co2
  
  x <- mbind(lapply(names(mapping), function(var){
    setNames(dimSums(x[,,mapping[[var]]$accounts][,,mapping[[var]]$emi],na.rm=T,dim = 3.2), var)
  }))
}


convertEurostatPopulation <- function(x) {
  # Fix names of sets, and of variable
  x <- collapseDim(x, dim = 3)
  getNames(x) <- "population"
  # Use the "DE_TOT" values for Germany, if they exist (DE_TOT = East + West Germany)
  x["DE",,] <- if ("DE_TOT" %in% getRegions(x)) x["DE_TOT",,] else x["DE",,]
  # Drop any countries with more than 2 charachters in their Eurostat identifier. Those are aggregates.
  my_countries <- getRegions(x)[purrr::map_lgl(getRegions(x), ~ nchar(.x) == 2)]
  x <- x[my_countries,,]
  # Convert the eurostat countrycodes to iso3c codes
  getItems(x, 1) <- countrycode::countrycode(getRegions(x), "eurostat", "iso3c")
  # Fix set names
  getSets(x) <- c("iso3c", "year", "value")
  # Filter out any countries that don't have a iso3c code (in this case Kosovo, and Mainland-France)
  x <- x[!is.na(getCells(x)),,]
  # Sort by year
  x <- x[,sort(getYears(x)),]
  # Replace NAs with 0
  x[is.na(x)] <- 0
  # Fill in 0 for all missing countries
  x <- toolCountryFill(x, fill = 0)
 }

convertEurostatGDP <- function(x) {
  # Fix names of sets, and of variable
  x <- collapseDim(x, dim = 3)
  getNames(x) <- "GDP"

  # Drop any countries with more than 2 charachters in their Eurostat identifier. Those are aggregates.
  my_countries <- getRegions(x)[purrr::map_lgl(getRegions(x), ~ nchar(.x) == 2)]
  x <- x[my_countries,,]

  # Convert the eurostat countrycodes to iso3c codes
  getItems(x, 1) <- countrycode::countrycode(getRegions(x), "eurostat", "iso3c")

  # Fix set names
  getSets(x) <- c("iso3c", "year", "value")

  # Filter out any countries that don't have a iso3c code
  x <- x[!is.na(getCells(x)),,]

  # Convert from constant 2005 LCU to constant 2005 Int$PPP
  x <- GDPuc::convertGDP(x, "constant 2005 LCU", "constant 2005 Int$PPP")

  # Sort by year
  x <- x[,sort(getYears(x)),]

  # Replace NAs with 0
  x[is.na(x)] <- 0

  # Fill in 0 for all missing countries
  x <- toolCountryFill(x, fill = 0)
}
