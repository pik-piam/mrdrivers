#' Read Eurostat historical emissions 
#' 
#' Read-in Eurostat historical emissions csv files as magclass object
#' 
#' @param subtype emissions for original eurostat emissions split, MACCemi for MACC historical emissions, or
#' sectorEmi for sector specific emissions
#' @return magpie object of Eurostat historical emissions (MtCO2) 
#' @seealso \code{\link{readSource}}
#' @examples \dontrun{a <- readSource(type = "Eurostat", subtype = "emissions")}
readEurostat <- function(subtype = "emissions") {

  switch(subtype,
         "emissions" = readEurostatEmissions(),
         "MACCemi" = readEurostatEmissions(),
         "sectorEmi" = readEurostatEmissions(),
         "population" = readEurostatPopulation(),
         "population_projections" = readEurostatPopulationProjections(),
         "GDP" = readEurostatGDP(),
         stop("Bad input for readEurostat. Invalid 'subtype' argument."))
} 




######################################################################################
# Functions
######################################################################################
readEurostatEmissions <- function() {
  #Reading Eurostat historical emissions 
  type <- c("GHG","CO2","CH4","N2O","HFC","PFC","HFC_PFC_NSP","SF6","NF3")
  data <- NULL
  for (t in type){
    df <- utils::read.csv(paste0("eurostat_",t,".csv"))[,-c(3,7)]
    colnames(df) <- c("period", "region", "emi", "sector", "value")
    df[df==":"]<-NA
    df$value <- gsub(",","",df$value)
    df$value <- as.numeric(df$value)/1000 # convert from Thousand tonnes to Mt
    df$emi <- t
    data <- rbind(data,df)
  }
  # mapping reg
  data$region <- toolCountry2isocode(data$region, mapping = c("germany (until 1990 former territory of the frg)"="DEU"))
  x <- as.magpie(data,spatial=2,temporal=1,datacol=5)
}

readEurostatPopulation <- function() {
  readr::read_csv("estat_demo_gind.csv.gz", col_types = "ccccddc") %>% 
  # Remove last column, mostly empty anyways, that causes issue when converting to magpie
  dplyr::select(-"OBS_FLAG") %>% 
  # Filter for "AVG" = average population. 
  dplyr::filter(.data$indic_de == "AVG") %>% 
  # Convert to magpie
  as.magpie(spatial = "geo", temporal = "TIME_PERIOD")
}

readEurostatPopulationProjections <- function() {
  readr::read_csv("estat_proj_19np.csv.gz", col_types = "cccccccddc") %>% 
  # Remove last column, mostly empty anyways, that causes issue when converting to magpie
  dplyr::select(-"OBS_FLAG") %>% 
  # Filter for baseline projection of total population. 
  dplyr::filter(.data$sex == "T", 
                .data$age == "TOTAL",
                .data$projection == "BSL") %>% 
  # Convert to magpie
  as.magpie(spatial = "geo", temporal = "TIME_PERIOD")
}

readEurostatGDP <- function() {
  readr::read_csv("estat_nama_10_gdp.csv.gz", col_types = "cccccddc") %>% 
  # Remove last column, mostly empty anyways, that causes issue when converting to magpie
  dplyr::select(-"OBS_FLAG") %>% 
  # Filter for GDP at market prices (=B1GQ) in Chained-Linked Volumes in 
  # 2005 mil. National Currencies (= CLV05_MNAC)
  dplyr::filter(.data$na_item == "B1GQ", .data$unit == "CLV05_MNAC") %>% 
  # Convert to magpie
  as.magpie(spatial = "geo", temporal = "TIME_PERIOD")
}
