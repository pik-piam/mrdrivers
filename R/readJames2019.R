#' Read James 2019 updated dataset
#' 
#' Read-in GDP per-capita data from the publication James, Spencer L., Paul
#' Gubbins, Christopher JL Murray, and Emmanuela Gakidou. 2012. "Developing a
#' Comprehensive Time Series of GDP per Capita for 210 Countries from 1950 to
#' 2015." Population Health Metrics 10 (1): 12. doi:10.1186/1478-7954-10-12.
#' from a .csv file to a magclass object
#' 
#' 2019 dataset from personal communication w/ B Bodirsky
#' 
#' @param subtype String indicating the data series
#' @return GDP per capita in USD05 in PPP or MER as magpie object
#' 
#' @seealso [madrat::readSource()]
#' @family "Past" GDPpc functions
#' @family James2019 functions
#' 
#' @examples \dontrun{
#' readSource("James2019", subtype = "IHME_USD05_PPP_pc")}
#' 
readJames2019 <- function(subtype) {
  readr::read_csv("james2019.csv", 
                  col_types = c("ISO3" = "c", ".default" = "d"), 
                  progress = FALSE) %>%
    `[`(, c("ISO3", "Year", subtype)) %>%
    as.magpie(spatial = 1, temporal = 2)    
}  
