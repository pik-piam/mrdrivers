#' Read SSP
#' 
#' Read-in an SSP data csv.zip file as magclass object
#' 
#' 
#' @param subtype data subtype. Either "all" or "ratioPM"
#' @return magpie object of the SSP data
#' @seealso \code{\link{readSource}}
#' @examples
#' 
#' \dontrun{ a <- readSource(type="SSP",subtype="all")
#' }
readSSP<- function(subtype) {
  files <- c(all = "SspDb_country_data_2013-06-12.csv.zip",
             pop2018Update = "Population in 000 by Age and Sex, countries, SSPs 2018vers wide.csv",
             ratioPM = "OECD-WB PPP-MER2005_conversion_rates.xlsx")
  
  file <- toolSubtypeSelect(subtype, files)
  
  if(subtype == "all") {

    x <- readr::read_csv(file, col_types = list(.default = readr::col_character())) %>% 
      tidyr::unite("mod.scen", .data$MODEL, .data$SCENARIO, .data$VARIABLE, .data$UNIT, sep = ".") %>% 
      dplyr::rename("iso3c" = .data$REGION) %>% 
      # Drop columns with only NAs
      dplyr::select(tidyselect::vars_select_helpers$where(~ !all(is.na(.x)))) %>% 
      tidyr::pivot_longer(cols = tidyselect::starts_with("2"), names_to = "year") %>% 
      dplyr::mutate(value = as.double(.data$value)) %>% 
      as.magpie(spatial = "iso3c", temporal = "year", tidy = TRUE, filter = FALSE)  

  } else if(subtype == "pop2018Update") {

    x <- readr::read_csv(file, col_types = list(.default = readr::col_double(), 
                                                scenario = readr::col_character(),
                                                vers = readr::col_character(),
                                                sex = readr::col_character(),
                                                agegrp = readr::col_character())) %>% 
      tidyr::pivot_longer(6:206, names_to = "iso3c") %>% 
      as.magpie(spatial = "iso3c", temporal = "year", tidy = TRUE) 
    
  } else if(subtype == "ratioPM") {

    data <- as.data.frame(readxl::read_excel(file))
    colnames(data) <- c("Region", "value") 
    x <- as.magpie(data)
    
  }

  return(x)
}
