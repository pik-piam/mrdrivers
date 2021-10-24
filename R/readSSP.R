#' Read SSP
#' 
#' Read-in an SSP data csv.zip file as magclass object
#' 
#' @param subtype A string, either "all", "gdp", "pop", "urb", "pop2018Update", 
#'   "lab2018Update" or "ratioPM".
#' @return Magpie object of the SSP data
#' @seealso [madrat::readSource()]
#' @examples \dontrun{
#' readSource("SSP", subtype = "gdp")}
#' 
readSSP <- function(subtype) {
  if (! subtype %in% c("all", "gdp", "pop", "urb", "pop2018Update", "lab2018Update")) {
     stop("Bad input for readSSP. Invalid 'subtype' argument.")
  }
  
  if (subtype %in% c("all", "gdp", "pop", "urb")) {

    x <- readr::read_csv("SspDb_country_data_2013-06-12.csv.zip", 
                         col_types = list(.default = readr::col_character()),
                         progress = FALSE) %>% 
      tidyr::unite("mod.variable", .data$MODEL, .data$SCENARIO, .data$VARIABLE, .data$UNIT, sep = ".") %>% 
      dplyr::rename("iso3c" = .data$REGION) %>% 
      # Drop columns with only NAs
      dplyr::select(tidyselect::vars_select_helpers$where(~ !all(is.na(.x)))) %>% 
      tidyr::pivot_longer(cols = tidyselect::starts_with("2"), names_to = "year") %>% 
      dplyr::mutate(value = as.double(.data$value)) %>% 
      as.magpie(spatial = "iso3c", temporal = "year", tidy = TRUE, filter = FALSE)  

    if (subtype == "gdp") x <- x[,,"GDP|PPP"][,,"OECD Env-Growth"]
    if (subtype == "pop") x <- x[,,"Population"][,,"IIASA-WiC POP"]
    if (subtype == "urb") x <- x[,,"Population|Urban|Share"][,,"NCAR"]

  } else if (subtype %in% c("pop2018Update", "lab2018Update")) {
    
    # Specifying the col_types quickens the read process
    my_col_types <- readr::cols(.default = "d", scenario = "c", vers = "_", sex = "c", agegrp = "c")
    x <- readr::read_csv("Population in 000 by Age and Sex, countries, SSPs 2018vers wide.csv", 
                         col_types = my_col_types,
                         progress = FALSE) %>% 
      tidyr::pivot_longer(5:205, names_to = "iso3c") %>% 
      dplyr::rename("variable" = "scenario") %>%
      as.magpie(spatial = "iso3c", temporal = "year", tidy = TRUE) 
    
  } 
  x
}
