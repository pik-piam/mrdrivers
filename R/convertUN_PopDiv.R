
#' Convert UN Population Divison Data
#' 
#' Converts data from \code{readUN_PopDiv()} to ISO country level. "Other, 
#' non-specified areas" is used as a stand-in for Taiwan, Province of China.
#' Countries missing in the data set are set to zero.
#'
#' @param x \code{magclass} object containing UN Population Division data.
#' @inheritParams readUN_PopDiv
#' @family UN_PopDiv functions
#' @return \code{magclass} object; population in millions.
convertUN_PopDiv <- function(x, subtype = "WPP2019_estimates") {
  
  if (grepl("WPP2019", subtype)) {
    getCells(x) <- getCells(x) %>% 
      countrycode::countrycode("un", 
                               "iso3c", 
                               custom_match = c("158" = "TWN", "830" = "GB_CHA"))
    # Add the Channel Islands (GB_CHA) to Great Britain (GBR)
    x["GBR",,] <- x["GBR",,] + x["GB_CHA",,]
    x <- x["GB_CHA",, invert = TRUE]
    x <- toolGeneralConvert(x)

  } else if (subtype == "WPP2015") {
     target_iso3c <- suppressMessages(
       readr::read_csv2(system.file('extdata', 'iso_country.csv', package = 'madrat'), 
                        col_names = c('country', 'iso3c'), 
                        col_types = 'cc', 
                        skip = 1)
     ) %>% 
       getElement('iso3c')
     
     x_have <- x %>%
       as.data.frame() %>%
       dplyr::select(.data$Year, .data$Value, .data$Region) %>%
       # convert years to integers and
       # add iso3c country codes . Use "other, non-specified areas" as proxy for "Taiwan, Province of China"
       dplyr::mutate(year = as.integer(as.character(.data$Year)),
                     iso3c = countrycode::countrycode(.data$Region, "un", "iso3c", warn = FALSE),
                     iso3c = ifelse(.data$Region == '158', 'TWN', .data$iso3c),
                     .keep = "unused") %>%
       # drop entries from non-countries
       dplyr::filter(!is.na(.data$iso3c)) 
     
     missing_iso3c <- setdiff(target_iso3c, unique(x_have$iso3c))
     
     # notify about missing countries
     message('Population data for the following countries is not available and',
             'therefore set to 0:\n',
             paste(countrycode::countrycode(missing_iso3c, 'iso3c', 'country.name'), 
                   collapse = ', '))
     
     # fill missing countries with zeros
     x_missing <- expand.grid(year = unique(x_have$year),
                              iso3c = missing_iso3c,
                              Value = 0)
     
     dplyr::bind_rows(x_have, x_missing) %>%
       dplyr::arrange(.data$year, .data$iso3c) %>%
       # convert from thousands to millions
       dplyr::mutate(Value = .data$Value / 1000) %>%
       # reorder columns because as.magpie() does not give a shit about its 
       # parameters and assignes dimensions based on column position
       dplyr::select(.data$iso3c, .data$year, .data$Value) %>%
       as.magpie()
  } 
}
