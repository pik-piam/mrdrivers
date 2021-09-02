
#' Convert UN Population Divison Data
#' 
#' Converts data from \code{readUN_PopDiv()} to ISO country level. "Other, 
#' non-specified areas" is used as a stand-in for Taiwan, Province of China.
#' Countries missing in the data set are set to zero.
#'
#' @param x \code{magclass} object containing UN Population Division data.
#' 
#' @return \code{magclass} object; population in millions.
convertUN_PopDiv <- function(x) {
  
  target_iso3c <- suppressMessages(
    readr::read_csv2(system.file('extdata', 'iso_country.csv', package = 'madrat'), 
                     col_names = c('country', 'iso3c'), 
                     col_types = 'cc', 
                     skip = 1)
  ) %>% 
    getElement('iso3c')
  
  x_have <- x %>%
    as.data.frame() %>%
    # convert years to integers
    dplyr::mutate(year = as.integer(as.character(.data$Year))) %>% 
    dplyr::select(.data$year, .data$Value, .data$Region) %>%
    # add iso3c country codes
    quitte::add_countrycode_(c('Region' = 'un'), 'iso3c', warn = FALSE) %>% 
    # use "other, non-specified areas" as proxy for "Taiwan, Province of China"
    dplyr::mutate(iso3c = ifelse(.data$Region == '158', 'TWN', .data$iso3c)) %>% 
    # drop entries from non-countries
    dplyr::filter(!is.na(.data$iso3c)) %>% 
    # drop m49/Region column
    dplyr::select(dplyr::matches('[^(Region)]'))
  
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
    as.magpie() %>% 
    return()
}
