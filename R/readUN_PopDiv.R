
#' Read UN Population Division Data
#'
#' Read past UN population data. Covers 1950 to 2015 yearly and per M.49 area.
#' See \emph{United Nations, Department of Economicand Social Affairs,
#' Population Division} "World Population Prospects: The 2015 Revision"
#' (\href{https://esa.un.org/unpd/wpp/}{website}).
#'
#' @return \code{magclass} object; population in thousands.
#' @seealso [madrat::readSource()]
#' @family "Past" population functions
#' @family UN_PopDiv functions
readUN_PopDiv <- function() {

  d <- readxl::read_excel('WPP2015_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.XLS',
                          sheet = 'ESTIMATES',
                          skip = 16) %>%
    dplyr::select('Country code', dplyr::matches('^[0-9]{4}$')) %>%
    tidyr::gather('year', 'value', dplyr::matches('^[0-9]{4}$'), na.rm = TRUE, convert = TRUE)

  if( !is.integer(d$year) ) {
    stop("Year is not an integer.")
  }

  return(as.magpie(d, spatial = 'Country code', temporal = 'year'))
}
