#' Read in the "Missing Islands" dataset
#'
#' @description Read in gdp or population data for minor islands (not included in big inventories) from a custom made
#' data set that gets data from a variety of sources (e.g. CIA World Factbook, Insee, BEA, PRISM, and Woldometers).
#'
#' @param subtype pop for population, or gdp for gdp
#' @inherit madrat::readSource return
#' @seealso [madrat::readSource()] and [madrat::downloadSource()]
#' @examples \dontrun{
#' readSource("MissingIslands", subtype = "pop", convert = FALSE)
#' }
#' @order 2
readMissingIslands <- function(subtype) {
  files <- c(pop = "pop_past_missing.csv", gdp = "gdp_past_missing.csv")
  file <- toolSubtypeSelect(subtype = subtype, files = files)
  x <- utils::read.csv(file, header = TRUE)
  names(x) <- substring(names(x), 1, 5)
  as.magpie(x)
}



#' @rdname readMissingIslands
#' @param x MAgPIE object returned by readMissingIslands
#' @order 3
convertMissingIslands <- function(x) {
  toolGeneralConvert(x, note = FALSE)
}


#' @rdname readMissingIslands
#' @order 1
downloadMissingIslands <- function() {
  urlMissingIslands <- "https://zenodo.org/record/4421504/files/MissingIslands.zip"
  utils::download.file(urlMissingIslands, destfile = "MissingIslands.zip", quiet = TRUE)
  utils::unzip("MissingIslands.zip")

  # Remove files that are not needed
  keep <- c("pop_past_missing.csv",
            "gdp_past_missing.csv",
            "WDI extended documentation.csv",
            "DOWNLOAD.yml")
  file.remove(setdiff(list.files(), keep))
  unlink("MissingIslands.zip")

  # Compose meta data
  list(url           = urlMissingIslands,
       doi           = "10.5281/zenodo.4421504",
       title         = "MissingIslands gdp and population datasets",
       description   = "MissingIslands dataset, mostly used for filling in gaps in other gdp and population datasets",
       unit          = "-",
       author        = c(utils::person("Ewerton", "Arujo"),
                         utils::person("Benjamin Leon", "Bodirsky"),
                         utils::person("Michael S.", "Crawford"),
                         utils::person("Debbora", "Leip"),
                         utils::person("Jan Philipp", "Dietrich", email = "dietrich@pik-potsdam.de")),
       release_date  = "2021-01-06",
       license       = "Creative Commons Attribution-ShareAlike 4.0 International License (CC BY-SA 4.0)",
       comment       = glue("Data collected from various sources (e.g. CIA World Factbook, Insee, BEA, PRISM, \\
                            and Woldometers). These data are to be used for filling in gaps within the \\
                            World Development Indicators (WDI) datasets on population size and GDP \\
                            projections. Please see the file 'WDI extended documentation.csv' for more  \\
                            information on the data's provenance."))
}
