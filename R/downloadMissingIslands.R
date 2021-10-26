#' @title downloadMissingIslands
#'
#' @description  Download MissingIslands data (on population or GDP) for filling in data gaps from the
#' full datasets.
#'
#' @param subtype pop for population or gdp for GDP
#'
#' @seealso  [downloadSource()]
#' @family MissingIslands functions
#' @examples \dontrun{
#' a <- downloadSource("MissingIslands", subtype = "pop")
#' }
#'
downloadMissingIslands <- function(subtype) {

  # Define subtype-specific elements of the meta data. Elements that are common to all subtypes are added further down.
  settings <- list(pop = list(title = "MissingIslands dataset for filling in data gaps from the population datasets"),
                   gdp = list(title = "MissingIslands dataset for filling in data gaps from the GDP datasets"))

  meta <- toolSubtypeSelect(subtype, settings)

  urlMissingIslands <- "https://zenodo.org/record/4421504/files/MissingIslands.zip"

  # Downloading and unzipping
  utils::download.file(urlMissingIslands, destfile = "MissingIslands.zip")
  utils::unzip("MissingIslands.zip")

  # remove files that are not needed
  keep <- c(paste0(subtype, "_past_missing.csv"),
            "WDI extended documentation.csv",
            "DOWNLOAD.yml")
  file.remove(setdiff(list.files(), keep))
  unlink("MissingIslands.zip")


  # Compose meta data
  return(list(url           = urlMissingIslands,
              doi           = "10.5281/zenodo.4421504",
              title         = meta$title,
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
  )
}
