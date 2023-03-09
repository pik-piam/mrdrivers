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
#' @seealso [madrat::readSource()] and [madrat::downloadSource()]
#' @examples \dontrun{
#' readSource("James2019", subtype = "IHME_USD05_PPP_pc")
#' }
#'
readJames2019 <- function(subtype) {
  readr::read_csv("james2019.csv",
                  col_types = c("ISO3" = "c", ".default" = "d"),
                  progress = FALSE) %>%
    `[`(, c("ISO3", "Year", subtype)) %>%
    as.magpie(spatial = 1, temporal = 2)
}

#' @describeIn readJames2019 convert function
#' @param x MAgPIE object returned by readJames2019
convertJames2019 <- function(x, subtype) {
  x <- toolGeneralConvert(x, no_remove_warning = c("CHN_354", "CHN_361", "USSR_FRMR"), note = FALSE)

  # fill missing islands not in MissingIslands, using older James
  old <- readSource("James", subtype = subtype)
  missing <- time_interpolate(old[c("ABW", "PYF", "NCL"), , ], interpolated_year = c(1950:2019))
  x[c("ABW", "PYF", "NCL"), , ] <- missing

  # use old HKG and MAC shares, and subtract from CHN
  # new james has much higher (double in earliest time steps)
  # historical china values
  pop <- readSource("WDI", subtype = "SP.POP.TOTL")[c("CHN", "HKG", "MAC"), , ]
  # Drop WDI years not in James2019
  pop <- pop[, intersect(getYears(pop), getYears(x)), ]

  oldyears <- intersect(getYears(pop), getYears(old))
  old <- pop[, oldyears, ] * old[c("CHN", "HKG", "MAC"), oldyears, ]
  old <- collapseNames(old)
  shr <- old[c("HKG", "MAC"), , ] / dimSums(old[c("CHN", "HKG", "MAC"), , ], dim = 1)

  shr <- time_interpolate(shr, getYears(pop))
  x1 <- x[c("CHN", "HKG", "MAC"), getYears(pop), ] * pop[c("CHN", "HKG", "MAC"), getYears(pop), ]
  x1[c("HKG", "MAC"), , ] <- shr * x1["CHN", , ]
  x1["CHN", , ] <- x1["CHN", , ] - dimSums(x1[c("HKG", "MAC"), , ], dim = 1)

  # fill 1950-1959 HKG and MAC with 1960 values, don't subtract from CHINA
  # for these years because no population data to convert to totals, but very small differnece anyways
  x[c("CHN", "HKG", "MAC"), getYears(x1), ] <- x1 / pop[c("CHN", "HKG", "MAC"), , ]
  x[c("HKG", "MAC"), 1950:1959, ] <- setYears(x[c("HKG", "MAC"), 1960, ], NULL)

  # South Sudan values are very large, likely inaccurate. In their stead,
  # the Missing island gdp values and WDI pop numbers are used.
  ssdGDP <- readSource("MissingIslands", "gdp")["SSD", , ]
  ssdPop <- readSource("WDI", subtype = "SP.POP.TOTL")["SSD", , ] %>% dimReduce()

  ssdGDP <- time_interpolate(ssdGDP, c(1950:2019))
  ssdPop <- time_interpolate(ssdPop, c(1950:2019))

  ssdGDPpc <- ssdGDP / ssdPop

  x["SSD", , ] <- ssdGDPpc

  x
}
