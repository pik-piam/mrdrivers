toolHarmonizeWithPEAPandFuture <- function(past, future) {
  # Get PEAP data (and fill with UN_PopDiv) and drop everything that is not "short-term", defined as being later than
  # the last year of the IMF WEO data (the UN_PopDiv values need to be extrapolated until the last year of the IMF
  # data. Constant extrapolation assumed.)
  shortTerm <- readSource("PEAP") %>%
    toolFillWith(readSource("UN_PopDiv", "pop", "medium")) %>%
    toolInterpolateAndExtrapolate()
  lastYearIMF <- max(getYears(readSource("IMF", "GDPpc"), as.integer = TRUE))
  shortTerm <- shortTerm[, getYears(shortTerm, as.integer = TRUE) <= lastYearIMF, ]

  # Use PEAP growth rates until last year of IMF WEO data, and future growth rates after that
  x <- past$x %>%
    toolHarmonizePast(shortTerm, method = "growth") %>%
    toolHarmonizePast(future$x, method = "growth") %>%
    # For any countries with missing projections, extrapolate (constant values assumed into the future).
    toolInterpolateAndExtrapolate()

  lastPastYear <- max(getYears(past$x, as.integer = TRUE))
  list(x = x,
       description = glue("use {past$description} until {lastPastYear}, \\
                          growth rates from the Wolrld Bank's PEAP until {lastYearIMF}, \\
                          and growth rates from {future$description} thereafter."))
}

toolHarmonizeLabourADBs <- function() {
  pop2 <- calcOutput("Population", scenario = "SSP2", naming = "scenario", extension2150 = "none", aggregate = FALSE)
  lab2 <- calcOutput("Labour", scenario = "SSP2", naming = "scenario", extension2150 = "none", aggregate = FALSE)
  pop  <- calcOutput("Population", scenario = "ADBs", extension2150 = "none", aggregate = FALSE)

  combined <- purrr::map(getNames(pop), function(x) {
    labShareSSP2 <- lab2 / pop2[, getYears(lab2), ]
    getNames(labShareSSP2) <- x
    pop[, getYears(lab2), x] * labShareSSP2
  }) %>%
    mbind()

  list(x = combined, description = glue("labour to population ratio equal to that in SSP2."))
}
