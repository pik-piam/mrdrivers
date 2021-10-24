#' calcLabour
#' 
#' Merges time series of population for the past and present. See
#' \code{\link{calcPopulationPast}} for past datasets, and
#' \code{\link{calcPopulationFuture}} for future datasets. The time series are
#' merged via the growth rates. The first year of the future scenarios
#' determines the merging point. All data is calibrated either to the "past" or
#' the "future" dataset as specified by PopulationCalib.
#' 
#' @param LabourFuture Labour future data source
#' @inheritParams calcGDP
#' @inherit calcGDP return
#' 
#' @seealso [madrat::calcOutput()]
#' @family Population functions
#' 
#' @examples \dontrun{
#' library(mrdrivers)
#' calcLabour()}
#' 
calcLabour <- function(LabourFuture = c("SSPs", "SDPs", "SSP2EU"), 
                       extension2150 = "bezier") {
  # Call internal_calcLabour function the appropriate number of times              
  toolInternalCalc("Labour", as.list(environment()))
}

######################################################################################
# Internal Function
######################################################################################
internal_calcLabour <- function(LabourFuture, extension2150) {
  x <- switch(
    LabourFuture,
    "SSPs"    = cLabourFutureSSPs(),
    "SDPs"    = cLabourFutureSDPs(),
    "SSP2EU"  = cLabourFutureSSP2EU(),
    "SSPsOld" = cLabourFutureSSPsOld(),
    stop("Bad input for calcLabour. Invalid 'LabourFuture' argument.")
  )

  # Apply finishing touches to combined time-series
  x <- toolFinishingTouches(x, extension2150)
  
  list(x = x,
       weight = NULL,
       unit = "million",
       description = glue("Working age population data."))

}


######################################################################################
# Functions
######################################################################################
cLabourFutureSSPs <- function() {
  x <- readSource("SSP", "lab2018Update") * 1e-3
  
  # lab2018Update only starts in 2015. However data is needed back until 2005.
  # Use old Labour growth rates to fill in.
  y <- calcOutput("Labour", LabourFuture = "SSPsOld", extension = "none", aggregate = FALSE)
  y <- y[,1:3,paste0("pop_", getNames(x))]
  getNames(y) <- getNames(x)
  x <- toolHarmonizeFutureGrPast(future = x, past = y)

  getNames(x) <- paste0("lab_", getNames(x))
  x
}

cLabourFutureSDPs <- function() {
  lab_SSP1 <- cLabourFutureSSPs()[,,"lab_SSP1"]

  x <- purrr::map(c("SDP", "SDP_EI", "SDP_RC", "SDP_MC"),
                  ~ setNames(lab_SSP1, gsub("SSP1", .x, getNames(lab_SSP1)))) %>%
    mbind()
}

cLabourFutureSSP2EU <- function() {
  lab_SSP2 <- cLabourFutureSSPs()[,, "lab_SSP2"]
  pop_SSP2 <- calcOutput("Population", 
                         PopulationCalib = "calibSSPs",
                         PopulationPast = "WDI", 
                         PopulationFuture = "SSPs",
                         FiveYearSteps = FALSE,
                         extension2150 = "none",
                         aggregate = FALSE)[,, "pop_SSP2"]
  pop_SSP2EU <- calcOutput("Population", 
                           PopulationCalib = "calibSSP2EU",
                           PopulationPast = "Eurostat-WDI", 
                           PopulationFuture = "SSP2EU",
                           FiveYearSteps = FALSE,
                           extension2150 = "none",
                           aggregate = FALSE)[,, "pop_SSP2EU"]

  y <- getYears(lab_SSP2)
  getNames(pop_SSP2) <- getNames(pop_SSP2EU) <- getNames(lab_SSP2) <- "lab_SSP2EU"
  lab_SSP2EU <- lab_SSP2 / pop_SSP2[, y, ] * pop_SSP2EU[, y, ]
  lab_SSP2EU[is.na(lab_SSP2EU)] <- 0
  lab_SSP2EU
}


#Legacy
cLabourFutureSSPsOld <- function() {
  aged <- purrr::cross3("Population", 
                        c("Male", "Female"), 
                        c("Aged15-19", "Aged20-24", "Aged25-29", "Aged30-34", "Aged35-39", 
                          "Aged40-44", "Aged45-49", "Aged50-54", "Aged55-59", "Aged60-64")) %>% 
    purrr::map_chr(paste, collapse = "|")
 
  readSource("SSP",subtype="all")[,,aged]
  
  data <- collapseNames(readSource("SSP",subtype="all")[,,aged][,,"IIASA-WiC POP"])
  data <- dimSums(data,dim=3.2)
  
  getNames(data) <- paste("pop_",gsub("_v[[:alnum:],[:punct:]]*","",getNames(data)),sep="")
  # change name of "SSP4d" to "SSP4
  getNames(data)<-sub("SSP4d","SSP4",getNames(data))
  
  time_extend <- c("y2105","y2110","y2115","y2120","y2125","y2130","y2135","y2140","y2145","y2150")
  data <- time_interpolate(data,time_extend,extrapolation_type="constant",integrate_interpolated_years=TRUE)
  
  # delete 0s
  data <- data[,c("y2000","y2005"),,invert=TRUE]
  # extrapolate data for 2005
  data <- time_interpolate(data,c("y2005"),extrapolation_type="linear",integrate_interpolated_years=TRUE)
  
  # add SDP, SDP_EI, SDP_MC, SDP_RC scenario as copy of SSP1, might be substituted by real data later
  if("pop_SSP1" %in% getNames(data,dim=1)){
     if(!("pop_SDP" %in% getNames(data,dim=1))){
      data_SDP <- data[,,"pop_SSP1"]
      getNames(data_SDP) <- gsub("pop_SSP1","pop_SDP",getNames(data_SDP))
      data <- mbind(data,data_SDP)
    }
     if(!("pop_SDP_EI" %in% getNames(data,dim=1))){
      data_SDP_EI <- data[,,"pop_SSP1"]
      getNames(data_SDP_EI) <- gsub("pop_SSP1","pop_SDP_EI",getNames(data_SDP_EI))
      data <- mbind(data,data_SDP_EI)
    }
     if(!("pop_SDP_MC" %in% getNames(data,dim=1))){
      data_SDP_MC <- data[,,"pop_SSP1"]
      getNames(data_SDP_MC) <- gsub("pop_SSP1","pop_SDP_MC",getNames(data_SDP_MC))
      data <- mbind(data,data_SDP_MC)
    }
     if(!("pop_SDP_RC" %in% getNames(data,dim=1))){
      data_SDP_RC <- data[,,"pop_SSP1"]
      getNames(data_SDP_RC) <- gsub("pop_SSP1","pop_SDP_RC",getNames(data_SDP_RC))
      data <- mbind(data,data_SDP_RC)
    }
  }
  # add SSP2EU scenario as copy of SSP2, might be substituted by real data later
  if("pop_SSP2" %in% getNames(data,dim=1)){
     if(!("pop_SSP2EU" %in% getNames(data,dim=1))){
      data_SSP2EU <- data[,,"pop_SSP1"]
      getNames(data_SSP2EU) <- gsub("pop_SSP1","pop_SSP2EU",getNames(data_SSP2EU))
      data <- mbind(data,data_SSP2EU)
    }
  }
  
  data
}
