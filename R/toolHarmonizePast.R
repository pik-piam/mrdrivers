toolHarmonizePast <- function(past, future) {
  firstyear <- min(getYears(future, as.integer = TRUE))
  tmp <- dimSums(future / setYears(future[,firstyear,],NULL) * setYears(past[,firstyear,], NULL), 
                 dim = 3.2)
  tmp[is.nan(tmp)] <- 0
  if (firstyear > min(getYears(past, as.integer = TRUE))) {                                                 
    years_past <- getYears(past)[which(getYears(past, as.integer = TRUE) < firstyear)]
    tmp2 <- setNames(past[,years_past,rep(1, ndata(future))], getNames(future))
    combined <- mbind(tmp2, tmp)
  } else {
    combined <- tmp
  }

  combined
}


