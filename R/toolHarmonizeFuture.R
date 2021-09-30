toolHarmonizeFuture <- function(past, future) {
  firstyear <- min(getYears(future, as.integer = TRUE))
  tmp <- dimSums(past/setYears(past[,firstyear,],NULL) * setYears(future[,firstyear,],NULL),
                 dim = 3.2)
  tmp[is.nan(tmp)] <- 0
  if (firstyear > min(getYears(past, as.integer = TRUE))) {                                                 
    years_past<-getYears(past)[which(getYears(past, as.integer = TRUE) < firstyear)]
    tmp2 <- setNames(tmp[,years_past,rep(1, ndata(future))], getNames(future))
    combined <- mbind(tmp2, future)
  } else {
    combined <- future
  }
  combined
}
