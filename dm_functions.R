data_prep <- function(dfraw) {
  dfnew<- dfraw[complete.cases(dfraw[ , 'Count1Species']),]
  dfspec <- allspecies(dfnew)
  total_detections <- counts.df(dfspec)
  return(total_detections)
}




