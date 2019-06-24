data_prep <- function(dfraw) {
  dfnew<- dfraw[complete.cases(dfraw[ , 'Count1Species']),]
  dfspec <- allspecies(dfnew)
  total_detections <- counts.df(dfspec)
  return(total_detections)
}

subdfngrp_spec<- function(cameradf) {
  
  horses <- cameradf[cameradf$horse >0, ]
  cattle <- cameradf[cameradf$cattle >0, ]
  elk <- cameradf[cameradf$elk >0, ]
  deer <- cameradf[cameradf$deer >0, ]
  
  horses <- unite(horses, ImageDate, ImageTime, col = "DateTime", sep = " ", remove = TRUE)
  cattle <- unite(cattle, ImageDate, ImageTime, col = "DateTime", sep = " ", remove = TRUE)
  elk <- unite(elk, ImageDate, ImageTime, col = "DateTime", sep = " ", remove = TRUE)
  deer <- unite(deer, ImageDate, ImageTime, col = "DateTime", sep = " ", remove = TRUE)
  
  horses$DateTime <- mdy_hms(horses$DateTime)
  cattle$DateTime <- mdy_hms(cattle$DateTime)
  elk$DateTime <- mdy_hms(elk$DateTime)
  deer$DateTime <- mdy_hms(deer$DateTime)
  
  horses <- arrange(horses, DateTime)
  cattle <- arrange(cattle, DateTime)
  elk <- arrange(elk, DateTime)
  deer <- arrange(deer, DateTime)
  
  lag_time_diffh <- difftime(horses$DateTime, lag(horses$DateTime, default = horses$DateTime[1]), units = "mins")
  lag_time_diffc <- difftime(cattle$DateTime, lag(cattle$DateTime, default = cattle$DateTime[1]), units = "mins")
  lag_time_diffe <- difftime(elk$DateTime, lag(elk$DateTime, default = elk$DateTime[1]), units = "mins")
  lag_time_diffd <- difftime(deer$DateTime, lag(deer$DateTime, default = deer$DateTime[1]), units = "mins")
  
  horses$group <- cumsum(ifelse(lag_time_diffh>10,1,0))
  cattle$group <- cumsum(ifelse(lag_time_diffc>10,1,0))
  elk$group <- cumsum(ifelse(lag_time_diffe>10,1,0))
  deer$group <- cumsum(ifelse(lag_time_diffd>10,1,0))
  
  horses$group <- horses$group+1
  cattle$group <- cattle$group+1
  elk$group <- elk$group+1
  deer$group <- deer$group+1
  
  
  horses <<- horses
  cattle <<- cattle
  elk <<- elk
  deer <<- deer
}


