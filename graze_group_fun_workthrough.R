BRL18horses <- data.frame(matrix(ncol=0,nrow=0))
BRL18cattle <- data.frame(matrix(ncol=0,nrow=0))
BRL18elk <- data.frame(matrix(ncol=0,nrow=0))
BRL18deer <- data.frame(matrix(ncol=0,nrow=0))

BRL18horses <- BRL18[BRL18$horse >0, ]
BRL18cattle <- BRL18[BRL18$cow >0, ]
BRL18elk <- BRL18[BRL18$elk >0, ]
BRL18deer <- BRL18[BRL18$deer >0, ]

BRL18horses <- unite(BRL18horses, ImageDate, ImageTime, col = "DateTime", sep = " ", remove = TRUE)
BRL18cattle <- unite(BRL18cattle, ImageDate, ImageTime, col = "DateTime", sep = " ", remove = TRUE)
BRL18elk <- unite(BRL18elk, ImageDate, ImageTime, col = "DateTime", sep = " ", remove = TRUE)
BRL18deer <- unite(BRL18deer, ImageDate, ImageTime, col = "DateTime", sep = " ", remove = TRUE)

BRL18horses$DateTime <- mdy_hms(BRL18horses$DateTime)
BRL18cattle$DateTime <- mdy_hms(BRL18cattle$DateTime)
BRL18elk$DateTime <- mdy_hms(BRL18elk$DateTime)
BRL18deer$DateTime <- mdy_hms(BRL18deer$DateTime)

BRL18horses <- arrange(BRL18horses, DateTime)
BRL18cattle <- arrange(BRL18cattle, DateTime)
BRL18elk <- arrange(BRL18elk, DateTime)
BRL18deer <- arrange(BRL18deer, DateTime)

lag_time_diffh <- difftime(BRL18horses$DateTime, lag(BRL18horses$DateTime, default = BRL18horses$DateTime[1]), units = "mins")
lag_time_diffc <- difftime(BRL18cattle$DateTime, lag(BRL18cattle$DateTime, default = BRL18cattle$DateTime[1]), units = "mins")
lag_time_diffe <- difftime(BRL18elk$DateTime, lag(BRL18elk$DateTime, default = BRL18elk$DateTime[1]), units = "mins")
lag_time_diffd <- difftime(BRL18deer$DateTime, lag(BRL18deer$DateTime, default = BRL18deer$DateTime[1]), units = "mins")

BRL18horses$group <- cumsum(ifelse(lag_time_diffh>10,1,0))
BRL18cattle$group <- cumsum(ifelse(lag_time_diffc>10,1,0))
BRL18elk$group <- cumsum(ifelse(lag_time_diffe>10,1,0))
BRL18deer$group <- cumsum(ifelse(lag_time_diffd>10,1,0))

BRL18horses$group <- BRL18horses$group+1
BRL18cattle$group <- BRL18cattle$group+1
BRL18elk$group <- BRL18elk$group+1
BRL18deer$group <- BRL18deer$group+1

cameradf.visits <- data.frame(species = c("horse", "cattle", "elk", "muledeer"), 
                              max = (c(max(grpgrazingtime_h), max(grpgrazingtime_c), max(grpgrazingtime_e), max(grpgrazingtime_d))),
                              mean = (c(mean(grpgrazingtime_h), mean(grpgrazingtime_c), mean(grpgrazingtime_e), mean(grpgrazingtime_d))),
                              median = (c(median(grpgrazingtime_h), median(grpgrazingtime_c), median(grpgrazingtime_e), median(grpgrazingtime_d))))



#SAUCE: https://stackoverflow.com/questions/12925063/numbering-rows-within-groups-in-a-data-frame
#_______________________Horses ___________
  BRL18horses$group_sequence <- 1  
  for (i in 2:(length(BRL18horses$group))) {  
    if (BRL18horses[i,"group"]==BRL18horses[(i-1),"group"]) {  
      BRL18horses[i,"group_sequence"]<-BRL18horses[i-1,"group_sequence"]+1  
    }  
  }  
  first_last_h <- BRL18horses %>% arrange(group_sequence) %>% group_by(group) %>% slice(c(1,n()))
  
  first_last_h<- first_last_h %>%
    group_by(group) %>% mutate(Diff = DateTime - lag(DateTime)) 

  tot_h<- first_last_h[!is.na(first_last_h$Diff),]
  tot_h$Diff[tot_h$Diff <= 100] <- 300
  grpgrazingtime_h <- (as.numeric(tot_h$Diff))
  grpgrazingtime_h <- remove_outliers(grpgrazingtime_h)
  grpgrazingtime_h <- grpgrazingtime_h[!is.na(grpgrazingtime_h)]
  
  #__________________________Cattle __________________
  if (length(BKL18cattle$group) > 0) {
    BKL18cattle$group_sequence <- 1 
  
  for (i in 2:(length(BKL18cattle$group))) {  
    if (BKL18cattle[i,"group"]==BKL18cattle[(i-1),"group"]) {  
      BKL18cattle[i,"group_sequence"]<-BKL18cattle[i-1,"group_sequence"]+1  
    }  
  }  
  first_last_c <- BKL18cattle %>% arrange(group_sequence) %>% group_by(group) %>% slice(c(1,n()))
  
  first_last_c<- first_last_c %>%
    group_by(group) %>% mutate(Diff = DateTime - lag(DateTime)) 
  
  tot_c<- first_last_c[!is.na(first_last_c$Diff),]
  tot_c$Diff[tot_c$Diff <= 100] <- 300
  grpgrazingtime_c <- (as.numeric(tot_c$Diff))
  grpgrazingtime_c <- remove_outliers(grpgrazingtime_c)
  grpgrazingtime_c <- grpgrazingtime_c[!is.na(grpgrazingtime_c)]
  
 if (length(grpgrazingtime_c) == 0) {
   grpgrazingtime_c <- 0
   }
  }
#_____________________________ Elk ________________________
  BRL18elk$group_sequence <- 1  
  for (i in 2:(length(BRL18elk$group))) {  
    if (BRL18elk[i,"group"]==BRL18elk[(i-1),"group"]) {  
      BRL18elk[i,"group_sequence"]<-BRL18elk[i-1,"group_sequence"]+1  
    }  
  }  
  
  first_last_e <- BRL18elk %>% arrange(group_sequence) %>% group_by(group) %>% slice(c(1,n()))
  
  first_last_e<- first_last_e %>%
    group_by(group) %>% mutate(Diff = DateTime - lag(DateTime)) 
  
  tot_e<- first_last_e[!is.na(first_last_e$Diff),]
  tot_e$Diff[tot_e$Diff <= 100] <- 300
  grpgrazingtime_e<- (as.numeric(tot_e$Diff))
  grpgrazingtime_e<- remove_outliers(grpgrazingtime_e)
  grpgrazingtime_e <- grpgrazingtime_e[!is.na(grpgrazingtime_e)]
#_______________________________ Deer _______________________
  if ((length(BRL18deer$group) > 0)) {
  BRL18deer$group_sequence <- 1  
   for (i in 2:(length(BRL18deer$group))) {  
    if (BRL18deer[i,"group"]==BRL18deer[(i-1),"group"]) {  
      BRL18deer[i,"group_sequence"]<-BRL18deer[i-1,"group_sequence"]+1  
    }  
  }  
  first_last_d <- BRL18deer %>% arrange(group_sequence) %>% group_by(group) %>% slice(c(1,n()))
  
  first_last_d<- first_last_d %>%
    group_by(group) %>% mutate(Diff = DateTime - lag(DateTime)) 
  
  tot_d<- first_last_d[!is.na(first_last_d$Diff),]
  tot_d$Diff[tot_d$Diff <= 100] <- 300
  grpgrazingtime_d<- (as.numeric(tot_d$Diff))
  grpgrazingtime_d<- remove_outliers(grpgrazingtime_d)
  grpgrazingtime_d <- grpgrazingtime_d[!is.na(grpgrazingtime_d)]
  }
  #______________________________________________________________________________________________________________________________
  # sauce: https://community.rstudio.com/t/apply-fill-to-all-columns-in-data-frame/3593
  tot_c_build <- data.frame(species = "Cattle", Time = grpgrazingtime_c)
  tot_h_build <- data.frame(species = "Horses", Time = grpgrazingtime_h)
  tot_e_build <- data.frame(species = "Elk", Time = grpgrazingtime_e)
  tot_d_build <- data.frame(species = "Muledeer", Time = grpgrazingtime_d)
  
  totall <- rbind(tot_h_build, tot_c_build, tot_e_build, tot_d_build)
  totall$Time <- (totall$Time/60)
  df.summ <- totall %>% group_by(species) %>% summarize(Mean = mean(Time), Min = min(Time), Max = max(Time), Total = sum(Time))
  # Box plot of the duration each site is visted. 
  box_grztime(totall)
  box_grztime_wmean(totall)
  barchart4sp_werr(totall,"Bear", "Average time spent at site by group", "Time (minutes)")
  