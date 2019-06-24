
#________________________________________________________________________________________________________________________________________________________________________________________
grpgrz_seq <- function(subdf) {
  if (length(subdf$group) <= 1) {
    subdf <- 0
    subdf$group_sequence <- 0
    grpgrazingtime <- (as.numeric(subdf$group))
  }else{
  subdf$group_sequence <- 1
  for (i in 2:(length(subdf$group))) {  
    if (subdf[i,"group"]==subdf[(i-1),"group"]) {  
      subdf[i,"group_sequence"]<-subdf[i-1,"group_sequence"]+1  
    }
  }
  first_last <- subdf %>% arrange(group_sequence) %>% group_by(group) %>% slice(c(1,n()))
  first_last<- first_last %>%
    group_by(group) %>% mutate(Diff = DateTime - lag(DateTime)) 
  
  tot<- first_last[!is.na(first_last$Diff),]
  tot$Diff[tot$Diff <= 100] <- 300
  grpgrazingtime <- (as.numeric(tot$Diff))
  grpgrazingtime <- remove_outliers(grpgrazingtime)
  grpgrazingtime <- grpgrazingtime[!is.na(grpgrazingtime)]
  grpgrazingtime <- ceiling(grpgrazingtime/60)
  }
}

#______________________________building dataframe with total time each group spent _______________________________________
bldtot_grztme <- function(site, year, dfyc, dfyh, dfye, dfyd){
  tot_c_build <- data.frame(site = site, year = year, species = "Cattle", Time = (grpgrz_seq(dfyc)))
  tot_h_build <- data.frame(site = site, year = year, species = "Horses", Time = (grpgrz_seq(dfyh)))
  tot_e_build <- data.frame(site = site, year = year, species = "Elk", Time = (grpgrz_seq(dfye)))
  tot_d_build <- data.frame(site = site, year = year, species = "Muledeer", Time = (grpgrz_seq(dfyd)))
  
  totall <- rbind(tot_h_build, tot_c_build, tot_e_build, tot_d_build)  
}

#______________________________
