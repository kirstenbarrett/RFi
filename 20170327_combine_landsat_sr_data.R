direct<-'/Users/kirsten/Documents/data/RF_trajectories/ALL_SR_ts'
setwd(direct)
fils<-list.files()
fils<-fils[grep('csv',fils)]

for (fil in fils){
  info<-read.csv(fil)
  info<-info[which(is.na(info$B1)==F),]
  sensor<-paste('L',gsub('sr','',unlist(strsplit(unlist(strsplit(fil,'\\.'))[1],'L'))[2]), sep = '')
  #reconcile differences in columns and bands for LC8 data
  if (sensor == 'LT4' | sensor == 'LT5' | sensor == 'LE7'){
    info$NDVI <- (info$B4-info$B3)/(info$B4+info$B3)
    info$NDMI <- (info$B4-info$B5)/(info$B4+info$B5)
    if (exists('allData') == F){
      allData = info
    } else {
      allData = rbind(allData,info)
    }
  } else if (sensor == 'LC8') {
    info$NDVI<-(info$B5-info$B4)/(info$B5+info$B4)
    info$NDMI<-(info$B5-info$B6)/(info$B5+info$B6)
    if(exists('L8data') == F){
      L8data<-info 
    } else {
      L8data<-rbind(L8data,info)
    }
  }
}

missingNamesL8 <- names(allData)[which(names(allData) %in% names(L8data) == F)]
L8data[missingNamesL8] <- NA
missingNamesAll<- names(L8data)[which(names(L8data) %in% names(allData) == F)]
allData[missingNamesAll] <- NA
allData<-rbind(allData,L8data)
allData<-allData[which((allData$cfmask == 0)&(allData$cfmask_conf>=1)),]
allData$dat <- as.Date(substr(as.character(allData$system.index),10,16),format = "%Y%j")

write.csv(allData,'ALL_Landsat_SR.csv')