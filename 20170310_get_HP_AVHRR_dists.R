require(sp)
require(rgdal)
require(rgeos)
require(maptools)

direct <- '/Users/kirsten/Documents/data/RF_trajectories'
setwd(direct)
ichoku<-readOGR('.','hps_ichoku_siberia_w_overpass')

avhrr96<-readOGR('/Users/kirsten/Documents/data/RF_trajectories/AVHRR_BA_shps','avhrr96')
avhrr97<-readOGR('/Users/kirsten/Documents/data/RF_trajectories/AVHRR_BA_shps','avhrr97')
avhrr98<-readOGR('/Users/kirsten/Documents/data/RF_trajectories/AVHRR_BA_shps','avhrr98')
avhrr99<-readOGR('/Users/kirsten/Documents/data/RF_trajectories/AVHRR_BA_shps','avhrr99')
avhrr00<-readOGR('/Users/kirsten/Documents/data/RF_trajectories/AVHRR_BA_shps','avhrr00')
avhrr01<-readOGR('/Users/kirsten/Documents/data/RF_trajectories/AVHRR_BA_shps','avhrr01')
avhrr02<-readOGR('/Users/kirsten/Documents/data/RF_trajectories/AVHRR_BA_shps','avhrr02')

pts<-readOGR('.','all_pts_w_veg')
ptsReproj<-readOGR('.','all_pts_w_veg_reproj_AVHRR_BA')

rm(list=setdiff(ls(), c("pts","ptsReproj", "ichoku", "avhrr96", "avhrr97", "avhrr98","avhrr99","avhrr00", "avhrr01","avhrr02")))
ids<-seq(1,nrow(pts))
avhrrYrs<-seq(1996,2002,1)
modisYrs<-unique(ichoku$Yr)

minMODISdists<-c()
minAVHRRdists<-c()
pts$minMODISdist<-NA
pts$minAVHRRdist<-NA
brnIds<-pts$FID_1[which(pts$brn == 1)]

for (id in brnIds){
  pt<-pts[which(pts$FID_1 == id),]
  ptReproj<-ptsReproj[which(ptsReproj$FID_1 == id),]
  
  minMODISdist<-NA
  if (pt$yrBrn %in% modisYrs){
    ichokuYr<-ichoku[which(ichoku$Yr == pt$yrBrn),]
    minMODISdist<-min(spDistsN1(ichokuYr,pt,longlat = T))*1000 #return meters
    minMODISdists<-c(minMODISdists,minMODISdist)
    pts$minMODISdist[which(pts$FID_1 == id)]<-minMODISdist
  }
  
  minAVHRRdist<-NA
  if (pt$yrBrn %in% avhrrYrs){
    avhrrYr<-substr(as.character(pt$yrBrn),3,4)
    avhrrFil<-get(paste('avhrr',avhrrYr, sep = ''))
    minAVHRRdist<-gDistance(ptReproj,avhrrFil)#returns meters
    pts$minAVHRRdist[which (pts$FID_1 == id)]<-minAVHRRdist
  }
}

write.csv(pts@data,'20170314_all_rs_ek_veg_hp_ba.csv')