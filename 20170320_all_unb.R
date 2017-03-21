require(sp)
require(zoo)
require(gridExtra)
require(grid)
direct <- '/Users/kirsten/Documents/data/RF_trajectories'
setwd(direct)
ptsInfo<-read.csv('20170315_all_rs_ek_veg_hp_ba.csv')
ptsInfoSPDF<-SpatialPointsDataFrame(cbind(ptsInfo$lon,ptsInfo$lat),ptsInfo)

#distThreshold <- 20 #points >10 km away will not be used as unburned controls

###READ IN DATA, PREP FOR ANALYSIS################
#READ IN EK DATA
ekLSNDVI8d<-read.csv('ekPts_Landsat_NDVI8d.csv')
ekLSNDVI8d$system.index<-as.character(ekLSNDVI8d$system.index)

getLSdat<-function(x){
  LSsplt<-unlist(strsplit(x,'_'))
  datInd<-which(nchar(LSsplt) == 8)
  dat<-LSsplt[datInd]
  return(dat)
}

ekLSNDVI8d$YYYYMMDD<-unlist(lapply(ekLSNDVI8d$system.index, function(x) getLSdat(x)))
ekLSNDVI8d$YYYYMMDD<-as.Date(ekLSNDVI8d$YYYYMMDD,format = "%Y%m%d")
ekLSNDVI8d$NDVI<-ekLSNDVI8d$mean
ekLSNDVI8d<-ekLSNDVI8d[which(is.na(ekLSNDVI8d$mean) == F),]

ekGIMMS<-read.csv('ekPts_GIMMS.csv')
GIMMSsplt<-strsplit(as.character(ekGIMMS$system.index),'_')
ekGIMMS$newId<-do.call(rbind, GIMMSsplt)[,2]
GIMMSsplt<-do.call(rbind, GIMMSsplt)[,1]
ekGIMMS$YYYYMMDD<-gsub('a','07',GIMMSsplt)
ekGIMMS$YYYYMMDD<-gsub('b','23',ekGIMMS$YYYYMMDD)
ekGIMMS$YYYYMMDD<-as.Date(ekGIMMS$YYYYMMDD, format = "%Y%m%d")
ekGIMMS$NDVI<-ekGIMMS$ndvi
rm(GIMMSsplt)

ekMOD13Q1<-read.csv('ekPts_MOD_MYD_13Q1.csv')
ekMCD43A4<-read.csv('ekPts_MCD43A4.csv')
ekMCD43A4<-ekMCD43A4[which(is.na(ekMCD43A4$Nadir_Reflectance_Band1) == F),]
ekMCD43A4$NDVI<-(ekMCD43A4$Nadir_Reflectance_Band2-ekMCD43A4$Nadir_Reflectance_Band1)/(ekMCD43A4$Nadir_Reflectance_Band2+ekMCD43A4$Nadir_Reflectance_Band1)

ekModDfList<- list(ekMOD13Q1=ekMOD13Q1,ekMCD43A4=ekMCD43A4)
ekModDfList <- lapply(ekModDfList, function(modDf) {
  aschar<-as.character(modDf$system.index)
  splt<-strsplit(aschar,'_')
  sensor<-unique(do.call(rbind,splt)[,1])
  if (sensor == 'MCD43A4'){
    yr<- do.call(rbind,splt)[,3]
    mnth<- do.call(rbind,splt)[,4]
    dy<-do.call(rbind,splt)[,5]
    modDf$newId<-newId<-do.call(rbind,splt)[,6]
  } else {
    yr<- do.call(rbind,splt)[,4]
    mnth<- do.call(rbind,splt)[,5]
    dy<-do.call(rbind,splt)[,6]
    modDf$newId<-newId<-do.call(rbind,splt)[,7]
  }
  
  modDf$YYYYMMDD<-paste(yr,mnth,sep = '')
  modDf$YYYYMMDD<-paste(modDf$YYYYMMD,dy,sep = '')
  modDf$YYYYMMDD<-as.Date(modDf$YYYYMMD,format = '%Y%m%d')
  modDf
})

#READ IN RS DATA
rsLSNDVI8d<-read.csv('rsPts_Landsat_NDVI8d.csv')
rsLSNDVI8d$system.index<-as.character(rsLSNDVI8d$system.index)

rsLSNDVI8d$YYYYMMDD<-unlist(lapply(rsLSNDVI8d$system.index, function(x) getLSdat(x)))
rsLSNDVI8d$YYYYMMDD<-as.Date(rsLSNDVI8d$YYYYMMDD,format = "%Y%m%d")
rsLSNDVI8d$NDVI<-rsLSNDVI8d$mean
rsLSNDVI8d<-rsLSNDVI8d[which(is.na(rsLSNDVI8d$mean) == F),]

rsGIMMS<-read.csv('rsPts_GIMMS.csv')
GIMMSsplt<-strsplit(as.character(rsGIMMS$system.index),'_')
rsGIMMS$newId<-do.call(rbind, GIMMSsplt)[,2]
GIMMSsplt<-do.call(rbind, GIMMSsplt)[,1]
rsGIMMS$YYYYMMDD<-gsub('a','07',GIMMSsplt)
rsGIMMS$YYYYMMDD<-gsub('b','23',rsGIMMS$YYYYMMDD)
rsGIMMS$YYYYMMDD<-as.Date(rsGIMMS$YYYYMMDD, format = "%Y%m%d")
rsGIMMS$NDVI<-rsGIMMS$ndvi
rm(GIMMSsplt)

rsMOD13Q1<-read.csv('rsPts_MOD_MYD_13Q1.csv')
rsMCD43A4<-read.csv('rsPts_MCD43A4.csv')
rsMCD43A4<-rsMCD43A4[which(is.na(rsMCD43A4$Nadir_Reflectance_Band1) == F),]
rsMCD43A4$NDVI<-(rsMCD43A4$Nadir_Reflectance_Band2-rsMCD43A4$Nadir_Reflectance_Band1)/(rsMCD43A4$Nadir_Reflectance_Band2+rsMCD43A4$Nadir_Reflectance_Band1)

rsModDfList<- list(rsMOD13Q1=rsMOD13Q1,rsMCD43A4=rsMCD43A4)
rsModDfList <- lapply(rsModDfList, function(modDf) {
  aschar<-as.character(modDf$system.index)
  splt<-strsplit(aschar,'_')
  sensor<-unique(do.call(rbind,splt)[,1])
  if (sensor == 'MCD43A4'){
    yr<- do.call(rbind,splt)[,3]
    mnth<- do.call(rbind,splt)[,4]
    dy<-do.call(rbind,splt)[,5]
    modDf$newId<-newId<-do.call(rbind,splt)[,6]
  } else {
    yr<- do.call(rbind,splt)[,4]
    mnth<- do.call(rbind,splt)[,5]
    dy<-do.call(rbind,splt)[,6]
    modDf$newId<-newId<-do.call(rbind,splt)[,7]
  }
  
  modDf$YYYYMMDD<-paste(yr,mnth,sep = '')
  modDf$YYYYMMDD<-paste(modDf$YYYYMMD,dy,sep = '')
  modDf$YYYYMMDD<-as.Date(modDf$YYYYMMD,format = '%Y%m%d')
  modDf
})
#######################
#MERGE EK AND RS DATA
########################
ekGIMMS<-ekGIMMS[-grep('region',names(ekGIMMS))]
rsGIMMS<-rbind(rsGIMMS,ekGIMMS)
ekLSNDVI8d<-ekLSNDVI8d[-grep('region',names(ekLSNDVI8d))]
ekLSNDVI8d$yrBrn<-NA
rsLSNDVI8d<-rsLSNDVI8d[-grep('id',names(rsLSNDVI8d))]
rsLSNDVI8d<-rbind(rsLSNDVI8d,ekLSNDVI8d)
ekModDfList$ekMOD13Q1<-ekModDfList$ekMOD13Q1[-grep('region',names(ekModDfList$ekMOD13Q1))]
rsModDfList$rsMOD13Q1<-rbind(rsModDfList$rsMOD13Q1,ekModDfList$ekMOD13Q1)
ekModDfList$ekMCD43A4<-ekModDfList$ekMCD43A4[-grep('region',names(ekModDfList$ekMCD43A4))]
rsModDfList$rsMCD43A4<-rbind(rsModDfList$rsMCD43A4,ekModDfList$ekMCD43A4)

#####GET COORDS, BRN FOR EACH POINT

getNewIds<-function(ts){
  ids<-paste(ts$lon,ts$lat, sep = '_')
  ids<-paste(ids,ts$method,sep = '_')
  ids<-paste(ids,ts$brn,sep = '_')
  newId <- c()
  n<-1
  for (id in unique(ids)){
    newId[which(ids == id)]<-n
    n <- n+1
  }
  ts$newId<-newId
  return(ts)
}

rsLSNDVI8d<-getNewIds(rsLSNDVI8d)

for (id in unique(rsLSNDVI8d$newId)){
  brn<-unique(rsLSNDVI8d$brn[which(rsLSNDVI8d$newId == id)])
  lat<-unique(rsLSNDVI8d$lat[which(rsLSNDVI8d$newId == id)])
  lon<-unique(rsLSNDVI8d$lon[which(rsLSNDVI8d$newId == id)])
  method<-unique(rsLSNDVI8d$method[which(rsLSNDVI8d$newId == id)])
  brnPt<-SpatialPoints(cbind(lon,lat))
  dists<-spDistsN1(ptsInfoSPDF,brnPt)
  nrest<-ptsInfoSPDF[which(dists == min(dists)),]
  hpDist<-nrest$minMODISdist
  baDist<-nrest$minAVHRRdist
  vegType<-as.character(nrest$VEGTYP1)
  addtl<-'none'
  notes<-'none'
  logged<-NA
  if ((brn == 1) & (method == 'field')){
    yrBrn<-nrest$yrBrn
    addtl<-nrest$addtl
    notes<-nrest$notes
    logged<-nrest$Logged
  } 
  if (method != 'field'){
    yrBrn<-unique(rsLSNDVI8d$yrBrn[which(rsLSNDVI8d$newId == id)]) 
  }
  if (brn == 0){
    yrBrn<-0
  }
  #some 2004 points were initially included in 2000 sites
  if (yrBrn == c(2000,2004)){
    yrBrn<-2004
  }
  ptDf<-data.frame(id,lat,lon,brn,yrBrn,method,hpDist,baDist,vegType,addtl,notes,logged)
  if(exists('ptsDf') == F){
    ptsDf<-ptDf
  } else {
    ptsDf<-rbind(ptsDf,ptDf)
  } 
}

assignIds<-function(ts,ptsDf){
  ts$newId<-NA
  for (pt in ptsDf$id){
    ptLat<-ptsDf$lat[which(ptsDf$id == pt)]
    ptLon<-ptsDf$lon[which(ptsDf$id == pt)]
    ptMethod<-ptsDf$method[which(ptsDf$id == pt)]
    ptBrn<-ptsDf$brn[which(ptsDf$id == pt)]
    ts$newId[which((ts$lat == ptLat)&(ts$lon == ptLon)&(ts$method == ptMethod)&(ts$brn == ptBrn))]<-pt
  }
  return(ts)
}

###MAKES SURE ALL newIds are consistent
rsGIMMS<-assignIds(rsGIMMS,ptsDf)
rsModDfList$rsMCD43A4<-assignIds(rsModDfList$rsMCD43A4,ptsDf)
rsModDfList$rsMOD13Q1<-assignIds(rsModDfList$rsMOD13Q1,ptsDf)

#######GET NEAREST SUITABLE CTRL POINTS
brnPts<-ptsDf[which(ptsDf$brn == 1),]
unbPts<-ptsDf[which(ptsDf$brn == 0),]
unbSPDF<-SpatialPointsDataFrame(cbind(unbPts$lon,unbPts$lat),unbPts)

#GET A TIME SERIES, PRINT RAW AND MINUS CONTROL YEAR ROUND AND SUMMER 
VIs<-function(bDat,ts,bId){
  unbNDVI<-unique(ts$NDVI[which((ts$YYYYMMDD == bDat) & (ts$newId == bId))])
  if ('EVI' %in% names(ts)){
    unbEVI<-unique(ts$EVI[which((ts$YYYYMMDD == bDat)& (ts$newId == bId))]) 
    VIdf<-data.frame(bDat,unbNDVI,unbEVI)
  } else{
    VIdf<-data.frame(bDat,unbNDVI)
  }
  return(VIdf)
}

getVIs<-function(ts, bId,VIs){
  tsDats<-unique(ts$YYYYMMDD[which(ts$brn == 0)])
  VI<-lapply(tsDats,function(x) VIs(bDat = x, ts = ts, bId = bId))
  VI<-do.call(rbind,VI)
  return(VI)
}
makePlots<-function(bId,ptsDf,distThreshold,modDfList,rsGIMMS,rsLSNDVI8d,unbSPDF,VIs,getVIs,tsToBfast,tsToReg){
  yrBrn<-ptsDf$yrBrn[which(ptsDf$id == bId)]
  brnLat<-ptsDf$lat[which(ptsDf$id == bId)]
  brnLon<-ptsDf$lon[which(ptsDf$id == bId)]
  brnPt<-SpatialPoints(cbind(brnLon,brnLat))
  #dists<-spDistsN1(unbSPDF,brnPt,longlat = T)
  #unbIds<-unbSPDF$id[which(dists<distThreshold)]
  datBrn<-ptsDf$yrBrn[which(ptsDf$id == bId)]
  datBrn<-as.Date(paste(datBrn,'-07-01', sep = ''))
  method<-ptsDf$method[which(ptsDf$id == bId)]
  hpDist<-ptsDf$hpDist[which(ptsDf$id == bId)]
  baDist<-ptsDf$baDist[which(ptsDf$id == bId)]
  vegType<-ptsDf$vegType[which(ptsDf$id == bId)]
  addtl<-ptsDf$addtl[which(ptsDf$id == bId)]
  notes<-ptsDf$notes[which(ptsDf$id == bId)]
  logged<-ptsDf$logged[which(ptsDf$id == bId)]
  
  #FIND INDICES FROM UNBURNED AREA, SUBTRACT
  #RECREATE AS INDEXminCtrl TIME SERIES
  MOD13Q1ts<-rsModDfList$rsMOD13Q1[which(rsModDfList$rsMOD13Q1$newId == bId),]
  MCD43A4ts<-rsModDfList$rsMCD43A4[which(rsModDfList$rsMCD43A4$newId == bId),]
  LSts<-rsLSNDVI8d[which(rsLSNDVI8d$newId == bId),]
  LSts$DayOfYear<-format(LSts$YYYYMMDD,'%j')
  GIMMSts<-rsGIMMS[which(rsGIMMS$newId == bId),] 
  GIMMSts$DayOfYear<-format(GIMMSts$YYYYMMDD,'%j')
  
  print(paste('Processing', bId, sep = ' '))
  tsList<-list(MOD13Q1ts = MOD13Q1ts, MCD43A4ts = MCD43A4ts,LSts = LSts,GIMMSts = GIMMSts)
  VIunbList<-lapply(tsList,function(x) getVIs(x,bId,VIs)) #FOR GETTING EARLIER BFAST
  
  VIunbList$MOD13Q1ts$unbNDVI<-VIunbList$MOD13Q1ts$unbNDVI/10000 #FOR GETTING EARLIER BFAST
  VIunbList$MOD13Q1ts$unbEVI<-VIunbList$MOD13Q1ts$unbEVI/10000 #FOR GETTING EARLIER BFAST
  
  MOD13Q1VIsUnb<-VIunbList[['MOD13Q1ts']] #FOR GETTING EARLIER BFAST
  MOD13Q1NDVIunb <- zoo(MOD13Q1VIsUnb$unbNDVI,MOD13Q1VIsUnb$bDat) #FOR GETTING EARLIER BFAST
  MOD13Q1EVIunb <- zoo(MOD13Q1VIsUnb$unbEVI,MOD13Q1VIsUnb$bDat)  #FOR GETTING EARLIER BFAST
  
  MCD43A4VIsUnb<-VIunbList[['MCD43A4ts']] #FOR GETTING EARLIER BFAST
  MCD43A4NDVIunb <- zoo(MCD43A4VIsUnb$unbNDVI,MCD43A4VIsUnb$bDat) #FOR GETTING EARLIER BFAST
  
  LSVIsUnb<-VIunbList[['LSts']] #FOR GETTING EARLIER BFAST
  LSNDVIunb <- zoo(LSVIsUnb$unbNDVI,LSVIsUnb$bDat) #FOR GETTING EARLIER BFAST

  GIMMSVIsUnb<-VIunbList[['GIMMSts']] #FOR GETTING EARLIER BFAST
  GIMMSNDVIunb <- zoo(GIMMSVIsUnb$unbNDVI,GIMMSVIsUnb$bDat) #FOR GETTING EARLIER BFAST
  
  VIunbList<-list(MCD43A4NDVIunb = MCD43A4NDVIunb, MOD13Q1NDVIunb = MOD13Q1NDVIunb, MOD13Q1EVIunb = MOD13Q1EVIunb, LSNDVIunb = LSNDVIunb)
  
  #5 PLOTS OF INDICES from different sensors (5 "raw" and 5 minus a control site)
  #ADD Site data in a table below (ID, Lat, lon, year burned, veg type, hotspot indicator)
  imgFil<-paste('UNB_RAW_TRAJ_',bId,sep = '')
  imgFil<-paste(imgFil,'.pdf',sep = '')
  pdf(imgFil,onefile=T, width = 18, height = 28)   
  par (mfrow=c(4,1))
  plot(LSNDVIunb, ylim = c(-0.2,1), ann=FALSE, main = 'LSNDVI')
  LSyrs<-as.Date(paste(unique(format(LSVIsUnb$bDat, '%Y')),'-01-01',sep = ''))
  abline(v = LSyrs, col = 'grey')
  abline(v=yrBrn, col='grey', lty = 2, lwd = 3)
  plot(MOD13Q1NDVIunb, ylim = c(-0.2,1), ann=FALSE, main = 'MOD13Q1NDVI')
  MOD13Q1yrs<-as.Date(paste(unique(format(MOD13Q1VIsUnb$bDat, '%Y')),'-01-01',sep = ''))
  abline(v = MOD13Q1yrs, col = 'grey')
  abline(v=yrBrn, col='grey', lty = 2, lwd = 3)
  plot(MOD13Q1EVIunb, ylim = c(-0.2,1), ann=FALSE, main ='MOD13Q1EVI')
  abline(v = MOD13Q1yrs, col = 'grey')
  abline(v=yrBrn, col='grey', lty = 2, lwd = 3)
  plot(MCD43A4NDVIunb, ylim = c(-0.2,1), ann=FALSE, main = 'MCD43A4NDVI')
  MCD43A4yrs<-as.Date(paste(unique(format(MCD43A4VIsUnb$bDat, '%Y')),'-01-01',sep = ''))
  abline(v = MCD43A4yrs, col = 'grey')
  abline(v=yrBrn, col='grey', lty = 2, lwd = 3)

  sitedetails <- data.frame(bId,brnLon,brnLat,method,brn,hpDist,baDist,vegType,addtl,notes,logged)
  
  othertheme <- gridExtra::ttheme_default(
    core = list(fg_params=list(cex = 1)),
    colhead = list(fg_params=list(cex = 1)),
    rowhead = list(fg_params=list(cex = 1)))
  
  ot <- gridExtra::tableGrob(sitedetails, theme = othertheme)
  
  pushViewport(viewport(y=0.5,height=1))
  grid.draw(ot)
  dev.off()   
  return(sitedetails)
}
}

brnPtsRun<-brnPts$id
unbPtsRun<-unbPts$id
siteDetailsOutput<-lapply(unbPtsRun, function (x) makePlots(x,ptsDf,distThreshold,modDfList,rsGIMMS,rsLSNDVI8d,unbSPDF,VIs,getVIs,tsToBfast,tsToReg))
siteDetails<-do.call(rbind,siteDetailsOutput)
write.csv(siteDetails,'UNB_site_details.csv')