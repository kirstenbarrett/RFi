direct <- '/Users/kirsten/Documents/data/RF_trajectories'
setwd(direct)
ptsInfo<-read.csv('20170315_all_rs_ek_veg_hp_ba.csv')
ptsInfoSPDF<-SpatialPointsDataFrame(cbind(ptsInfo$lon,ptsInfo$lat),ptsInfo)
unbPtsInfo<-read.csv('UNB_site_details.csv')
unbPtsInfo$possible_burn<-as.character(unbPtsInfo$possible_burn)
unbPtsInfo$possible_burn[which(unbPtsInfo$possible_burn == 'none')]<-0
unbPtsInfo$possible_burn<-as.numeric(unbPtsInfo$possible_burn)

distThreshold <- 100 #points >100 km away will not be used as unburned controls
################################################
###READ IN DATA, PREP FOR ANALYSIS################
################################################

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
ekMCD43A4$NDMI<-(ekMCD43A4$Nadir_Reflectance_Band2-ekMCD43A4$Nadir_Reflectance_Band7)/(ekMCD43A4$Nadir_Reflectance_Band2+ekMCD43A4$Nadir_Reflectance_Band7)

ekModDfList<- list(ekMOD13Q1=ekMOD13Q1,ekMCD43A4=ekMCD43A4)
ekModDfList <- lapply(ekModDfList, function(modDf) {
  aschar<-as.character(modDf$system.index)
  splt<-strsplit(aschar,'_')
  sensor<-unique(do.call(rbind,splt)[,1])
  if (sensor == 'MCD43A4'){
    yr<- do.call(rbind,splt)[,3]
    mnth<- do.call(rbind,splt)[,4]
    dy<-do.call(rbind,splt)[,5]
  } else {
    yr<- do.call(rbind,splt)[,4]
    mnth<- do.call(rbind,splt)[,5]
    dy<-do.call(rbind,splt)[,6]
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
rsMCD43A4$NDMI<-(rsMCD43A4$Nadir_Reflectance_Band2-rsMCD43A4$Nadir_Reflectance_Band7)/(rsMCD43A4$Nadir_Reflectance_Band2+rsMCD43A4$Nadir_Reflectance_Band7)

rsModDfList<- list(rsMOD13Q1=rsMOD13Q1,rsMCD43A4=rsMCD43A4)
rsModDfList <- lapply(rsModDfList, function(modDf) {
  aschar<-as.character(modDf$system.index)
  splt<-strsplit(aschar,'_')
  sensor<-unique(do.call(rbind,splt)[,1])
  if (sensor == 'MCD43A4'){
    yr<- do.call(rbind,splt)[,3]
    mnth<- do.call(rbind,splt)[,4]
    dy<-do.call(rbind,splt)[,5]
  } else {
    yr<- do.call(rbind,splt)[,4]
    mnth<- do.call(rbind,splt)[,5]
    dy<-do.call(rbind,splt)[,6]
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
rsModDfList$rsMOD13Q1$NDVI<-rsModDfList$rsMOD13Q1$NDVI/10000
rsModDfList$rsMOD13Q1$EVI<-rsModDfList$rsMOD13Q1$EVI/10000
ekModDfList$ekMCD43A4<-ekModDfList$ekMCD43A4[-grep('region',names(ekModDfList$ekMCD43A4))]
rsModDfList$rsMCD43A4<-rbind(rsModDfList$rsMCD43A4,ekModDfList$ekMCD43A4)
rsLandsatSR<-read.csv('ALL_Landsat_SR.csv')  
rsLandsatSR$YYYYMMDD<-as.Date(as.character(rsLandsatSR$dat))
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
  
  if (method == 'field'){
    brn<-nrest$brn
    if (brn == 1){
      yrBrn<-nrest$yrBrn
      addtl<-nrest$addtl
      notes<-nrest$notes
      logged<-nrest$Logged
    } 
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
rsLandsatSR<-assignIds(rsLandsatSR,ptsDf)
rsModDfList$rsMCD43A4<-assignIds(rsModDfList$rsMCD43A4,ptsDf)
rsModDfList$rsMOD13Q1<-assignIds(rsModDfList$rsMOD13Q1,ptsDf)

#######GET NEAREST SUITABLE CTRL POINTS
brnPts<-ptsDf[which(ptsDf$brn == 1),]
unbPts<-ptsDf[which(ptsDf$brn == 0),]
unbSPDF<-SpatialPointsDataFrame(cbind(unbPts$lon,unbPts$lat),unbPts)

#FIND WHICH UNB POINT MATCHES IN UNB PT INFO FILE (USE SPATIAL PROXIMITY)
for (unbId in unbPtsInfo$bId){
  unbPt<-unbPtsInfo[which(unbPtsInfo$bId == unbId),]
  if (unbPt$possible_burn>0){
    unbPt<-SpatialPoints(cbind(unbPt$brnLon,unbPt$brnLat))
    unbDists<-spDistsN1(unbSPDF,unbPt)
    unbPtRun<-unbSPDF$id[which(unbDists == min(unbDists))]
    unbPtsRun<-c(unbPtsRun,unbPtRun)
  }
}
unbSPDF<-unbSPDF[which(unbSPDF$id %in% unbPtsRun),] #TAKE OUT ALL WITH A SIGNAL OF PREVIOUS BURN

#GET A TIME SERIES
VIs<-function(bDat,ts,bId,unbIds){
  brnNDVI<-unique(ts$NDVI[which((ts$YYYYMMDD == bDat) & (ts$newId == bId))])
  NDVIminCtrl<-unique(brnNDVI-mean(ts$NDVI[which((ts$newId %in% unbIds)&(ts$YYYYMMDD == bDat))]))
  if ('EVI' %in% names(ts)){ #FOR MOD13Q1
    brnEVI<-unique(ts$EVI[which((ts$YYYYMMDD == bDat)& (ts$newId == bId))]) 
    EVIminCtrl<-unique(brnEVI-mean(ts$EVI[which((ts$newId %in% unbIds)&(ts$YYYYMMDD == bDat))]))
    VIdf<-data.frame(bDat,brnNDVI,NDVIminCtrl,brnEVI,EVIminCtrl)
  } else if ('NDMI' %in% names(ts)){ #FOR MCD43A4 and LandsatSR
    brnNDMI<-unique(ts$NDMI[which((ts$YYYYMMDD == bDat)& (ts$newId == bId))]) 
    NDMIminCtrl<-unique(brnNDMI-mean(ts$NDMI[which((ts$newId %in% unbIds)&(ts$YYYYMMDD == bDat))]))
    VIdf<-data.frame(bDat,brnNDVI,NDVIminCtrl,brnNDMI,NDMIminCtrl)
  } else {
    VIdf<-data.frame(bDat,brnNDVI,NDVIminCtrl)
  }
  return(VIdf)
}

getVIs<-function(ts, bId, unbIds,VIs){
  tsDats<-unique(ts$YYYYMMDD[which(ts$brn == 1)])
  VI<-lapply(tsDats,function(x) VIs(bDat = x, ts = ts, bId = bId, unbIds = unbIds))
  VI<-do.call(rbind,VI)
  return(VI)
}

makePlots<-function(bId,ptsDf,distThreshold,modDfList,rsLSNDVI8d,rsLandsatSR,unbSPDF,VIs,getVIs){
  yrBrn<-ptsDf$yrBrn[which(ptsDf$id == bId)]
  brnLat<-ptsDf$lat[which(ptsDf$id == bId)]
  brnLon<-ptsDf$lon[which(ptsDf$id == bId)]
  brnPt<-SpatialPoints(cbind(brnLon,brnLat))
  dists<-spDistsN1(unbSPDF,brnPt,longlat = T)
  unbIds<-unbSPDF$id[which(dists<distThreshold)]
  datBrn<-ptsDf$yrBrn[which(ptsDf$id == bId)]
  datBrn<-as.Date(paste(datBrn,'-07-01', sep = ''))
  method<-ptsDf$method[which(ptsDf$id == bId)]
  hpDist<-ptsDf$hpDist[which(ptsDf$id == bId)]
  baDist<-ptsDf$baDist[which(ptsDf$id == bId)]
  vegType<-as.character(ptsDf$vegType[which(ptsDf$id == bId)])
  addtl<-as.character(ptsDf$addtl[which(ptsDf$id == bId)])
  notes<-as.character(ptsDf$notes[which(ptsDf$id == bId)])
  logged<-ptsDf$logged[which(ptsDf$id == bId)]

  #FIND INDICES FROM UNBURNED AREA, SUBTRACT
  #RECREATE AS INDEXminCtrl TIME SERIES
  MOD13Q1ts<-rsModDfList$rsMOD13Q1[which((rsModDfList$rsMOD13Q1$newId == bId)|(rsModDfList$rsMOD13Q1$newId %in% unbIds)),]
  MCD43A4ts<-rsModDfList$rsMCD43A4[which((rsModDfList$rsMCD43A4$newId == bId)|(rsModDfList$rsMCD43A4$newId %in% unbIds)),]
  SRts<-rsLandsatSR[which((rsLandsatSR$newId == bId)|(rsLandsatSR$newId %in% unbIds)),]
  LSts<-rsLSNDVI8d[which((rsLSNDVI8d$newId == bId)|(rsLSNDVI8d$newId %in% unbIds)),]

  if(nrow(SRts)>0){
    tsList<-list(MOD13Q1ts = MOD13Q1ts, MCD43A4ts = MCD43A4ts,LSts = LSts, SRts = SRts)
  } else{
    tsList<-list(MOD13Q1ts = MOD13Q1ts, MCD43A4ts = MCD43A4ts,LSts = LSts) 
  }
  VIlist<-lapply(tsList,function(x) getVIs(x,bId,unbIds,VIs))

  MOD13Q1VIs<-VIlist[['MOD13Q1ts']]
  MOD13Q1NDVI <- zoo(MOD13Q1VIs$brnNDVI,MOD13Q1VIs$bDat)
  MOD13Q1NDVIminCtrl <- zoo(MOD13Q1VIs$NDVIminCtrl,MOD13Q1VIs$bDat)
  MOD13Q1EVI <- zoo(MOD13Q1VIs$brnEVI,MOD13Q1VIs$bDat)
  MOD13Q1EVIminCtrl <- zoo(MOD13Q1VIs$EVIminCtrl,MOD13Q1VIs$bDat)

  MCD43A4VIs<-VIlist[['MCD43A4ts']]
  MCD43A4NDVI <- zoo(MCD43A4VIs$brnNDVI,MCD43A4VIs$bDat)
  MCD43A4NDVIminCtrl <- zoo(MCD43A4VIs$NDVIminCtrl,MCD43A4VIs$bDat)
  MCD43A4NDMI <- zoo(MCD43A4VIs$brnNDMI,MCD43A4VIs$bDat)
  MCD43A4NDMIminCtrl <- zoo(MCD43A4VIs$NDMIminCtrl,MCD43A4VIs$bDat)

  LSVIs<-VIlist[['LSts']]
  LSNDVI <- zoo(LSVIs$brnNDVI,LSVIs$bDat)
  LSNDVIminCtrl <- zoo(LSVIs$NDVIminCtrl,LSVIs$bDat)

  if ('SRts' %in% VIlist){
    SRVIs<-VIlist[['SRts']]
    SRNDVI <- zoo(SRVIs$brnNDVI,SRVIs$bDat)
    SRNDMI <- zoo(SRVIs$brnNDMI,SRVIs$bDat)
    SRNDVIminCtrl <- zoo(SRVIs$NDVIminCtrl,SRVIs$bDat)
    SRNDMIminCtrl <- zoo(SRVIs$NDMIminCtrl,SRVIs$bDat)
  }
  
  sitedetails<-cbind(bId,method,yrBrn,hpDist,baDist,vegType,addtl,notes,logged,brnLat,brnLon)
  imgFil<-paste('RS_ts_',bId,sep = '')
  imgFil<-paste(imgFil,'.pdf',sep = '')
  pdf(imgFil,onefile=T, width = 28, height = 18)   
  par (mfrow=c(5,1))
  
  dBrn<-as.Date(paste(yrBrn,'-07-01', sep = ''))
  plot(LSNDVI, ylim = c(-0.2,1), ann=FALSE, main = 'LSNDVI')
  LSyrs<-as.Date(paste(unique(format(LSVIs$bDat, '%Y')),'-01-01',sep = ''))
  abline(v = LSyrs, col = 'grey')
  abline(v=dBrn, col='grey', lty = 2, lwd = 3)
  plot(MOD13Q1NDVI, ylim = c(-0.2,1), ann=FALSE, main = 'MOD13Q1NDVI')
  MOD13Q1yrs<-as.Date(paste(unique(format(MOD13Q1VIs$bDat, '%Y')),'-01-01',sep = ''))
  abline(v = MOD13Q1yrs, col = 'grey')
  abline(v=dBrn, col='grey', lty = 2, lwd = 3)
  plot(MOD13Q1EVI, ylim = c(-0.2,1), ann=FALSE, main ='MOD13Q1EVI')
  abline(v = MOD13Q1yrs, col = 'grey')
  abline(v=dBrn, col='grey', lty = 2, lwd = 3)
  plot(MCD43A4NDVI, ylim = c(-0.2,1), ann=FALSE, main = 'MCD43A4NDVI')
  MCD43A4yrs<-as.Date(paste(unique(format(MCD43A4VIs$bDat, '%Y')),'-01-01',sep = ''))
  abline(v = MCD43A4yrs, col = 'grey')
  abline(v=dBrn, col='grey', lty = 2, lwd = 3)
  plot(MCD43A4NDMI, ylim = c(-0.2,1), ann=FALSE, main = 'MCD43A4NDMI')
  abline(v = MCD43A4yrs, col = 'grey')
  abline(v=dBrn, col='grey', lty = 2, lwd = 3)
  othertheme <- gridExtra::ttheme_default(
    core = list(fg_params=list(cex = 1)),
    colhead = list(fg_params=list(cex = 1)),
    rowhead = list(fg_params=list(cex = 1)))

  ot <- gridExtra::tableGrob(sitedetails, theme = othertheme)

  pushViewport(viewport(y=.80, x = .20,height=1))
  grid.draw(ot)
  dev.off()   

  imgFil<-paste('RS_ts_minCtrl_',bId,sep = '')
  imgFil<-paste(imgFil,'.pdf',sep = '')
  pdf(imgFil,onefile=T, width = 28, height = 18)   
  par (mfrow=c(5,1))
  plot(LSNDVIminCtrl, ylim = c(-0.5,0.5), ann=FALSE, main = 'LSNDVIminCtrl')
  LSyrs<-as.Date(paste(unique(format(LSVIs$bDat, '%Y')),'-01-01',sep = ''))
  abline(v = LSyrs, col = 'grey')
  abline(v=dBrn, col='grey', lty = 2, lwd = 3)
  plot(MOD13Q1NDVIminCtrl, ylim = c(-0.5,0.5), ann=FALSE, main = 'MOD13Q1NDVIminCtrl')
  MOD13Q1yrs<-as.Date(paste(unique(format(MOD13Q1VIs$bDat, '%Y')),'-01-01',sep = ''))
  abline(v = MOD13Q1yrs, col = 'grey')
  abline(v=dBrn, col='grey', lty = 2, lwd = 3)
  plot(MOD13Q1EVIminCtrl, ylim = c(-0.5,0.5), ann=FALSE, main ='MOD13Q1EVIminCtrl')
  abline(v = MOD13Q1yrs, col = 'grey')
  abline(v=dBrn, col='grey', lty = 2, lwd = 3)
  plot(MCD43A4NDVIminCtrl, ylim = c(-0.5,0.5), ann=FALSE, main = 'MCD43A4NDVIminCtrl')
  MCD43A4yrs<-as.Date(paste(unique(format(MCD43A4VIs$bDat, '%Y')),'-01-01',sep = ''))
  abline(v = MCD43A4yrs, col = 'grey')
  abline(v=dBrn, col='grey', lty = 2, lwd = 3)
  plot(MCD43A4NDMIminCtrl, ylim = c(-0.5,0.5), ann=FALSE, main = 'MCD43A4NDMIminCtrl')
  abline(v = MCD43A4yrs, col = 'grey')
  abline(v=dBrn, col='grey', lty = 2, lwd = 3)
    
  othertheme <- gridExtra::ttheme_default(
    core = list(fg_params=list(cex = 1)),
    colhead = list(fg_params=list(cex = 1)),
    rowhead = list(fg_params=list(cex = 1)))
    
  ot <- gridExtra::tableGrob(sitedetails, theme = othertheme)
    
  pushViewport(viewport(y=.80, x = .20,height=1))
  grid.draw(ot)
  dev.off()   
}

lapply(brnPts$id, function (x) makePlots(x,ptsDf,distThreshold,modDfList,rsGIMMS,rsLSNDVI8d,MOD13Q1VIs,MCD43A4VIs,GIMMSVIs,LSVIs,unbSPDF))
