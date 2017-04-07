require(sp)
require(zoo)
require(gridExtra)
require(grid)

direct<-'/Users/kirsten/Documents/data/RF_trajectories/EB_ALL'
setwd(direct)

#distThreshold <- 10 #points >10 km away will not be used as unburned controls

###READ IN DATA, PREP FOR ANALYSIS################
rsLSNDVI8d<-read.csv('ebPts_Landsat_NDVI8d.csv')
rsLSNDVI8d$brn<-1
rsLSNDVI8d$system.index<-as.character(rsLSNDVI8d$system.index)

getLSdat<-function(x){
  LSsplt<-unlist(strsplit(x,'_'))
  datInd<-which(nchar(LSsplt) == 8)
  dat<-LSsplt[datInd]
  return(dat)
}

rsLSNDVI8d$YYYYMMDD<-unlist(lapply(rsLSNDVI8d$system.index, function(x) getLSdat(x)))
rsLSNDVI8d$YYYYMMDD<-as.Date(rsLSNDVI8d$YYYYMMDD,format = "%Y%m%d")
rsLSNDVI8d<-rsLSNDVI8d[which(is.na(rsLSNDVI8d$mean) == F),]

# rsGIMMS<-read.csv('rsPts_GIMMS.csv')
# GIMMSsplt<-strsplit(as.character(rsGIMMS$system.index),'_')
# rsGIMMS$newId<-do.call(rbind, GIMMSsplt)[,2]
# GIMMSsplt<-do.call(rbind, GIMMSsplt)[,1]
# rsGIMMS$YYYYMMDD<-gsub('a','07',GIMMSsplt)
# rsGIMMS$YYYYMMDD<-gsub('b','23',rsGIMMS$YYYYMMDD)
# rsGIMMS$YYYYMMDD<-as.Date(rsGIMMS$YYYYMMDD, format = "%Y%m%d")
# rm(GIMMSsplt)

rsMOD13Q1<-read.csv('ebPts_MOD_MYD_13Q1.csv')
rsMOD13Q1$brn<-1
rsMCD43A4<-read.csv('ebPts_MCD43A4.csv')
rsMCD43A4$brn<-1
rsMCD43A4<-rsMCD43A4[which(is.na(rsMCD43A4$Nadir_Reflectance_Band1) == F),]
rsMCD43A4$NDVI<-(rsMCD43A4$Nadir_Reflectance_Band2-rsMCD43A4$Nadir_Reflectance_Band1)/(rsMCD43A4$Nadir_Reflectance_Band2+rsMCD43A4$Nadir_Reflectance_Band1)
rsMCD43A4$NDMI<-(rsMCD43A4$Nadir_Reflectance_Band2-rsMCD43A4$Nadir_Reflectance_Band7)/(rsMCD43A4$Nadir_Reflectance_Band2+rsMCD43A4$Nadir_Reflectance_Band7)

#get separate lat and lon columns
crdsMOD13Q1 <- strsplit(as.character(rsMOD13Q1$.geo),'\\[')
crdsMOD13Q1 <- do.call(rbind,crdsMOD13Q1)[,2]
crdsMOD13Q1 <- strsplit(crdsMOD13Q1,',')
rsMOD13Q1$lon <- as.numeric(do.call(rbind,crdsMOD13Q1)[,1])
rsMOD13Q1$lat <- as.numeric(gsub(']}','',do.call(rbind,crdsMOD13Q1)[,2]))

#get separate lat and lon columns
crdsMCD43A4 <- strsplit(as.character(rsMCD43A4$.geo),'\\[')
crdsMCD43A4 <- do.call(rbind,crdsMCD43A4)[,2]
crdsMCD43A4 <- strsplit(crdsMCD43A4,',')
rsMCD43A4$lon <- as.numeric(do.call(rbind,crdsMCD43A4)[,1])
rsMCD43A4$lat <- as.numeric(gsub(']}','',do.call(rbind,crdsMCD43A4)[,2]))

#get separate lat and lon columns
crdsLS8d <- strsplit(as.character(rsLSNDVI8d$.geo),'\\[')
crdsLS8d <- do.call(rbind,crdsLS8d)[,2]
crdsLS8d <- strsplit(crdsLS8d,',')
rsLSNDVI8d$lon <- as.numeric(do.call(rbind,crdsLS8d)[,1])
rsLSNDVI8d$lat <- as.numeric(gsub(']}','',do.call(rbind,crdsLS8d)[,2]))

rsLSNDVI8d$method<-'eb_field'
rsMOD13Q1$method<-'eb_field'
rsMCD43A4$method<-'eb_field'

modDfList<- list(rsMOD13Q1=rsMOD13Q1,rsMCD43A4=rsMCD43A4)
modDfList <- lapply(modDfList, function(modDf) {
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

#####GET COORDS, BRN FOR EACH POINT
getNewIds<-function(ts){
  ids<-paste(ts$lon,ts$lat, sep = '_')
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
  desc<-unique(rsLSNDVI8d$description[which(rsLSNDVI8d$newId == id)])
#  yrBrn<-unique(rsLSNDVI8d$yrBrn[which(rsLSNDVI8d$newId == id)])
  #some 2004 points were initially included in 2000 sites
#   if (yrBrn == c(2000,2004)){
#     yrBrn<-2004
#   }
#  ptDf<-data.frame(id,lat,lon,brn,yrBrn,method)
  ptDf<-data.frame(id,lat,lon,method,desc)
  if(exists('ptsDf') == F){
    ptsDf<-ptDf
  } else {
    ptsDf<-rbind(ptsDf,ptDf)
  } 
}

assignIds<-function(ts,ptsDf){
  for (pt in ptsDf$id){
    ptLat<-ptsDf$lat[which(ptsDf$id == pt)]
    ptLon<-ptsDf$lon[which(ptsDf$id == pt)]   
    ts$newId[which((ts$lat == ptLat)&(ts$lon == ptLon))]<-pt
  }
  return(ts)
}
###MAKES SURE ALL newIds are consistent
# rsGIMMS<-assignIds(rsGIMMS,ptsDf)
modDfList$rsMCD43A4<-assignIds(modDfList$rsMCD43A4,ptsDf)
modDfList$rsMOD13Q1<-assignIds(modDfList$rsMOD13Q1,ptsDf)

#######GET NEAREST SUITABLE CTRL POINTS
brnPts<-ptsDf
# unbPts<-ptsDf[which(ptsDf$brn == 0),]
# unbSPDF<-SpatialPointsDataFrame(cbind(unbPts$lon,unbPts$lat),unbPts)


#GET A TIME SERIES, PRINT RAW AND MINUS CONTROL YEAR ROUND AND SUMMER 
#THIS MIGHT NEED TO BE APPLY INSTEAD OF A FIRE LOOP

#MOD13Q1VIs<-function(bDat,ts,bId,unbIds)
MOD13Q1VIs<-function(bDat,ts,bId){
  brnNDVI<-(ts$NDVI[which((ts$YYYYMMDD == bDat)& (ts$newId == bId))])/10000
  brnEVI<-(ts$EVI[which((ts$YYYYMMDD == bDat)& (ts$newId == bId))])/10000
#  NDVIminCtrl<-(brnNDVI-mean(ts$NDVI[which((ts$newId %in% unbIds)&(ts$YYYYMMDD == bDat))]))/10000
#  EVIminCtrl<-(brnEVI-mean(ts$EVI[which((ts$newId %in% unbIds)&(ts$YYYYMMDD == bDat))]))/10000
#  VIdf<-data.frame(bDat,brnNDVI,brnEVI,NDVIminCtrl,EVIminCtrl)
  VIdf<-data.frame(bDat,brnNDVI,brnEVI)
  return(VIdf)
}

#MCD43A4VIs<-function(bDat,ts,bId,unbIds)
MCD43A4VIs<-function(bDat,ts,bId){
  brnNDVI<-ts$NDVI[which((ts$YYYYMMDD == bDat) & (ts$newId == bId))]
  brnNDMI<-ts$NDMI[which((ts$YYYYMMDD == bDat) & (ts$newId == bId))]
  #NDVIminCtrl<-brnNDVI-mean(ts$NDVI[which((ts$newId %in% unbIds)&(ts$YYYYMMDD == bDat))])
  #VIdf<-data.frame(bDat,brnNDVI,NDVIminCtrl)
  VIdf<-data.frame(bDat,brnNDVI,brnNDMI)
  return(VIdf)
}

# GIMMSVIs<-function(bDat,ts,bId,unbIds){
#   brnNDVI<-ts$ndvi[which((ts$YYYYMMDD == bDat) & (ts$newId == bId))]
#   NDVIminCtrl<-brnNDVI-mean(ts$ndvi[which((ts$newId %in% unbIds)&(ts$YYYYMMDD == bDat))])
#   VIdf<-data.frame(bDat,brnNDVI,NDVIminCtrl)
#   return(VIdf)
# }

#LSVIs<-function(bDat,ts,bId,unbIds){
LSVIs<-function(bDat,ts,bId){
  brnNDVI<-ts$mean[which((ts$YYYYMMDD == bDat) & (ts$newId == bId))]
#  NDVIminCtrl<-brnNDVI-mean(ts$mean[which((ts$newId %in% unbIds)&(ts$YYYYMMDD == bDat))])
#  VIdf<-data.frame(bDat,brnNDVI,NDVIminCtrl)
  VIdf<-data.frame(bDat,brnNDVI)
  return(VIdf)
}

#makePlots<-function(bId,ptsDf,distThreshold,modDfList,rsGIMMS,rsLSNDVI8d,MOD13Q1VIs,MCD43A4VIs,GIMMSVIs,LSVIs,unbSPDF){
makePlots<-function(bId,ptsDf,modDfList,rsLSNDVI8d,MOD13Q1VIs,MCD43A4VIs,LSVIs){
  brnLat<-ptsDf$lat[which(ptsDf$id == bId)]
  brnLon<-ptsDf$lon[which(ptsDf$id == bId)]
  brnPt<-SpatialPoints(cbind(brnLon,brnLat))
  desc<-ptsDf$desc[which(ptsDf$id == bId)]
#  dists<-spDistsN1(unbSPDF,brnPt,longlat = T)
#  unbIds<-unbSPDF$id[which(dists<distThreshold)]
 # yrBrn<-ptsDf$yrBrn[which(ptsDf$id == bId)]
 # yrBrn<-as.Date(paste(yrBrn,'-07-01', sep = ''))
  method<-ptsDf$method[which(ptsDf$id == bId)]
#  sitedetails <- data.frame(bId,yrBrn,brnLon,brnLat,method)
  sitedetails <- data.frame(bId,brnLon,brnLat,method,desc)
  
  #FIND INDICES FROM UNBURNED AREA, SUBTRACT
  #RECREATE AS INDEXminCtrl TIME SERIES
  
  #MOD13Q1 VIs: NDVI, EVI
  #MOD13Q1ts<-modDfList$rsMOD13Q1[which((modDfList$rsMOD13Q1$newId == bId)|(modDfList$rsMOD13Q1$newId %in% unbIds)),]
  MOD13Q1ts<-modDfList$rsMOD13Q1[which(modDfList$rsMOD13Q1$newId == bId),]
  #######SUMMER ONLY#########
  #MOD13Q1ts<-MOD13Q1ts[which((MOD13Q1ts$DayOfYear>153)&(MOD13Q1ts$DayOfYear<243)),]
  ###########################
  MOD13Q1dats<-unique(MOD13Q1ts$YYYYMMDD[which(MOD13Q1ts$brn == 1)])
  #MOD13Q1VI<-lapply(MOD13Q1dats,function(x) MOD13Q1VIs(bDat = x, ts = MOD13Q1ts, bId = bId, unbIds = unbIds))
  MOD13Q1VI<-lapply(MOD13Q1dats,function(x) MOD13Q1VIs(bDat = x, ts = MOD13Q1ts, bId = bId))
  MOD13Q1VI<-do.call(rbind,MOD13Q1VI)
  MOD13Q1NDVI <- zoo(MOD13Q1VI$brnNDVI,MOD13Q1VI$bDat)
  MOD13Q1EVI <- zoo(MOD13Q1VI$brnEVI,MOD13Q1VI$bDat)
  MOD13Q1NDVIminCtrl<-zoo(MOD13Q1VI$NDVIminCtrl,MOD13Q1VI$bDat)
  MOD13Q1EVIminCtrl<-zoo(MOD13Q1VI$EVIminCtrl,MOD13Q1VI$bDat)
  
  #MCD43A4: NDVI
  #MCD43A4ts<-modDfList$rsMCD43A4[which((modDfList$rsMCD43A4$newId == bId)|(modDfList$rsMCD43A4$newId %in% unbIds)),]
  MCD43A4ts<-modDfList$rsMCD43A4[which(modDfList$rsMCD43A4$newId == bId),]
  #######SUMMER ONLY#########
  #MCD43A4ts$DayOfYear<-format(MCD43A4ts$YYYYMMDD,'%j')
  #MCD43A4ts<-MCD43A4ts[which((MCD43A4ts$DayOfYear>153)&(MCD43A4ts$DayOfYear<243)),]
  ###########################
  MCD43A4dats<-unique(MCD43A4ts$YYYYMMDD[which(MCD43A4ts$brn == 1)])
 # MCD43A4VI<-lapply(MCD43A4dats,function(x) MCD43A4VIs(bDat = x, ts = MCD43A4ts, bId = bId, unbIds = unbIds))
  MCD43A4VI<-lapply(MCD43A4dats,function(x) MCD43A4VIs(bDat = x, ts = MCD43A4ts, bId = bId))
  MCD43A4VI<-do.call(rbind,MCD43A4VI)
  MCD43A4NDVI<-zoo(MCD43A4VI$brnNDVI,MCD43A4VI$bDat)
  MCD43A4NDMI<-zoo(MCD43A4VI$brnNDMI,MCD43A4VI$bDat)
  #MCD43A4NDVIminCtrl<-zoo(MCD43A4VI$NDVIminCtrl,MCD43A4VI$bDat)
  
#   #GIMMSts ndvi
#   GIMMSts<-rsGIMMS[which((rsGIMMS$newId == bId)|(rsGIMMS$newId %in% unbIds)),] 
#   GIMMSts$DayOfYear<-format(GIMMSts$YYYYMMDD,'%j')
#   #######SUMMER ONLY#########
#   GIMMSts<-GIMMSts[which((GIMMSts$DayOfYear>153)&(GIMMSts$DayOfYear<243)),]
#   ###########################
#   GIMMSdats<-unique(GIMMSts$YYYYMMDD[which(GIMMSts$brn ==1)])
#   GIMMSVI<-lapply(GIMMSdats,function(x) GIMMSVIs(bDat = x, ts = GIMMSts, bId = bId, unbIds = unbIds))
#   GIMMSVI<-do.call(rbind,GIMMSVI)
#   GIMMSNDVI<-zoo(GIMMSVI$brnNDVI,GIMMSVI$bDat)
#   GIMMSNDVIminCtrl<-zoo(GIMMSVI$NDVIminCtrl,GIMMSVI$bDat)
#   
  #'mean' == ndvi
  #LSts<-rsLSNDVI8d[which((rsLSNDVI8d$newId == bId)|(rsLSNDVI8d$newId %in% unbIds)),]
  LSts<-rsLSNDVI8d[which(rsLSNDVI8d$newId == bId),]
#  LSts$DayOfYear<-format(LSts$YYYYMMDD,'%j')
  #######SUMMER ONLY#########
 # LSts<- LSts[which((LSts$DayOfYear>153)&(LSts$DayOfYear<243)),]
  ###########################
  LSdats<-unique(LSts$YYYYMMDD[which(LSts$brn ==1)])
  #LSVI<-lapply(LSdats,function(x) LSVIs(bDat = x, ts = LSts, bId = bId, unbIds = unbIds))
  LSVI<-lapply(LSdats,function(x) LSVIs(bDat = x, ts = LSts, bId = bId))
  LSVI<-do.call(rbind,LSVI)
  LSNDVI<-zoo(LSVI$brnNDVI,LSVI$bDat)
  LSNDVIminCtrl<-zoo(LSVI$NDVIminCtrl,LSVI$bDat)
  
  #5 PLOTS OF INDICES from different sensors
  #ADD Site data in a table below (ID, Lat, lon, year burned, veg type, hotspot indicator)
  imgFil<-paste('EB_RS_ts_',bId,sep = '')
  imgFil<-paste(imgFil,'.pdf',sep = '')
  pdf(imgFil,onefile=T, width = 28, height = 18)   
  par (mfrow=c(5,1))
  plot(LSNDVI, ylim = c(-0.2,1), ann=FALSE, main = 'LSNDVI')
  LSyrs<-as.Date(paste(unique(format(LSVI$bDat, '%Y')),'-01-01',sep = ''))
  abline(v = LSyrs, col = 'grey')
 # abline(v=yrBrn, col='grey', lty = 2, lwd = 3)
  plot(MOD13Q1NDVI, ylim = c(-0.2,1), ann=FALSE, main = 'MOD13Q1NDVI')
  MOD13Q1yrs<-as.Date(paste(unique(format(MOD13Q1VI$bDat, '%Y')),'-01-01',sep = ''))
  abline(v = MOD13Q1yrs, col = 'grey')
  #abline(v=yrBrn, col='grey', lty = 2, lwd = 3)
  plot(MOD13Q1EVI, ylim = c(-0.2,1), ann=FALSE, main ='MOD13Q1EVI')
  abline(v = MOD13Q1yrs, col = 'grey')
 # abline(v=yrBrn, col='grey', lty = 2, lwd = 3)
  plot(MCD43A4NDVI, ylim = c(-0.2,1), ann=FALSE, main = 'MCD43A4NDVI')
  MCD43A4yrs<-as.Date(paste(unique(format(MCD43A4VI$bDat, '%Y')),'-01-01',sep = ''))
  abline(v = MCD43A4yrs, col = 'grey')
  #abline(v=yrBrn, col='grey', lty = 2, lwd = 3)
  plot(MCD43A4NDMI, ylim = c(-0.2,1), ann=FALSE, main = 'MCD43A4NDMI')
  MCD43A4yrs<-as.Date(paste(unique(format(MCD43A4VI$bDat, '%Y')),'-01-01',sep = ''))
  abline(v = MCD43A4yrs, col = 'grey')
 # abline(v=yrBrn, col='grey', lty = 2, lwd = 3)
  #plot(GIMMSNDVI, ylim = c(-0.2,1), ann=FALSE, main = 'GIMMSNDVI')
  #GIMMSyrs<-as.Date(paste(unique(format(GIMMSVI$bDat, '%Y')),'-01-01',sep = ''))
  #abline(v = GIMMSyrs, col = 'grey')
  #abline(v=yrBrn, col='grey', lty = 2, lwd = 3)
#   plot(LSNDVIminCtrl, ylim = c(-0.6,0.6), ann=FALSE, main = 'LSNDVIminCtrl')
#   abline(v = LSyrs, col = 'grey')
#   abline(v=yrBrn, col='grey', lty = 2, lwd = 3)
#   plot(MOD13Q1NDVIminCtrl, ylim = c(-1,0.6), ann=FALSE, main = 'MOD13Q1NDVIminCtrl')
#   abline(v = MOD13Q1yrs, col = 'grey')
#   abline(v=yrBrn, col='grey', lty = 2, lwd = 3)
#   plot(MOD13Q1EVIminCtrl, ylim = c(-0.6,0.6), ann=FALSE, main = 'MOD13Q1EVIminCtrl')
#   abline(v = MOD13Q1yrs, col = 'grey')
#   abline(v=yrBrn, col='grey', lty = 2, lwd = 3)
#   plot(MCD43A4NDVIminCtrl, ylim = c(-0.6,0.6), ann=FALSE, main = 'MCD43A4NDVIminCtrl')
#   abline(v = MCD43A4yrs, col = 'grey')
#   abline(v=yrBrn, col='grey', lty = 2, lwd = 3)
#   plot(GIMMSNDVIminCtrl, ylim = c(-0.6,0.6), ann=FALSE, main = 'GIMMSNDVIminCtrl')
#   abline(v = GIMMSyrs, col = 'grey')
#   abline(v=yrBrn, col='grey', lty = 2, lwd = 3)
  othertheme <- gridExtra::ttheme_default(
    core = list(fg_params=list(cex = 2)),
    colhead = list(fg_params=list(cex = 2)),
    rowhead = list(fg_params=list(cex = 2)))
  
  ot <- gridExtra::tableGrob(sitedetails, theme = othertheme)
  
  pushViewport(viewport(y=.80, x = .20,height=1))
  grid.draw(ot)
  dev.off()   
}

#lapply(brnPts$id, function (x) makePlots(x,ptsDf,distThreshold,modDfList,rsGIMMS,rsLSNDVI8d,MOD13Q1VIs,MCD43A4VIs,GIMMSVIs,LSVIs,unbSPDF))
lapply(brnPts$id, function (x) makePlots(x,ptsDf,modDfList,rsLSNDVI8d,MOD13Q1VIs,MCD43A4VIs,LSVIs))
