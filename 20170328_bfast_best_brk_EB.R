require(sp)
require(zoo)
require(gridExtra)
require(grid)
library(devtools)
install_github('dutri001/bfastSpatial')
install_github('bendv/rgrowth')
library(rgrowth)
require(bfast)
require(lubridate)

direct<-'/Users/kirsten/Documents/data/RF_trajectories/EB_ALL'
setwd(direct)
#ptsInfo<-read.csv('20170315_all_rs_ek_veg_hp_ba.csv')
#ptsInfoSPDF<-SpatialPointsDataFrame(cbind(ptsInfo$lon,ptsInfo$lat),ptsInfo)
# unbPtsInfo<-read.csv('UNB_site_details.csv')
# unbPtsInfo$possible_burn<-as.character(unbPtsInfo$possible_burn)
# unbPtsInfo$possible_burn[which(unbPtsInfo$possible_burn == 'none')]<-0
# unbPtsInfo$possible_burn<-as.numeric(unbPtsInfo$possible_burn)
# 
# distThreshold <- 100 #points >100 km away will not be used as unburned controls

#####################################################################################
#Modify rgrowth plot function to include a data table of relevant info (bFastdetails)
#####################################################################################
plot.tsreg <- function(x, ylabs = c("data", "|MOSUM|"), bFastDet, siteDet, yrs) {
  # set up plotting area
  lo <- matrix(c(1:3), nr=3, nc=1)
  layout(lo)
  op <- par(mar = c(0, 5, 0, 5), oma = c(3, 3, 3, 3))
  
  # 1) top panel: data time series
  plot(x$data, xlab = '', xaxt = 'n', ylab = ylabs[1])
  abline(v = yrs, col = 'grey')
  lines(x$fit, col = 'blue', lty = 3)
  points(x$data[time(x$data) >= min(time(x$fit)) & time(x$data) <= max(time(x$fit))], type = 'p', pch = '*', cex = 0.7, col = 'blue')
  if(x$start != x$disturbance) {
    abline(v = as.numeric(x$disturbance), lty = 2, col = 'red')
    abline(v = as.numeric(x$start), lty = 2)
  } else {
    abline(v = as.numeric(x$start), lty = 2)
  }
  if(!is.na (x$regrowth_onset) & x$prereg_check > 0)
    abline(v = as.numeric(x$regrowth_onset), col = 'blue', lty = 3)
  
  # 2) bottom panel: MOSUM time series
  ymax <- max(abs(x$MOSUM), x$bound)
  plot(x$data, col = 'white', ylim = c(min(abs(x$MOSUM)), ymax),ylab = ylabs[2])
  abline(v = yrs, col = 'grey')
  lines(abs(x$MOSUM), lty = 2, col = 'blue')
  lines(zoo(x$bound, time(x$MOSUM)), col = 'green')
  if(x$start != x$disturbance) {
    abline(v = as.numeric(x$disturbance), lty = 2, col = 'red')
    abline(v = as.numeric(x$start), lty = 2)
  } else {
    abline(v = as.numeric(x$start), lty = 2)
  }
  if(!is.na (x$regrowth_onset) & x$prereg_check > 0)
    abline(v = as.numeric(x$regrowth_onset), col = 'blue', lty = 3)
  
  # THIRD PANEL FOR DETAILS
  
  mytheme <- gridExtra::ttheme_default(
    core = list(fg_params=list(cex = 2)),
    colhead = list(fg_params=list(cex = 2)),
    rowhead = list(fg_params=list(cex = 2)))
  
  myt <- gridExtra::tableGrob(bFastDet, theme = mytheme)
  
  pushViewport(viewport(y=.2,height=.5))
  grid.draw(myt)
  
  
  othertheme <- gridExtra::ttheme_default(
    core = list(fg_params=list(cex = 2)),
    colhead = list(fg_params=list(cex = 2)),
    rowhead = list(fg_params=list(cex = 2)))
  
  ot <- gridExtra::tableGrob(siteDet, theme = mytheme)
  
  pushViewport(viewport(y=.2,height=.5))
  grid.draw(ot)
  
  # reset graphical parameters
  layout(1)
  par(op)
}

################################################
###READ IN DATA, PREP FOR ANALYSIS################
################################################

getLSdat<-function(x){
  LSsplt<-unlist(strsplit(x,'_'))
  datInd<-which(nchar(LSsplt) == 8)
  dat<-LSsplt[datInd]
  return(dat)
}
rsLSNDVI8d<-read.csv('ebPts_Landsat_NDVI8d.csv')
rsLSNDVI8d<-rsLSNDVI8d[which(is.na (rsLSNDVI8d$mean) == F),]
rsLSNDVI8d$NDVI<-rsLSNDVI8d$mean
rsMOD13Q1<-read.csv('ebPts_MOD_MYD_13Q1.csv')
rsMCD43A4<-read.csv('ebPts_MCD43A4.csv')
rsLSNDVI8d$method<-'eb_field'
rsMOD13Q1$method<-'eb_field'
rsMCD43A4$method<-'eb_field'

#get dates
LSdat<-unlist(strsplit(as.character(rsLSNDVI8d$system.index),'_'))
rsLSNDVI8d$YYYYMMDD<-as.Date(LSdat[which(nchar(LSdat) == 8)],format = '%Y%m%d')
rsMOD13Q1$YYYYMMDD <-as.Date(substr(rsMOD13Q1$system.index, 15,24),'%Y_%m_%d')
rsMCD43A4$YYYYMMDD <-as.Date(substr(rsMCD43A4$system.index, 13,22),'%Y_%m_%d')

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

rsMCD43A4<-rsMCD43A4[which(is.na (rsMCD43A4$Nadir_Reflectance_Band1) == F),]
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
  lat<-unique(rsLSNDVI8d$lat[which(rsLSNDVI8d$newId == id)])
  lon<-unique(rsLSNDVI8d$lon[which(rsLSNDVI8d$newId == id)])
  method<-unique(rsLSNDVI8d$method[which(rsLSNDVI8d$newId == id)])
  ptDf<-data.frame(id,lat,lon,method)
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
    ts$newId[which((ts$lat == ptLat)&(ts$lon == ptLon)&(ts$method == ptMethod))]<-pt
  }
  return(ts)
}

###MAKES SURE ALL newIds are consistent
rsModDfList$rsMOD13Q1<-assignIds(rsModDfList$rsMOD13Q1,ptsDf)
rsModDfList$rsMCD43A4<-assignIds(rsModDfList$rsMCD43A4,ptsDf)

#######GET NEAREST SUITABLE CTRL POINTS
brnPts<-ptsDf
#unbPts<-ptsDf[which(ptsDf$brn == 0),]
# unbSPDF<-SpatialPointsDataFrame(cbind(unbPts$lon,unbPts$lat),unbPts)

# unbPtsRun<-c()
#FIND WHICH UNB POINT MATCHES IN UNB PT INFO FILE (USE SPATIAL PROXIMITY)
# for (unbId in unbPtsInfo$bId){
#   unbPt<-unbPtsInfo[which(unbPtsInfo$bId == unbId),]
#   if (unbPt$possible_burn>0){
#     unbPt<-SpatialPoints(cbind(unbPt$brnLon,unbPt$brnLat))
#     unbDists<-spDistsN1(unbSPDF,unbPt)
#     unbPtRun<-unbSPDF$id[which(unbDists == min(unbDists))]
#     unbPtsRun<-c(unbPtsRun,unbPtRun)
#   }
# }
# unbSPDF<-unbSPDF[which(unbSPDF$id %in% unbPtsRun),] #TAKE OUT ALL WITH A SIGNAL OF PREVIOUS BURN

#GET A TIME SERIES
VIs<-function(bDat,ts,bId){
  brnNDVI<-unique(ts$NDVI[which((ts$YYYYMMDD == bDat) & (ts$newId == bId))])
  if ('EVI' %in% names(ts)){ #FOR MOD13Q1
    brnEVI<-unique(ts$EVI[which((ts$YYYYMMDD == bDat)&(ts$newId == bId))]) 
    VIdf<-data.frame(bDat,brnNDVI,brnEVI)
  } else if ('NDMI' %in% names(ts)){ #FOR MCD43A4 and LandsatSR
    brnNDMI<-unique(ts$NDMI[which((ts$YYYYMMDD == bDat)& (ts$newId == bId))]) 
    NDMIminCtrl<-unique(brnNDMI-mean(ts$NDMI[which(ts$YYYYMMDD == bDat)]))
    VIdf<-data.frame(bDat,brnNDVI,brnNDMI)
  } else {
    VIdf<-data.frame(bDat,brnNDVI)
  }
  return(VIdf)
}

getVIs<-function(ts, bId,VIs){
  tsDats<-unique(ts$YYYYMMDD)
  VI<-lapply(tsDats,function(x) VIs(bDat = x, ts = ts, bId = bId))
  VI<-do.call(rbind,VI)
  return(VI)
}

makePlots<-function(bId,ptsDf,distThreshold,modDfList,rsGIMMS,rsLSNDVI8d,rsLandsatSR,unbSPDF,VIs,getVIs,tsToBfast,tsToReg){
  print(paste('Processing', bId, sep = ' '))
  # yrBrn<-ptsDf$yrBrn[which(ptsDf$id == bId)]
  brnLat<-ptsDf$lat[which(ptsDf$id == bId)]
  brnLon<-ptsDf$lon[which(ptsDf$id == bId)]
  #  brnPt<-SpatialPoints(cbind(brnLon,brnLat))
  #  dists<-spDistsN1(unbSPDF,brnPt,longlat = T)
  #  unbIds<-unbSPDF$id[which(dists<distThreshold)]
  #  datBrn<-ptsDf$yrBrn[which(ptsDf$id == bId)]
  #  datBrn<-as.Date(paste(datBrn,'-07-01', sep = ''))
  method<-ptsDf$method[which(ptsDf$id == bId)]
  #   hpDist<-ptsDf$hpDist[which(ptsDf$id == bId)]
  #   baDist<-ptsDf$baDist[which(ptsDf$id == bId)]
  #   vegType<-ptsDf$vegType[which(ptsDf$id == bId)]
  #   addtl<-ptsDf$addtl[which(ptsDf$id == bId)]
  #   notes<-ptsDf$notes[which(ptsDf$id == bId)]
  #   logged<-ptsDf$logged[which(ptsDf$id == bId)]
  
  #FIND INDICES FROM UNBURNED AREA, SUBTRACT
  #RECREATE AS INDEXminCtrl TIME SERIES
  MOD13Q1ts<-rsModDfList$rsMOD13Q1[which(rsModDfList$rsMOD13Q1$newId == bId),]
  MCD43A4ts<-rsModDfList$rsMCD43A4[which(rsModDfList$rsMCD43A4$newId == bId),]
  #  SRts<-rsLandsatSR[which((rsLandsatSR$newId == bId)|(rsLandsatSR$newId %in% unbIds)),]
  LSts<-rsLSNDVI8d[which(rsLSNDVI8d$newId == bId),]
  # LSts$DayOfYear<-format(LSts$YYYYMMDD,'%j')
  #  GIMMSts<-rsGIMMS[which((rsGIMMS$newId == bId)|(rsGIMMS$newId %in% unbIds)),] 
  #  GIMMSts$DayOfYear<-format(GIMMSts$YYYYMMDD,'%j')
  
  #  if(nrow(SRts)>0){
  #    tsList<-list(MOD13Q1ts = MOD13Q1ts, MCD43A4ts = MCD43A4ts,LSts = LSts, SRts = SRts, GIMMSts = GIMMSts)
  # } else{
  #    tsList<-list(MOD13Q1ts = MOD13Q1ts, MCD43A4ts = MCD43A4ts,LSts = LSts, GIMMSts = GIMMSts) 
  #  }
  tsList<-list(MOD13Q1ts = MOD13Q1ts, MCD43A4ts = MCD43A4ts,LSts = LSts)
  VIlist<-lapply(tsList,function(x) getVIs(x,bId,VIs))
  
  MOD13Q1VIs<-VIlist[['MOD13Q1ts']]
  MOD13Q1NDVI <- zoo(MOD13Q1VIs$brnNDVI,MOD13Q1VIs$bDat)
  #  MOD13Q1NDVIminCtrl <- zoo(MOD13Q1VIs$NDVIminCtrl,MOD13Q1VIs$bDat)
  MOD13Q1EVI <- zoo(MOD13Q1VIs$brnEVI,MOD13Q1VIs$bDat)
  #  MOD13Q1EVIminCtrl <- zoo(MOD13Q1VIs$EVIminCtrl,MOD13Q1VIs$bDat)
  
  MCD43A4VIs<-VIlist[['MCD43A4ts']]
  MCD43A4NDVI <- zoo(MCD43A4VIs$brnNDVI,MCD43A4VIs$bDat)
  #  MCD43A4NDVIminCtrl <- zoo(MCD43A4VIs$NDVIminCtrl,MCD43A4VIs$bDat)
  MCD43A4NDMI <- zoo(MCD43A4VIs$brnNDMI,MCD43A4VIs$bDat)
  #  MCD43A4NDMIminCtrl <- zoo(MCD43A4VIs$NDMIminCtrl,MCD43A4VIs$bDat)
  
  LSVIs<-VIlist[['LSts']]
  LSNDVI <- zoo(LSVIs$brnNDVI,LSVIs$bDat)
  #   LSNDVIminCtrl <- zoo(LSVIs$NDVIminCtrl,LSVIs$bDat)
  #   
  #   if ('SRts' %in% VIlist){
  #     SRVIs<-VIlist[['SRts']]
  #     SRNDVI <- zoo(SRVIs$brnNDVI,SRVIs$bDat)
  #     SRNDMI <- zoo(SRVIs$brnNDMI,SRVIs$bDat)
  #     SRNDVIminCtrl <- zoo(SRVIs$NDVIminCtrl,SRVIs$bDat)
  #     SRNDMIminCtrl <- zoo(SRVIs$NDMIminCtrl,SRVIs$bDat)
  #   }
  #   
  #   GIMMSVIs<-VIlist[['GIMMSts']]  
  #   GIMMSNDVI <- zoo(GIMMSVIs$brnNDVI,GIMMSVIs$bDat)
  #   GIMMSNDVIminCtrl <- zoo(GIMMSVIs$NDVIminCtrl,GIMMSVIs$bDat)
  #   
  #   if (exists ('SRNDVI')){
  #     VInewList<-list(MCD43A4NDVI = MCD43A4NDVI, MCD43A4NDMI = MCD43A4NDMI, MOD13Q1NDVI = MOD13Q1NDVI, MOD13Q1EVI= MOD13Q1EVI, LSNDVI = LSNDVI, SRNDVI = SRNDVI, SRNDMI = SRNDMI, MCD43A4NDVIminCtrl = MCD43A4NDVIminCtrl, MCD43A4NDMIminCtrl = MCD43A4NDMIminCtrl, MOD13Q1NDVIminCtrl = MOD13Q1NDVIminCtrl, MOD13Q1EVIminCtrl= MOD13Q1EVIminCtrl, LSNDVIminCtrl = LSNDVIminCtrl,SRNDVIminCtrl = SRNDVIminCtrl, SRNDMIminCtrl = SRNDMIminCtrl)
  #   } else {
  #     VInewList<-list(MCD43A4NDVI = MCD43A4NDVI, MCD43A4NDMI = MCD43A4NDMI, MOD13Q1NDVI = MOD13Q1NDVI, MOD13Q1EVI= MOD13Q1EVI, LSNDVI = LSNDVI, MCD43A4NDVIminCtrl = MCD43A4NDVIminCtrl, MCD43A4NDMIminCtrl = MCD43A4NDMIminCtrl, MOD13Q1NDVIminCtrl = MOD13Q1NDVIminCtrl, MOD13Q1EVIminCtrl= MOD13Q1EVIminCtrl, LSNDVIminCtrl = LSNDVIminCtrl)
  #   }
  #   
  VInewList<-list(MCD43A4NDVI = MCD43A4NDVI, MCD43A4NDMI = MCD43A4NDMI, MOD13Q1NDVI = MOD13Q1NDVI, MOD13Q1EVI= MOD13Q1EVI, LSNDVI = LSNDVI)
  
  #GET BEST BREAKPOINT
  minMag<-0 #only select negative magnitude breakpoints
  for (VIname in names(VInewList)){
    VI<-VInewList[[VIname]]
    bts <- bfastts(VI, dates = time(VI), type = 'irregular') 
    strt = decimal_date(min(time(VI)))
    # try(bfm <- bfastmonitor(bts, start = strt, formula = response~harmon, order = 1))
    try(bfm <- bfastmonitor(bts, start = strt+2,order = 1))  
    if(exists('bfm')){
      if (is.na (bfm$breakpoint) == F){
        if (bfm$magnitude < minMag){
          brk<-bfm$breakpoint
          minMag<-bfm$magnitude
        } 
      }
    }
  }
  ##MAKE SURE THIS IS RIGHT, ITEMS IN LIST COULD GET SWITCHED
  for (VIname in names(VInewList)){
    VI<-VInewList[[VIname]]
    bts <- bfastts(VI, dates = time(VI), type = 'irregular') 
    strt = decimal_date(min(time(VI)))
    # try(bfm <- bfastmonitor(bts, start = strt, formula = response~harmon, order = 1))
    try(bfm <- bfastmonitor(bts, start = strt+2,order = 1))
    if(exists('brk')){
      if (minMag<0){
        try(reg <- tsreg(VI, change = brk, h = 0.5))
      }
    }
    if (exists('reg')){
      reg$error_message[is.null(reg$error_message)]<-'none'
      bfastdetails<-cbind(data.frame(bfm[c('magnitude','breakpoint')]), data.frame(reg[c('start','disturbance','regrowth_onset','s','prereg_check','error_message')]))
      sitedetails<-data.frame(VIname,bId,method,brnLat,brnLon)
      
      #     if(bfastdetails$magnitude < -0.01){
      yrs<-unique(as.numeric(format(time(VI),'%Y')))
      imgFil<-paste('EB_Bfast_brk_order1_harmon_trend_default_h_ctrl_avg_',bId,sep = '')
      imgFil<-paste(imgFil,VIname,sep = '')
      imgFil<-paste(imgFil,'.pdf',sep = '')
      pdf(imgFil,onefile=T, width = 28, height = 18)   
      plot(reg,siteDet = sitedetails, bFastDet = bfastdetails, yrs = yrs) 
      dev.off()
      #      }
      #HAVE TO CREATE SOMETHING TO RETURN BFASTOUTPUT FOR EACH bId
      #CREATE ONE BIG CSV OF ALL BFAST DETAILS OR PER INDEX? PER SITE?
      bfastdetails<-cbind(sitedetails,bfastdetails)
      if (exists('bfastOutput') == F){
        bfastOutput<-bfastdetails
      } else {
        bfastOutput<-rbind(bfastOutput,bfastdetails)
      }
    }
    rm(bfm,reg)
  }
  
  if (exists('bfastOutput')){
    return(bfastOutput)
  }
}
###ONLY BRN POST 2002#######
brnPtsRun<-brnPts$id
bfastOutput<-lapply(brnPtsRun, function (x) makePlots(x,ptsDf,distThreshold,modDfList,rsGIMMS,rsLSNDVI8d,rsLandsatSR,unbSPDF,VIs,getVIs,tsToBfast,tsToReg))
bfastOutput<-do.call(rbind,bfastOutput)
write.csv(bfastOutput,'EB_Bfast_brk_order1_harmon_trend_default_h_ctrl_avg.csv')