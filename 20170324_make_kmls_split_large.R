require(rgdal)
direct <- '/Users/kirsten/Documents/data/RF_trajectories/brn_pts_standard_regional'

setwd(direct)
filList<-list.files()
regs<-c('zab','bur')

for (reg in regs){
  shpName<-paste('all_pts_standard_',reg,sep = '')
  shpFil<-readOGR('.',shpName)
  
  shpSplt<-split(shpFil, sample(rep(1:10, round(nrow(shpFil)/10,0))))
  
  for (shpInd in seq(1,length(shpSplt),1)){
    shp<-shpSplt[[shpInd]]
    shpProj<-spTransform(shp, CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
    newName<-paste('VEG_HP_BA_',shpInd,sep = '')
    newName<-paste(newName,reg,sep ='_')
    writeOGR(shpProj, paste(newName,'.kml', sep = ''), layer="regPoints", driver="KML")
  }
}