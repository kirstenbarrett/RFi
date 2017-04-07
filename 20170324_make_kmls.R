require(rgdal)
direct <- '/Users/kirsten/Documents/data/RF_trajectories/brn_pts_standard_regional'

setwd(direct)
filList<-list.files()
regs<-c('ang','min','zab','bur')

for (reg in regs){
  shpName<-paste('all_pts_standard_',reg,sep = '')
  shpFil<-readOGR('.',shpName)
  shpProj<-spTransform(shpFil, CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
  newName<-paste('ALL_VEG_HP_BA_',reg,sep = '')
  writeOGR(shpProj, paste(newName,'.kml', sep = ''), layer="regPoints", driver="KML")
}