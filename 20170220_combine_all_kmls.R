directory<-'/Users/kirsten/Documents/data/RF_trajectories/Pts_kmls'
setwd(directory)

#########ELENA'S SITES##############
kml.text<-readLines('ekid.kml')
re <- "<coordinates> *([^<]+?) *<\\/coordinates>"  
coordInd <- grep(re,kml.text)  

re1<-"\t\t<SimpleData name=\"region\">*([^<]+?) *</SimpleData"
regInd<-grep(re1,kml.text)

re2<-"\t\t<SimpleData name=\"years_burn\">*([^<]+?) *</SimpleData"
yrBrnInd<-grep(re2,kml.text)

re3<-"\t\t<SimpleData name=\"Logged\">*([^<]+?) *</SimpleData"
loggedInd<-grep(re3,kml.text)

re4<-"\t\t<SimpleData name=\"notes\">*([^<]+?) *</SimpleData"
#notesInd<-grep(re4,kml.text)

re5<-"\t\t<SimpleData name=\"ID\">*([^<]+?) *</SimpleData"
idInd<-grep(re5,kml.text)

#only works if there's data for every row

for(i in 1:length(coordInd)){   
  temp1 <- gsub("<coordinates>"," ",kml.text[coordInd[i]])  
  temp2 <- gsub("</coordinates>"," ",temp1)  
  temp3 <- gsub("<Point>"," ",temp2)  
  temp4 <- gsub("</Point>"," ",temp3)  
  coordinates <- as.numeric(unlist(strsplit(temp4,",")))
  lon<-coordinates[1]
  lat<-coordinates[2]
  
  region <- gsub("\t\t<SimpleData name=\"region\">",'',kml.text[regInd[i]]) 
  region <- gsub('</SimpleData>','',region)
  
  yrBrn <-  gsub("\t\t<SimpleData name=\"years_burn\">",'',kml.text[yrBrnInd[i]]) 
  yrBrn <- gsub('</SimpleData>','',yrBrn)
  
  logged <-gsub("\t\t<SimpleData name=\"Logged\">",'',kml.text[loggedInd[i]]) 
  logged <- gsub('</SimpleData>','',logged)
  
  ###only some have notes#######
  #notes<-gsub("\t\t<SimpleData name=\"notes\">",'',kml.text[notesInd[i]])
  #notes<-gsub('</SimpleData>','',notes)
  
  id<-gsub("\t\t<SimpleData name=\"ID\">",'',kml.text[idInd[i]])
  id<-gsub('</SimpleData>','',id)
  
  infoDf<-data.frame(lon,lat,id,region,yrBrn)
  if (exists('ekidDf') == F){
    ekidDf<-infoDf
  } else{
    ekidDf<-rbind(ekidDf,infoDf)
  }
}  
rm(i,coordInd,regInd,idInd,loggedInd,yrBrnInd,kml.text,infoDf,id,logged,yrBrn,coordinates,region,lon,lat,re,re1,re2,re3,re4,re5,temp1,temp2,temp3,temp4)

#########ELENA UNB##############
unbEkFils<-c("angUnb.kml","zabUnb.kml","mnkUnb.kml")

for (unbEkFil in unbEkFils){
  kml.text<-readLines(unbEkFil)
  re <- "<coordinates> *([^<]+?) *<\\/coordinates>"  
  coordInd <- grep(re,kml.text)  
  
  region<-substr(unbEkFil,0,3)
  brn<-substr(unbEkFil,4,6)
  
  for(i in 1:length(coordInd)){   
    temp1 <- gsub("<coordinates>"," ",kml.text[coordInd[i]])  
    temp2 <- gsub("</coordinates>"," ",temp1)  
    temp3 <- gsub("<Point>"," ",temp2)  
    temp4 <- gsub("</Point>"," ",temp3)  
    coordinates <- as.numeric(unlist(strsplit(temp4,",")))
    lon<-coordinates[1]
    lat<-coordinates[2]
    
    infoDf<-data.frame(lon,lat,region,brn)
    if (exists('ekUnbDf') == F){
      ekUnbDf<-infoDf
    } else{
      ekUnbDf<-rbind(ekUnbDf,infoDf)
    }
  }  
  rm(coordInd,kml.text,brn,coordinates,infoDf,region,lon,lat,re,temp1,temp2,temp3,temp4) #etc etc etc) #etc etc etc
}
rm(i,unbEkFil,unbEkFils)

######GEE FILES 1990s########################
GEEfils1990s<-c('GEE_BrnPts_1990_1991.kml','GEE_UnbPts_1990_1991.kml','GEE_BrnPts_1994_1996.kml','GEE_UnbPts_1994_1996.kml')

for (GEEfil in GEEfils1990s){
  kml.text<-readLines(GEEfil)
  
  re <- "<coordinates> *([^<]+?) *<\\/coordinates>"  
  coordInd <- grep(re,kml.text) 
  
  re1<-"\t\t<SimpleData name=\"region\">*([^<]+?) *</SimpleData"
  regInd<-grep(re1,kml.text)
  
  re2<-"\t\t<SimpleData name=\"brnYr\">*([^<]+?) *</SimpleData"
  yrBrnInd<-grep(re2,kml.text)
  
  re3<-"\t\t<SimpleData name=\"ID\">*([^<]+?) *</SimpleData"
  idInd<-grep(re3,kml.text)
  
  ###longitude and latitude are switched for these######
  re4<-"\t\t<SimpleData name=\"unbLon\">*([^<]+?) *</SimpleData"
  unbLatInd<-grep(re4,kml.text)
  
  re5<-"\t\t<SimpleData name=\"unbLat\">*([^<]+?) *</SimpleData"
  unbLonInd<-grep(re5,kml.text)
  
  re6<-"\t\t<SimpleData name=\"brnLon\">*([^<]+?) *</SimpleData"
  brnLatInd<-grep(re6,kml.text)
  
  re7<-"\t\t<SimpleData name=\"brnLat\">*([^<]+?) *</SimpleData"
  brnLonInd<-grep(re7,kml.text)
  
  for(i in 1:length(coordInd)){  
    temp1 <- gsub("<coordinates>"," ",kml.text[coordInd[i]])  
    temp2 <- gsub("</coordinates>"," ",temp1)  
    temp3 <- gsub("<Point>"," ",temp2)  
    temp4 <- gsub("</Point>"," ",temp3)  
    coordinates <- as.numeric(unlist(strsplit(temp4,",")))
    lon<-coordinates[1]
    lat<-coordinates[2]
  
    yrBrn <-  gsub("\t\t<SimpleData name=\"brnYr\">",'',kml.text[yrBrnInd[i]]) 
    yrBrn <- gsub('</SimpleData>','',yrBrn)
  
    id<-gsub("\t\t<SimpleData name=\"ID\">",'',kml.text[idInd[i]])
    id<-gsub('</SimpleData>','',id)
    
    ###longitude and latitude switched for these
    unbLat<-gsub("\t\t<SimpleData name=\"unbLon\">",'',kml.text[unbLatInd[i]])
    unbLat<-as.numeric(gsub('</SimpleData>','',unbLat))
    
    unbLon<-gsub("\t\t<SimpleData name=\"unbLat\">",'',kml.text[unbLonInd[i]])
    unbLon<-as.numeric(gsub('</SimpleData>','',unbLon))
    
    brnLat<-gsub("\t\t<SimpleData name=\"brnLon\">",'',kml.text[brnLatInd[i]])
    brnLat<-as.numeric(gsub('</SimpleData>','',brnLat))
    
    brnLon<-gsub("\t\t<SimpleData name=\"brnLat\">",'',kml.text[brnLonInd[i]])
    brnLon<-as.numeric(gsub('</SimpleData>','',brnLon))
    
    if ((brnLon-lon == 0) & (brnLat - lat == 0)){
      brn<-1
    } else if ((unbLon-lon == 0) & (unbLat-lat == 0)){
      brn<-0
    }
    
    infoDf<-data.frame(id,lon,lat,yrBrn,brn)
    if (exists('GEE1990sDf') == F){
      GEE1990sDf<-infoDf
    } else{
      GEE1990sDf<-rbind(GEE1990sDf,infoDf)
    }
  }  
  rm(coordInd,regInd,idInd,brnLatInd,unbLatInd,brnLonInd,unbLonInd,yrBrnInd,brn,coordinates,GEEfil,kml.text,infoDf,id,yrBrn,lon,lat,brnLon,brnLat,unbLon,unbLat,re,re1,re2,re3,re4,re5,re6,re7,temp1,temp2,temp3,temp4) #etc etc etc) #etc etc etc
}
rm(i,GEEfils1990s)

GEEfils2000s<-c('GEE_sites_2000.kml','GEE_sites_2004.kml')
for (GEEfil in GEEfils2000s){
  kml.text<-readLines(GEEfil)
  yrBrn<-as.numeric(gsub('.kml','',unlist(strsplit(GEEfil,'_'))[3]))
  
  re <- "<coordinates> *([^<]+?) *<\\/coordinates>"  
  coordInd <- grep(re,kml.text)  

  re2 <- "\t\t<SimpleData name=\"field_1\">*([^<]+?) *</SimpleData"
  idInd <- grep(re2,kml.text) 
  
  re3 <- "\t\t<SimpleData name=\"s\">*([^<]+?) *</SimpleData"
  sInd <- grep(re3,kml.text) 
  
  for(i in 1:length(coordInd)){ 
    temp1 <- gsub("<coordinates>"," ",kml.text[coordInd[i]])  
    temp2 <- gsub("</coordinates>"," ",temp1)  
    temp3 <- gsub("<Point>"," ",temp2)  
    temp4 <- gsub("</Point>"," ",temp3)  
    coordinates <- as.numeric(unlist(strsplit(temp4,",")))
    lon<-coordinates[1]
    lat<-coordinates[2]
    
    id <- gsub("\t\t<SimpleData name=\"field_1\">",'',kml.text[idInd[i]]) 
    id <- gsub('</SimpleData>','',id)
    
    s <- gsub("\t\t<SimpleData name=\"s\">",'',kml.text[sInd[i]])
    s <- gsub('</SimpleData>','',s)
    if (s == 1){
      brn<-1
    } else if (s ==2){
      brn<-0
    }
    
    infoDf<-data.frame(id,lon,lat,brn,yrBrn)
    if (exists('GEE2000sDf') == F){
      GEE2000sDf<-infoDf
    } else {
      GEE2000sDf<-rbind(GEE2000sDf,infoDf)
    }
  }
rm(i,coordInd,idInd,sInd,kml.text,infoDf,s,id,yrBrn,coordinates,region,lon,lat,re,re2,re3,temp1,temp2,temp3,temp4) #etc etc etc)
}
rm(GEEfils2000s, GEEfil)

###Sites chosen randomly from Hansen forest loss data########
hansenFils<-c('Hansen_20011091155053.kml','Hansen_20021091155053.kml','Hansen_20031091105152.kml')
for (hansenFil in hansenFils){
  kml.text<-readLines(hansenFil)
  yrBrn<-as.numeric(substr(unlist(strsplit(hansenFil,'_'))[2],0,4))
  
  re <- "<coordinates> *([^<]+?) *<\\/coordinates>"  
  coordInd <- grep(re,kml.text)  
  
  re2 <- "\t\t<SimpleData name=\"newIDs\">*([^<]+?) *</SimpleData"
  idInd <- grep(re2,kml.text) 
  
  re3 <- "\t\t<SimpleData name=\"brns\">*([^<]+?) *</SimpleData"
  brnInd <- grep(re3,kml.text) 
  
  for(i in 1:length(coordInd)){ 
    temp1 <- gsub("<coordinates>"," ",kml.text[coordInd[i]])  
    temp2 <- gsub("</coordinates>"," ",temp1)  
    temp3 <- gsub("<Point>"," ",temp2)  
    temp4 <- gsub("</Point>"," ",temp3)  
    coordinates <- as.numeric(unlist(strsplit(temp4,",")))
    lon<-coordinates[1]
    lat<-coordinates[2]
    
    id <- gsub("\t\t<SimpleData name=\"newIDs\">",'',kml.text[idInd[i]]) 
    id <- gsub('</SimpleData>','',id)
    
    brn <- gsub("\t\t<SimpleData name=\"brns\">",'',kml.text[brnInd[i]]) 
    brn <- gsub('</SimpleData>','',brn)
    
    infoDf<-data.frame(id,lon,lat,brn,yrBrn)
    if (exists('hansenDf') == F){
      hansenDf<-infoDf
    } else {
      hansenDf<-rbind(hansenDf,infoDf)
    }
  }
  rm(coordInd,idInd,brnInd,kml.text,infoDf,id,brn,yrBrn,coordinates,lon,lat,re,re2,re3,temp1,temp2,temp3,temp4) #etc etc etc)
}
rm(i,hansenFil, hansenFils)

###################################################
#### COMBINE ALL  #################################
###################################################
ekUnbDf$yrBrn<-NA
ekUnbDf$brn<-0
ekUnbDf$id<-NA
ekidDf$brn<-1
ekidDf<-rbind(ekidDf,ekUnbDf)
rm(ekUnbDf)

GEE1990sDf$yrBrn<-as.numeric(as.character(GEE1990sDf$yrBrn))
GEEsitesDf<-rbind(GEE1990sDf,GEE2000sDf)
rm(GEE1990sDf,GEE2000sDf)

hansenDf$method<-'hansen'
GEEsitesDf$method<-'GEE'
ekidDf$method<-'field'

rsDf<-rbind(GEEsitesDf,hansenDf)
rm(GEEsitesDf,hansenDf)

####Keep EK and RS separate, EK has multiple burn years

ekidSPDF<-SpatialPointsDataFrame(cbind(ekidDf$lon,ekidDf$lat),ekidDf)
proj4string(ekidSPDF)<-"+proj=longlat +datum=WGS84"
rsSPDF<-SpatialPointsDataFrame(cbind(rsDf$lon,rsDf$lat),rsDf)
proj4string(rsSPDF)<-"+proj=longlat +datum=WGS84"
writeOGR(rsSPDF,layer = 'rsPts','rsPts.kml',driver = "KML")
writeOGR(ekidSPDF,layer = 'ekPts','ekPts.kml',driver = "KML")
