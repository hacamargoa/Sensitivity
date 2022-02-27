#Source("Senescheck.R")
#library(FactoMineR)
#library(ade4)
#library(FactoClass)
#library(factoextra)
library(missMDA)
library(ca)
library(ncdf4)
library(raster)
library(rworldmap)
library(dplyr)
library(plyr)
library(ltm)
library(devtools)
install_github('sinhrks/ggfortify')
library(ggfortify)

#Areas
crop<-c("SPAMest_Wheat_","SPAMest_Maize_")
cropirr<-c("SPAMest_Wheatirr_","SPAMest_Maizeirr_")
ArNew<-list()
ArNewi<-list()
ArNewr<-list()
setwd("C:/Users/hac809/Documents/Pughtam-cropozone/Global_evaluation_outputs/Area")
for (j in 1:length(crop)){
  ArNew[[j]]<-list.files(pattern=crop[j])
  ArNewi[[j]]<-list.files(pattern=cropirr[j])
  ArNew[[j]]<-brick(ArNew[[j]])
  ArNewi[[j]]<-brick(ArNewi[[j]])*(ArNew[[j]]/ArNew[[j]])
  ArNewr[[j]]<-ArNew[[j]]-ArNewi[[j]]
}

ArWheat05<-ArNew[[1]][[45]];ArWheat05i<-ArNewi[[1]][[45]];ArWheat05r<-ArNewr[[1]][[45]]
ArMaiz05<-ArNew[[2]][[45]];ArMaiz05i<-ArNewi[[2]][[45]];ArMaiz05r<-ArNewr[[2]][[45]]


#Rayyield
crop<-c("Wheat","Maize")
cropR<-c("Ray_Yield_Wheat","Ray_Yield_Maize_")
crop_rayR<-list()
#Prod_ray<-list()
setwd("C:/Users/hac809/Documents/Pughtam-cropozone/Global_evaluation_outputs")
for (j in 1:length(crop)){
  crop_rayR[[j]]<-list.files(pattern=cropR[j])
  crop_rayR[[j]]<-brick(crop_rayR[[j]])
  
}
Deraster<-function(x){
  df<-list()
  for(i in 1:nlayers(x)){
    df[[i]]<-as.data.frame(x[[i]],xy=TRUE)
    df[[i]]$year<-2000+i
  }
  df<-lapply(df, setNames, c("Lon","Lat","Yield","Years"))
  return(bind_rows(df, .id = "label"))
}
crop_rayR1<-list()
for (i in 1:2){
  crop_rayR1[[i]]<-crop_rayR[[i]][[32:41]]
}
setwd("C:/Users/hac809/Desktop/Sensitivity/")
yield_Raydf<-lapply(crop_rayR1,FUN=Deraster)
yield_Raydf<-lapply(yield_Raydf,FUN=na.omit)
ObsC=read.csv("Obs_corn.csv",h=T)
ObsC1=ObsC
ObsC1$Lat[ObsC1$Lat==51.25]<-49.25
YiMa<-yield_Raydf[[2]]
yieldRayMaize<-YiMa[paste0(YiMa$Lon,YiMa$Lat) %in% paste0(ObsC1$Lon,ObsC1$Lat),]
temp1<-data.frame(label=seq(1,8),Lon=-0.75, Lat=41.75, Yield=10.75881, Years=seq(2001,2008))
yieldRayMaize<-rbind(yieldRayMaize,temp1)
yieldRayAvg<-aggregate(Yield~Lon+Lat,yieldRayMaize,mean);colnames(yieldRayAvg)[3]="YieldRay"
yieldRayAvg$Areai<-extract(ArMaiz05i,yieldRayAvg[,c(1,2)])
yieldRayAvg$Arear<-extract(ArMaiz05r,yieldRayAvg[,c(1,2)])
yieldRayAvg$Lat[yieldRayAvg$Lat==49.25]<-51.25
yieldRayAvg$YieldRay=yieldRayAvg$YieldRay/10

#Maize
FMai<- read.csv("C:/Users/hac809/Documents/Pughtam-cropozone/Global_evaluation_inputs/FAO/FAO_MaizBC.csv", h=T)
FMai<-subset(FMai,year<2011&year>2000)
FMaiy<-aggregate(FYield~UN,FMai, FUN=mean)
countries<-as.data.frame(gridCountriesDegreesHalf)
names(countries)<-c("UN","Lon","Lat")
Yield<-join(yield_d2CO2,ObsC)
Yield<-join(Yield,yieldRayAvg)
Yield<-join(Yield,countries)
Yield$Num<-ifelse(Yield$Sret==1,Yield$Num+8640,Yield$Num)
Yield<-join(Yield,FMaiy);Yield$FYield[which(is.na(Yield$FYield))]=7.0
Yield$FYield=Yield$FYield/10
Yield$YTeCS=(Yield$TeCS*Yield$Arear+Yield$TeCSi*Yield$Areai)/(Yield$Areai+Yield$Arear)/(2*0.446*0.87)
Yield$DiffRay<-(Yield$YieldRay-Yield$YTeCS)/Yield$YieldRay
Yield$DiffCount<-(Yield$FYield-Yield$YTeCS)/Yield$FYield


HI<-join(HI_d2CO2,ObsC)
HI<-join(HI,yieldRayAvg)
HI$Num<-ifelse(HI$Sret==1,HI$Num+8640,HI$Num)
HI$HTeCS=(HI$TeCS*HI$Arear+HI$TeCSi*HI$Areai)/(HI$Areai+HI$Arear)
HI$DiffHI<-(HI$Hiobs-HI$HTeCS)/HI$Hiobs


Senes_HI<-join(senes[,-c(12:15)],HI[,c(1:11,23)])
plot(Senes_HI$d_perc_rf,Senes_HI$HTeCS,ylab="HI",xlab="Harvest dead leaves (%) Maize rainfed",col=ifelse(Senes_HI$Sret==1,"black","red"))
plot(Senes_HI$d_perc_ir,Senes_HI$HTeCS,ylab="HI",xlab="Harvest dead leaves (%) Maize irrigated",col=ifelse(Senes_HI$Sret==1,"black","red"))

####Evaluation by location. 

Yi_HIm=join(Yield[,c(1:11,26,27)],HI[,-c(12:23)])
Yi_HIm=join(Yi_HIm,summ)
Yi_HIm$distR=sqrt(Yi_HIm$DiffRay^2+Yi_HIm$DiffHI^2)
Yi_HIm$distC=sqrt((Yi_HIm$DiffCount)^2+(Yi_HIm$DiffHI)^2)
attach(Yi_HIm)
bestsmC<-list()
bestsmR<-list()
for (i in (unique(paste0(Lon,Lat)))){
  temp1=subset(Yi_HIm,paste0(Lon,Lat)==i)
  temp1=temp1[which(temp1$sen_loc_ir>11),]
  bestsmC[[i]]=temp1[order(temp1$distC),][1,]
  bestsmR[[i]]=temp1[order(temp1$distR),][1,]
  }
Cou_bestm=do.call("rbind", bestsmC)[,c(1:11,18)]
Ray_bestm=do.call("rbind", bestsmR)[,c(1:11,17)]


#Function to create graph of k_means
wssplot<-function(data, nc=15, seed=1234){
  wss<-nrow(data)-1*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i]<-sum(kmeans(data,centers=1)$withinss)}
  plot(1:nc,wss,type="b",xlab="Number of Clusters",ylab="Within groups sum of Squares")
}


 wssplot(Ray_bestm[,c(4:5)])
# km_clus<-kmeans(Ray_bestm[,c(2:9)],2)
# autoplot(km_clus,Ray_bestm[,c(2:9)],frame=T)
#Ray_bestm=cbind(Ray_bestm,Cluster=km_clus[[1]])
Ray_bestm$Cluster=ifelse(abs(Ray_bestm$Lat)<35|Ray_bestm$Lon>105,2,1)

# wssplot(Cou_bestm[,c(2:9)])
# km_clusCou<-kmeans(Cou_bestm[,c(2:9)],2)
# autoplot(km_clusCou,Cou_bestm[,c(2:9)],frame=T)
#Cou_bestm=cbind(Cou_bestm,Cluster=km_clusCou[[1]])
Cou_bestm$Cluster=ifelse(abs(Cou_bestm$Lat)<35|Cou_bestm$Lon>105,2,1)


#### all the simulations
# SSE_2<-data.frame("Num"=1)
# SSE_2<-as.data.frame(seq(1:17280));names(SSE_2)[1]<-"Num"
# for (i in SSE_2$Num){
#   SSE_2[i,"Raydist"]=quantile(Yi_HIm$distR[which(Yi_HIm$Num==i)],0.8)
#   SSE_2[i,"Coudist"]=quantile(Yi_HIm$distC[which(Yi_HIm$Num==i)],0.8)
# }
# SSEm<-join(SSE_2,Yi_HIm[,c(1:9)],match='first')
# SSEm<-join(SSEm,summ)


###Distance of QAE for clusters using Country yield  
SSE_3<-data.frame("Num"=1)
SSE_3<-as.data.frame(seq(1:17280));names(SSE_3)[1]<-"Num"
Yi_HIm=join(Yi_HIm,Cou_bestm[,c(10,11,13)]);names(Yi_HIm)[19]="ClusterC"
Yi_HIm=join(Yi_HIm,Ray_bestm[,c(10,11,13)]);names(Yi_HIm)[20]="ClusterR"
for (i in SSE_3$Num){
  SSE_3[i,"Clust1C"]=quantile(Yi_HIm$distC[which(Yi_HIm$ClusterC==1&Yi_HIm$Num==i)],0.8)
  SSE_3[i,"Clust2C"]=quantile(Yi_HIm$distC[which(Yi_HIm$ClusterC==2&Yi_HIm$Num==i)],0.8)
  SSE_3[i,"Clust1R"]=quantile(Yi_HIm$distR[which(Yi_HIm$ClusterR==1&Yi_HIm$Num==i)],0.8)
  SSE_3[i,"Clust2R"]=quantile(Yi_HIm$distR[which(Yi_HIm$ClusterR==2&Yi_HIm$Num==i)],0.8)
}
SSE3m<-join(SSE_3,Yi_HIm[,c(1:9)],match='first')
SSE3m<-join(SSE3m,summ)

Temp1<-subset(SSE2m,sen_loc_ir>11)
MaizAv<-list()
for (i in 1:4){
  Temp2<-Temp1[order(Temp1[,i+1]),][c(1:10),]
  MaizAv[[i]]<-colMeans(Temp2[,c(6:13)])
}
MaizAv_<-do.call("rbind",MaizAv)
MaizAv_[,1]<-names(Temp1[2:5])
#Wheat
#Rayyield
FWhe<- read.csv("C:/Users/hac809/Documents/Pughtam-cropozone/Global_evaluation_inputs/FAO/FAO_WheaBC.csv", h=T)
FWhe<-subset(FWhe,year<2011&year>2000)
FWhey<-aggregate(FYield~UN,FWhe, FUN=mean)
ObsW1=ObsW
ObsW1$Lon[ObsW1$Lon==23.75]<-23.25
YiWh<-yield_Raydf[[1]]
yieldRayWheat<-YiWh[paste0(YiWh$Lon,YiWh$Lat) %in% paste0(ObsW1$Lon,ObsW1$Lat),]
yieldRayWAvg<-aggregate(Yield~Lon+Lat,yieldRayWheat,mean);colnames(yieldRayWAvg)[3]="YieldRay"
yieldRayWAvg$Lon[yieldRayWAvg$Lon==23.25]<-23.75
yieldRayWAvg$YieldRay=yieldRayWAvg$YieldRay/10
yieldRayWAvg$Areai<-extract(ArWheat05i,yieldRayWAvg[,c(1,2)])
yieldRayWAvg$Arear<-extract(ArWheat05r,yieldRayWAvg[,c(1,2)])
ObsW1$Lon[ObsW1$Lon==23.25]<-23.75
YieldW<-join(yield_d2WW3,ObsW1)
YieldW<-join(YieldW,yieldRayWAvg)
YieldW<-join(YieldW,countries)
YieldW<-join(YieldW,FWhey)
YieldW$FYield=YieldW$FYield/10
YieldW$YTeW=(YieldW$TeW*YieldW$Arear+YieldW$TeWi*YieldW$Areai)/(YieldW$Areai+YieldW$Arear)/(2*0.446*0.88)
YieldW$Num<-ifelse(YieldW$Sret==1,YieldW$Num+8640,YieldW$Num)
YieldW$DiffRay<-abs(YieldW$YTeW-YieldW$YieldRay)
YieldW$DiffCount<-abs(YieldW$YTeW-YieldW$FYield)


HIw<-join(HI_d2WW3,ObsW1)
HIw$Num<-ifelse(HIw$Sret==1,HIw$Num+8640,HIw$Num)
HIw$Areai<-extract(ArWheat05i,HIw[,c(10,11)])
HIw$Arear<-extract(ArWheat05r,HIw[,c(10,11)])
HIw$HTeW=(HIw$TeW*HIw$Arear+HIw$TeWi*HIw$Areai)/(HIw$Areai+HIw$Arear)
HIw$DiffHI1<-abs(HIw$Hiobs-HIw$HTeW)
HIw$DiffHI2<-abs(HIw$Hiobs2-HIw$HTeW)



Senes_HIw<-join(senesw[,-c(12:19)],HIw[,c(1:15,23)])
plot(Senes_HIw$d_perc_wwrf,Senes_HIw$TeWW,ylab="HI",xlab="Ratio_d/a_TeWW",col=ifelse(Senes_HIw$Sret==1,"black","red"))
plot(Senes_HIw$d_perc_wwir,Senes_HIw$TeWWi,ylab="HI",xlab="Ratio_d/a_TeWWi",col=ifelse(Senes_HIw$Sret==1,"black","red"))
plot(Senes_HIw$d_perc_swrf,Senes_HIw$TeSW,ylab="HI",xlab="Ratio_d/a_TeSW",col=ifelse(Senes_HIw$Sret==1,"black","red"))
plot(Senes_HIw$d_perc_swir,Senes_HIw$TeSWi,ylab="HI",xlab="Ratio_d/a_TeSWi",col=ifelse(Senes_HIw$Sret==1,"black","red"))

#Wheat
Yi_HIw=join(YieldW[,c(1:11,17,30:31)],HIw[,c(1:11,28,29)])
Yi_HIw=join(Yi_HIw,summw)


testw1<-data.frame("Num"=1)
testw1<-as.data.frame(seq(1:17280));names(testw1)[1]<-"Num"
for (i in testw1$Num){
  testw1[i,"MAEYiSW"]=quantile(Yi_HIw$DiffCount[which(Yi_HIw$wheat=="Spring"&Yi_HIw$Num==i)],0.8)
  testw1[i,"MAEHiSW"]=quantile(Yi_HIw$DiffHI2[which(Yi_HIw$wheat=="Spring"&Yi_HIw$Num==i)],0.8)
  testw1[i,"MAEYiWW"]=quantile(Yi_HIw$DiffCount[which(Yi_HIw$wheat=="Winter"&Yi_HIw$Num==i)],0.8)
  testw1[i,"MAEHiWW"]=quantile(Yi_HIw$DiffHI2[which(Yi_HIw$wheat=="Winter"&Yi_HIw$Num==i)],0.8)
}

testw_<-join(testw1,Yi_HIw[,c(1:9)],match='first')
testw_<-join(testw_,summw)
testw_$distSW=sqrt(testw_$MAEYiSW^2+testw_$MAEHiSW^2)
testw_$distWW=sqrt(testw_$MAEYiWW^2+testw_$MAEHiWW^2)

Yi_HIw$distR=sqrt(Yi_HIw$DiffRay^2+Yi_HIw$DiffHI^2)
Yi_HIw$distC=sqrt(Yi_HIw$DiffCount^2+Yi_HIw$DiffHI^2)

SSE_2w<-data.frame("Num"=1)
SSE_2w<-as.data.frame(seq(1:17280));names(SSE_2w)[1]<-"Num"
for (i in SSE_2w$Num){
  SSE_2w[i,"Raydist"]=quantile(Yi_HIw$distR[which(Yi_HIw$Num==i)],0.8)
  SSE_2w[i,"Coudist"]=quantile(Yi_HIw$distC[which(Yi_HIw$Num==i)],0.8)
}

SSEw<-join(SSE_2w,Yi_HIw[,c(1:9)],match='first')
SSEw<-join(SSEw,summw)

SSE_4w<-data.frame("Num"=1)
SSE_4w<-as.data.frame(seq(1:17280));names(SSE_4w)[1]<-"Num"
for (i in SSE_4w$Num){
  SSE_4w[i,"Spring"]=quantile(Yi_HIw$distC[which(Yi_HIw$wheat=="Spring"&Yi_HIw$Num==i)],0.8)
  SSE_4w[i,"Winter"]=quantile(Yi_HIw$distC[which(Yi_HIw$wheat=="Winter"&Yi_HIw$Num==i)],0.8)
}
SSE2w<-join(SSE_4w,Yi_HIw[,c(1:9)],match='first')
SSE2w<-join(SSE2w,summw)




#####kmeans wheat

attach(Yi_HIw)
bestsw<-list()
for (i in (unique(paste0(Lon,Lat)))){
  temp1=subset(Yi_HIw,paste0(Lon,Lat)==i)
  temp2=temp1[which(temp1$Diffww>10),]
  bestsw[[i]]=temp2[order(temp2$distC),][1,]
  
}
Ray_bestw=do.call("rbind", bestsw)[,c(1:11,22)]

wssplot(Ray_bestw[,c(2:9)])
km_clusw<-kmeans(Ray_bestw[,c(2:9)],2)
autoplot(km_clusw,Ray_bestw[,c(2:9)],frame=T)
Ray_bestw=cbind(Ray_bestw,Cluster=km_clusw[[1]])

SSE_3w<-data.frame("Num"=1)
SSE_3w<-as.data.frame(seq(1:17280));names(SSE_3w)[1]<-"Num"
Yi_HIw2=join(Yi_HIw,Ray_bestw[,c(10,11,13)])
for (i in SSE_3w$Num){
  SSE_3w[i,"Clus1"]=quantile(Yi_HIw2$distC[which(Yi_HIw2$Cluster==1&Yi_HIw2$Num==i)],0.8)
  SSE_3w[i,"Clus2"]=quantile(Yi_HIw2$distC[which(Yi_HIw2$Cluster==2&Yi_HIw2$Num==i)],0.8)
}
SSE3_3<-join(SSE_3w,Yi_HIw2[,c(1:9)],match='first')
SSE3_3<-join(SSE3_3,summw)