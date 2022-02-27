library("scatterplot3d")
China<-data.frame("Num"=1)
China<-as.data.frame(seq(1:17280));names(China)[1]<-"Num"
temp=subset(Yield,Lon==126.75|Lon==108.25)

for (i in China$Num){
  x=0.5157-mean(temp$YTeCS[which(temp$Num==i)])
  China[i,"diffC"]=x
}
dev.new()
hist(China$diffC)

Hich<-data.frame("Num"=1)
Hich<-as.data.frame(seq(1:17280));names(Hich)[1]<-"Num"
temph=subset(HI[,-c(12:23)],Lon==126.75|Lon==108.25)

for (i in Hich$Num){
  x=mean(temph$Diff[which(temph$Num==i)])
  Hich[i,"difHIch"]=x
}
dev.new()
hist(Hich$difHIch)



USA<-data.frame("Num"=1)
USA<-as.data.frame(seq(1:17280));names(USA)[1]<-"Num"
temp1=subset(Yield,Lon>-87&Lon< -83)

for (i in USA$Num){
  x=mean(temp1$YTeCS[which(temp1$Num==i)])-0.934
  USA[i,"diffU"]=x
}
dev.new()
hist(USA$diffU)

Hius<-data.frame("Num"=1)
Hius<-as.data.frame(seq(1:17280));names(Hius)[1]<-"Num"
temp1h=subset(HI[,-c(12:23)],Lon>-87&Lon< -83)

for (i in Hius$Num){
  x=mean(temp1h$Diff[which(temp1h$Num==i)])
  Hius[i,"difHIus"]=x
}
dev.new()
hist(Hius$difHIus)

EU<-data.frame("Num"=1)
EU<-as.data.frame(seq(1:17280));names(EU)[1]<-"Num"
temp2=subset(Yield,Lon>-1&Lon< 2| Lon==8.75)

for (i in EU$Num){
  x=mean(temp2$YTeCS[which(temp2$Num==i)])-0.89
  EU[i,"diffE"]=x
}
dev.new()
hist(EU$diffE)

Hieu<-data.frame("Num"=1)
Hieu<-as.data.frame(seq(1:17280));names(Hieu)[1]<-"Num"
temp2h=subset(HI[,-c(12:23)],Lon>-1&Lon< 2| Lon==8.75)

for (i in Hieu$Num){
  x=mean(temp2h$Diff[which(temp2h$Num==i)])
  Hieu[i,"difHIeu"]=x
}
dev.new()
hist(Hieu$difHIeu)

Sam<-data.frame("Num"=1)
Sam<-as.data.frame(seq(1:17280));names(Sam)[1]<-"Num"
temp=subset(Yield,Lon>-99&Lon< -95| Lon>-60&Lon< -43)


for (i in Sam$Num){
  x=mean(temp$YTeCS[which(temp$Num==i)])-0.436
  Sam[i,"diffS"]=x
}
dev.new()
hist(Sam$diffS)

HiSam<-data.frame("Num"=1)
HiSam<-as.data.frame(seq(1:17280));names(HiSam)[1]<-"Num"
temph=subset(HI[,-c(12:23)],Lon>-99&Lon< -95| Lon>-60&Lon< -43)

for (i in HiSam$Num){
  x=mean(temph$Diff[which(temph$Num==i)])
  HiSam[i,"difHIsam"]=x
}
dev.new()
hist(HiSam$difHIsam)

AfAs<-data.frame("Num"=1)
AfAs<-as.data.frame(seq(1:17280));names(AfAs)[1]<-"Num"
temp=subset(Yield,Lon>12&Lon< 78| Lon==3.25)
for (i in AfAs$Num){
  x=mean(temp$YTeCS[which(temp$Num==i)])-0.248
  AfAs[i,"diffAA"]=x
}
dev.new()
hist(AfAs$diffAA)

HiAfAs<-data.frame("Num"=1)
HiAfAs<-as.data.frame(seq(1:17280));names(HiAfAs)[1]<-"Num"
temph=subset(HI[,-c(12:23)],Lon>12&Lon< 78| Lon==3.25)

for (i in HiAfAs$Num){
  x=mean(temph$Diff[which(temph$Num==i)])
  HiAfAs[i,"difHIAA"]=x
}
dev.new()
hist(HiAfAs$difHIAA)

hi<-data.frame("Num"=1)
hi<-as.data.frame(seq(1:17280));names(hi)[1]<-"Num"
temh=HI[,-c(12:23)]

for (i in hi$Num){
  x=mean(temh$Diff[which(temh$Num==i)])
  hi[i,"difhi"]=x
}
dev.new()
hist(hi$difhi)


MaiRegions<-list(China, USA, EU, Sam, AfAs, Hich, Hius,Hieu, HiSam, HiAfAs,hi)
Mreg<-join_all(MaiRegions)

#Wheat
Chinaw<-data.frame("Num"=1)
Chinaw<-as.data.frame(seq(1:17280));names(Chinaw)[1]<-"Num"
tempw=subset(YieldW,Lon>108&Lon<126)

for (i in Chinaw$Num){
  x=mean(tempw$YTeW[which(tempw$Num==i)])-0.4350
  Chinaw[i,"diffC"]=x
}
dev.new()
hist(Chinaw$diffC)

Hiwch<-data.frame("Num"=1)
Hiwch<-as.data.frame(seq(1:17280));names(Hiwch)[1]<-"Num"
tempwh=subset(HIw[,-c(12:23)],Lon>108&Lon<126)

for (i in Hiwch$Num){
  x=mean(tempwh$Diff[which(tempwh$Num==i)])
  Hiwch[i,"difHIch"]=x
}
dev.new()
hist(Hiwch$difHIch)

USAw<-data.frame("Num"=1)
USAw<-as.data.frame(seq(1:17280));names(USAw)[1]<-"Num"
tempw1=subset(YieldW,Lon>-121&Lon< -110|Lon>-98.5&Lon< -97)

for (i in USAw$Num){
  x=mean(tempw1$YTeW[which(tempw1$Num==i)])-0.2818
  USAw[i,"diffU"]=x
}
dev.new()
hist(USAw$diffU)

Hiwus<-data.frame("Num"=1)
Hiwus<-as.data.frame(seq(1:17280));names(Hiwus)[1]<-"Num"
tempw1h=subset(HIw[,-c(12:23)],Lon>-121&Lon< -110|Lon>-98.5&Lon< -97)

for (i in Hiwus$Num){
  x=mean(tempw1h$Diff[which(tempw1h$Num==i)])
  Hiwus[i,"difHIus"]=x
}
dev.new()
hist(Hiwus$difHIus)

EUw<-data.frame("Num"=1)
EUw<-as.data.frame(seq(1:17280));names(EUw)[1]<-"Num"
tempw2=subset(YieldW,Lon>4&Lon< 24)

for (i in EUw$Num){
  x=mean(tempw2$YTeW[which(tempw2$Num==i)])-0.66
  EUw[i,"diffE"]=x
}
dev.new()
hist(EUw$diffE)

Hiweu<-data.frame("Num"=1)
Hiweu<-as.data.frame(seq(1:17280));names(Hiweu)[1]<-"Num"
tempw2h=subset(HIw[,-c(12:23)],Lon>4&Lon< 24)

for (i in Hiweu$Num){
  x=mean(tempw2h$Diff[which(tempw2h$Num==i)])
  Hiweu[i,"difHIeu"]=x
}
dev.new()
hist(Hiweu$difHIeu)

hiw<-data.frame("Num"=1)
hiw<-as.data.frame(seq(1:17280));names(hiw)[1]<-"Num"
temwh=HIw[,-c(12:23)]

for (i in hiw$Num){
  x=mean(temwh$Diff[which(temwh$Num==i)])
  hiw[i,"difhi"]=x
}
dev.new()
hist(hiw$difhi)

WheRegions<-list(Chinaw, USAw, EUw, Hiwch, Hiwus,Hiweu,hiw)
Wreg<-join_all(WheRegions)


dev.new()
barplot(Yield$Diff[which(Yield$Num==4647)],names.arg=Yield$Cult[which(Yield$Num==4647)])


#plots
dev.new()
plot(Wreg$diffC,Wreg$diffU)
plot(Wreg$diffC,Wreg$diffE)
plot(Wreg$diffC,Wreg$difhi)
plot(Wreg$diffU,Wreg$difhi)
plot(Wreg$diffE,Wreg$difhi)

plot(Mreg$diffC,Mreg$diffU)
plot(abs(Mreg$diffU),abs(Mreg$diffE))
plot(Mreg$diffC,Mreg$difHIch)
plot(abs(Mreg$diffU),abs(Mreg$difHIus))
plot(Mreg$diffE,Mreg$difhi)
plot(Mreg$diffE,Mreg$difHIeu)
plot(Mreg$diffU,Mreg$difHIus)
plot(Mreg$diffC,Mreg$difHIch)
plot(Mreg$diffAA,Mreg$difHIAA)
plot(Mreg$diffC,Mreg$diffAA)
plot(Mreg$diffC,Mreg$diffS)
plot(Mreg$diffU,Mreg$diffE)
plot(Mreg$diffU,Mreg$diffC)

plot(Mreg$diffU+Mreg$difHIus,Mreg$diffE+Mreg$difHIeu)
plot(Mreg$diffU+Mreg$difHIus,Mreg$diffAA+Mreg$difHIAA)
plot(Mreg$diffC+Mreg$difHIch,Mreg$diffS+Mreg$difHIsam)

Mreg2=join(Mreg,completeM2[,c(1,17)])
Mreg2=subset(Mreg2,Diff2>=12)


Mreg2$distOC=sqrt(Mreg2$diffC^2+Mreg2$difHIch^2)
Mreg2$distOU=sqrt(Mreg2$diffU^2+Mreg2$difHIus^2)
Mreg2$distOE=sqrt(Mreg2$diffE^2+Mreg2$difHIeu^2)
Mreg2$distOSa=sqrt(Mreg2$diffS^2+Mreg2$difHIsam^2)
Mreg2$distOAA=sqrt(Mreg2$diffAA^2+Mreg2$difHIAA^2)
scatterplot3d(distOC,distOSa,distOAA)
Mreg2$dist1=sqrt(distOC^2+distOSa^2+distOAA^2)
Mreg2$dist2=sqrt(distOU^2+distOE^2)
