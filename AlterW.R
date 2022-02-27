AlterW<-YieldW
AlterW<-subset(AlterW,Lat>50&Lat<61)
AlterW<-AlterW[-c(12:26,28,29)]

SSE<-data.frame("Num"=1)
SSE<-as.data.frame(seq(1:17280));names(SSE)[1]<-"Num"
for (i in SSE$Num){
  SSE[i,"MeanY"]=mean(AlterW$YTeW[which(AlterW$Num==i)])
  }
SSE2<-join(SSE,AlterW[,c(1:9)],match='first')
SSE2<-join(SSE2,SSE3w[c(1,6,7)])
SSE2<-join(SSE2,summw)

bestww<-subset(SSE3w,HIav_Wi<0.47&PercWWi>10)
bestww<-bestww[order(bestww$WinterC),][c(1:50),]
bestww<-join(bestww,SSE2[c(1,2)])
