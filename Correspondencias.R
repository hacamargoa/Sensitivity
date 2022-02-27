library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)
library(ggplot2)
library(ggbreak)
library(grid)
library(gridExtra)
library(ade4)
library(FactoClass)
library(factoextra)
library(magrittr)
library(dplyr)
#Maize

Maiz_Nsen<-subset(SSE2m,sen_loc_ir>11&HIav_CL1<0.59&HIav_CL2>0.3)
Maiz_Nsen<-Maiz_Nsen[,c(-6,-7)]
Clust1<-Maiz_Nsen[order(Maiz_Nsen$Clust1),][c(1:50),-c(2,3,5,14,15)]
Clust1$Clust="1";names(Clust1)[2]="Diff"
Clust2<-Maiz_Nsen[order(Maiz_Nsen$Clust2),][c(1:50),-c(2,3,4,14,15)]
Clust2$Clust="2";names(Clust2)[2]="Diff"
Maize_best=rbind(Clust1,Clust2)[,-1]
Maize_best$CN_range=round(Maize_best$CN_range,1);Maize_best$N_dem_re=round(Maize_best$N_dem_re,1)
Test<-as.data.frame(lapply(Maize_best[,c(3:10)],as.factor))
names(Test)<-c("SLA","CNmin","CNRange","Nret","Cret","kN","Ndred","Clust")
summary(Test,nbelements=Inf)
acm<-dudi.acm(Test,nf=10,scannf = F)
grp <- as.factor(Test[, "Clust"])
options(ggrepel.max.overlaps = Inf)
dev.new()
fviz_mca_biplot(acm, repel=T,label="var",habillage=grp, invisible="none", col.var = "black",pointsize.ind=0.2,labelsize=5,alpha.ind = 1/8, axes = c(1, 2),
                addEllipses=TRUE, ellipse.level=0.95)+labs(title ="Correspondence Analysis Maize")+theme(text = element_text(size = 25),axis.title = element_text(size = 20),
                                                                                                         axis.text = element_text(size = 20),plot.title = element_text( hjust = 0.5))
boxplot(acm)



give.n <- function(x){
  return(c(y = mean(x), label = length(x)))
}

test<-list()
names(Maize_best)<-c("Diff", "Sret","SLA","CNmin","CNrange","Nret","Cret","kN","Ndred","Clust")
for (i in 1:8){
  g1<-ggplot(Maize_best[Maize_best$Clust=="1",], aes(x=as.factor(eval(parse(text=names(Maize_best)[i+1]))), y=Diff,fill=Clust))+
    geom_boxplot(fill="orange",alpha=0.5)+ylim(c(0.19,0.32))+
    theme(legend.position = "none",plot.margin=unit(c(-0.4,-0.1,-0.3,0), "cm"), axis.text.x = element_blank(), axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank(),axis.title=element_text(size=12,face="bold"),axis.text=element_text(size=12,face="bold"))+
    stat_summary(fun = mean, geom = "point", shape=16, size=1,position=position_dodge(1))+
    stat_boxplot(geom = "errorbar", width = 0.2) +
    labs(title="",x="", y = ifelse(i==1|i==5,"Yield difference",""))
  temp <- Maize_best[Maize_best$Clust=="1",] %>% group_by(Parm = factor(eval(parse(text=names(Maize_best)[i+1])))) %>% 
    summarise(Count = length(eval(parse(text=names(Maize_best)[i+1]))))
  d=dim(temp)[1]
  b1<-ggplot(Maize_best[Maize_best$Clust=="1",], aes(x=as.factor(eval(parse(text=names(Maize_best)[i+1])))))+
    geom_bar(color="black",fill="orange",alpha=0.5)+ylim(c(0,max(temp$Count)+5))+
    theme(plot.margin=unit(c(-0.5,-0.1,-0.6,0), "cm"),axis.ticks.y = element_blank(),
          axis.title.y = element_text(margin = margin(t = 0, r = 12, b = 0, l = 0)),
          axis.title=element_text(size=12,face="bold"),axis.text=element_text(size=12,face="bold"))+
    geom_text(stat='count', aes(label=paste0("n=",..count..)), y=max(temp$Count)+5,size=ifelse(d>4,2.8,ifelse(d==4,3.5,4)))+
    labs(title=" ",x="", y = ifelse(i==1|i==5,"Number of simulations",""))
  
  g2<-ggplot(Maize_best[Maize_best$Clust=="2",], aes(x=as.factor(eval(parse(text=names(Maize_best)[i+1]))), y=Diff))+
    geom_boxplot(fill="green",alpha=0.5)+ylim(c(0.19,0.40))+
    theme(legend.position = "none",plot.margin=unit(c(-0.4,-0.1,-0.3,0.2),"cm"), axis.text.y = element_blank(), axis.text.x = element_blank(),
          axis.ticks.y = element_blank(),axis.title=element_text(size=12,face="bold"),axis.text=element_text(size=12,face="bold"))+
    stat_summary(fun = mean, geom = "point", shape=16, size=1,position=position_dodge(1))+
    stat_boxplot(geom = "errorbar", width = 0.2) +
    labs(title=" ",x="", y = "")
  temp <- Maize_best[Maize_best$Clust=="2",] %>% group_by(Parm = factor(eval(parse(text=names(Maize_best)[i+1])))) %>% 
    summarise(Count = length(eval(parse(text=names(Maize_best)[i+1]))))
  d=dim(temp)[1]
  b2<-ggplot(Maize_best[Maize_best$Clust=="2",], aes(x=as.factor(eval(parse(text=names(Maize_best)[i+1])))))+
    geom_bar(color="black",fill="green",alpha=0.5)+ylim(c(0,max(temp$Count)+5))+
    theme(plot.margin=unit(c(-0.5,0,-0.6,0.2), "cm"), axis.text.y = element_blank(),axis.ticks.y = element_blank(),
          axis.title=element_text(size=10,face="bold"),axis.text=element_text(size=12,face="bold"))+
    geom_text(stat='count', aes(label=paste0("n=",..count..)), y=max(temp$Count)+5,size=ifelse(d>4,2.8,ifelse(d==4,3.5,4)))+
    labs(title=" ",x="", y = "")
 
  test[[i]]<-grid.arrange(g1,g2,b1,b2,ncol=2,nrow=2,bottom =(textGrob(names(Maize_best)[i+1],gp=gpar(fontsize = 12,cex=1.2,pos=1))))
  
}
do.call("grid.arrange",c(test,nrow=2))


#Wheat
Wheat_NsenS<-subset(SSE3w,PercSWi>10&HIav_Sp<=0.45&HIav_Sp>=0.3)
Spring<-Wheat_NsenS[order(Wheat_NsenS$SpringC),][c(1:50),-c(3:7,16:19)]
Spring$Cult="Spring";names(Spring)[2]="Diff"
Wheat_NsenW<-subset(SSE3w,PercWWi>10&HIav_Wi<=0.45&HIav_Wi>=0.3)
Winter<-Wheat_NsenW[order(Wheat_NsenW$WinterC),][c(1:50),-c(2,4:7,16:19)]
Winter$Cult="Winter";names(Winter)[2]="Diff"
Wheat_best=rbind(Spring,Winter)[,-1]
Wheat_best$CN_range=round(Wheat_best$CN_range,1);Wheat_best$N_dem_re=round(Wheat_best$N_dem_re,1)
TestW<-as.data.frame(lapply(Wheat_best[,c(3:10)],as.factor))
names(TestW)<-c("SLA","CNmin","CNRange","Nret","Cret","kN","Ndred","Cult")
summary(TestW,nbelements=Inf)
acm<-dudi.acm(TestW,nf=10,scannf = F)
grp <- as.factor(TestW[, "Cult"])
options(ggrepel.max.overlaps = Inf)
fviz_mca_biplot(acm, repel=T,label="var",habillage=grp, invisible="none", col.var = "black",pointsize.ind=0.2,labelsize=5,alpha.ind = 1/8, axes = c(1, 2),
                addEllipses=TRUE, ellipse.level=0.95)+labs(title ="Correspondence Analysis Maize")+theme(text = element_text(size = 25),axis.title = element_text(size = 20),
                                                                                                         axis.text = element_text(size = 20),plot.title = element_text( hjust = 0.5))
boxplot(acm)
names()
testw<-list()
names(Wheat_best)<-c("Diff", "Sret","SLA","CNmin","CNrange","Nret","Cret","kN","Ndred","Cult")
for (i in 1:8){
  g1<-ggplot(Wheat_best[Wheat_best$Cult=="Spring",], aes(x=as.factor(eval(parse(text=names(Wheat_best)[i+1]))), y=Diff,fill=Cult))+
    geom_boxplot(fill="orange",alpha=0.5)+ylim(c(0.24,0.35))+
    theme(legend.position = "none",plot.margin=unit(c(-0.4,-0.1,-0.3,0), "cm"),axis.text.x = element_blank(), axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank(),axis.title=element_text(size=12,face="bold"),axis.text=element_text(size=12,face="bold"))+
    stat_summary(fun = mean, geom = "point", shape=16, size=1,position=position_dodge(1))+
    stat_boxplot(geom = "errorbar", width = 0.2) +
    labs(title="",x="", y = ifelse(i==1|i==5,"Yield difference",""))
  temp <- Wheat_best[Wheat_best$Cult=="Spring",] %>% group_by(Parm = factor(eval(parse(text=names(Wheat_best)[i+1])))) %>% 
    summarise(Count = length(eval(parse(text=names(Wheat_best)[i+1]))))
  d=dim(temp)[1]
  b1<-ggplot(Wheat_best[Wheat_best$Cult=="Spring",], aes(x=as.factor(eval(parse(text=names(Wheat_best)[i+1])))))+
    geom_bar(color="black",fill="orange",alpha=0.5)+ylim(c(0,max(temp$Count)+5))+
    theme(plot.margin=unit(c(-0.5,-0.1,-0.6,0), "cm"),axis.ticks.y = element_blank(),
          axis.title.y = element_text(margin = margin(t = 0, r = 12, b = 0, l = 0)),
          axis.title=element_text(size=12,face="bold"),axis.text=element_text(size=12,face="bold"))+
    geom_text(stat='count', aes(label=paste0("n=",..count..)), y=max(temp$Count)+5,size=ifelse(d>4,2.8,ifelse(d==4,3.5,4)))+
    labs(title=" ",x="", y = ifelse(i==1|i==5,"Number of simulations",""))
  
  g2<-ggplot(Wheat_best[Wheat_best$Cult=="Winter",], aes(x=as.factor(eval(parse(text=names(Wheat_best)[i+1]))), y=Diff))+
    geom_boxplot(fill="green",alpha=0.5)+ylim(c(0.24,0.36))+
    theme(legend.position = "none",plot.margin=unit(c(-0.4,0,-0.3,0.2),"cm"),axis.text.y = element_blank(), 
          axis.text.x = element_blank(),axis.ticks.y = element_blank(),axis.ticks.x = element_blank(),
          axis.title=element_text(size=10,face="bold"),axis.text=element_text(size=12,face="bold"))+
    stat_summary(fun = mean, geom = "point", shape=16, size=1,position=position_dodge(1))+
    stat_boxplot(geom = "errorbar", width = 0.2) +
    labs(title=" ",x="", y = "")
  temp <- Wheat_best[Wheat_best$Cult=="Winter",] %>% group_by(Parm = factor(eval(parse(text=names(Wheat_best)[i+1])))) %>% 
    summarise(Count = length(eval(parse(text=names(Wheat_best)[i+1]))))
  d=dim(temp)[1]
  b2<-ggplot(Wheat_best[Wheat_best$Cult=="Winter",], aes(x=as.factor(eval(parse(text=names(Wheat_best)[i+1])))))+
    geom_bar(color="black",fill="green",alpha=0.5)+ylim(c(0,max(temp$Count)+5))+
    theme(plot.margin=unit(c(-0.5,0,-0.6,0.2), "cm"), axis.text.y = element_blank(),axis.ticks.y = element_blank(),
          axis.title=element_text(size=10,face="bold"),axis.text=element_text(size=12,face="bold"))+
    geom_text(stat='count', aes(label=paste0("n=",..count..)), y=max(temp$Count)+5,size=ifelse(d>4,2.8,ifelse(d==4,3.5,4)))+
    labs(title=" ",x="", y = "")
  
  testw[[i]]<-grid.arrange(g1,g2,b1,b2,ncol=2,nrow=2,bottom =(textGrob(names(Wheat_best)[i+1],gp=gpar(fontsize = 12,cex=1.2,pos=2))))
  
}
do.call("grid.arrange",c(testw,nrow=2))


