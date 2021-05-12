library(plyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)

HI_d1CO<-read.table("HI_d1CO.out",h=T)
HI_d2CO<-read.table("HI_d2CO.out",h=T)

HI_d1CO2<-join(HI_d1CO,yield_av[,c(1,2,4)])
HI_d2CO2<-join(HI_d1CO,yield_av[,c(1,2,4)])

for (j in c("Low","Medium","High")){
#range bars Maize
plotf<-list()
HI.summary<-list()
#HI_d1CO_2<-subset(yield_d1CO2,Group==j)
HI_d2CO_2<-subset(yield_d2CO2,Group==j)
sdev=sd(HI_d2CO_2$TeCS)
me=mean(HI_d2CO_2$TeCS)
#for (i in names(HI_d1CO_2)[2:8]){
for (i in names(HI_d2CO_2)[2:8]){
  #HI_d1CO_3<-HI_d1CO_2[,c(i,"Lon","Lat","TeCS")];names(HI_d1CO_3)[1]<-"Var"
  HI_d2CO_3<-HI_d2CO_2[,c(i,"Lon","Lat","TeCS")];names(HI_d2CO_3)[1]<-"Var"
  #HI.summary_all<- HI_d1CO_3 %>%
  HI.summary_all<- HI_d2CO_3 %>%
    group_by(Var) %>%
    summarise(
      sd_TeCS = sd(TeCS, na.rm = TRUE),
      mean_TeCS = mean(TeCS)
    )
  #HI_sum1<-aggregate(TeCS~Var+Lon+Lat,HI_d1CO_3,mean)
  HI_sum2<-aggregate(TeCS~Var+Lon+Lat,HI_d2CO_3,mean)
  #HI.summary_loc <- HI_sum1 %>%
  HI.summary_loc <- HI_sum2 %>%
    group_by(Var) %>%
    summarise(
      sd_TeCS_locat = sd(TeCS, na.rm = TRUE),
      mean_TeCS_locat= mean(TeCS)
    )
  HI.summary[[i]]<-join(HI.summary_all,HI.summary_loc)
  
  plotf[[i]]=ggplot(HI.summary[[i]]) +
    geom_errorbar(aes(x = mean_TeCS, y = mean_TeCS,
                      ymin = mean_TeCS-sd_TeCS,ymax = mean_TeCS+sd_TeCS,
                      color = as.character(Var)),width = 0.01,size = 2) +
    geom_errorbarh(aes(y = mean_TeCS, xmin = mean_TeCS_locat-sd_TeCS_locat, xmax = mean_TeCS_locat+sd_TeCS_locat,
                       color = as.character(Var)),height = 0.01,size = 2,linetype = "longdash") + 
    geom_point(size = 4, aes(x = mean_TeCS, y = mean_TeCS,color = as.character(Var))) + theme_light() + ggtitle(paste(i,j))+
    theme(legend.title = element_blank(),legend.position= c(0.5,0.05),plot.title = element_text(hjust = 0.5), 
          legend.background = element_rect(fill = "white", color = "black"),legend.direction="horizontal")+  
    xlab("Mean with one Std. over locations") + ylab("Mean with one Std. over all simulations")+xlim(me-sdev-0.07,me+sdev+0.11)+ylim(me-sdev-0.07,me+sdev+0.11)
}
  dev.new()
  do.call("grid.arrange",c(plotf, nrow = 2))

}
#range bars Wheat
HI_d1WW<-read.table("HI_d1WW.out",h=T)
HI_d2WW<-read.table("HI_d2WW.out",h=T)

#range bars
plotfw<-list()
HI.summaryw<-list()
for (i in names(HI_d1WW)[2:8]){
#for (i in names(HI_d2WW)[2:8]){
  HI.summary_all<- HI_d1WW %>%
  #HI.summary_all<- HI_d2WW %>%
    group_by(get(i)) %>%
    summarise(
      sd_TeWW = sd(TeWW, na.rm = TRUE),
      mean_TeWW = mean(TeWW)
    )
  HI_sum1<-aggregate(TeWW~get(i)+Lon+Lat,HI_d1WW,mean);names(HI_sum1)[1]<-i
  #HI_sum2<-aggregate(TeWW~get(i)+Lon+Lat,HI_d2WW,mean);names(HI_sum2)[1]<-i
  HI.summary_loc <- HI_sum1 %>%
  #HI.summary_loc <- HI_sum2 %>%
    group_by(get(i)) %>%
    summarise(
      sd_TeWW_locat = sd(TeWW, na.rm = TRUE),
      mean_TeWW_locat= mean(TeWW)
    )
  HI.summaryw[[i]]<-join(HI.summary_all,HI.summary_loc);names(HI.summaryw[[i]])[1]<-i
  
  plotfw[[i]]=ggplot(HI.summaryw[[i]]) +
    geom_errorbar(aes(x = mean_TeWW, y = mean_TeWW,
                      ymin = mean_TeWW-sd_TeWW,ymax = mean_TeWW+sd_TeWW,
                      color = as.character(get(i))),width = 0.01,size = 2) +
    geom_errorbarh(aes(y = mean_TeWW, xmin = mean_TeWW_locat-sd_TeWW_locat, xmax = mean_TeWW_locat+sd_TeWW_locat,
                       color = as.character(get(i))),height = 0.01,size = 2,linetype = "longdash") + 
    geom_point(size = 4, aes(x = mean_TeWW, y = mean_TeWW,color = as.character(get(i)))) + theme_light() + ggtitle(i)+
    theme(legend.title = element_blank(),legend.position= c(0.5,0.05),plot.title = element_text(hjust = 0.5), 
          legend.background = element_rect(fill = "white", color = "black"),legend.direction="horizontal")+  
    xlab("Mean with one Std. over locations") + ylab("Mean with one Std. over all simulations")+xlim(0.25,0.75)+ylim(0.25,0.75)
  dev.new(width=15,height=10,unit="in")
  print(plotfw[[i]])
}

