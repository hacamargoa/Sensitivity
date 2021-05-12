library(plyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)

yield_d1CO<-read.table("yield_d1.out",h=T)
yield_d2CO<-read.table("yield_d2.out",h=T)

yield_av<-aggregate(TeCS~Lon+Lat,yield_d2CO,mean);
yield_av0<-aggregate(TeCS~Lon+Lat,yield_d1CO,mean);
yield_av1<-join(yield_av,yield_av0,by=c("Lon","Lat"))
yield_av$Group<-ifelse(yield_av$TeCS<0.4,"Low",ifelse(yield_av$TeCS>0.6,"High","Medium"))

yield_d1CO2<-join(yield_d1CO,yield_av[,c(1,2,4)])
yield_d2CO2<-join(yield_d1CO,yield_av[,c(1,2,4)])

for (j in c("Low","Medium","High")){
  #range bars Maize
  plotf<-list()
  yield.summary<-list()
  #yield_d1CO_2<-subset(yield_d1CO2,Group==j)
  yield_d2CO_2<-subset(yield_d2CO2,Group==j)
  sdev=sd(yield_d2CO_2$TeCS)
  me=mean(yield_d2CO_2$TeCS)
  #for (i in names(yield_d1CO_2)[2:8]){
  for (i in colnames(yield_d2CO_2)[2:8]){
    yield_d2CO_3<-yield_d2CO_2[,c(i,"Lon","Lat","TeCS")];names(yield_d2CO_3)[1]<-"Var"
    #yield.summary_all<- yield_d1CO_2 %>%
    yield.summary_all<- yield_d2CO_3 %>%
      group_by(Var) %>%
      summarise(
        sd_TeCS = sd(TeCS, na.rm = TRUE),
        mean_TeCS = mean(TeCS)
      )
    
    #yield_sum1<-aggregate(TeCS~get(i)+Lon+Lat,yield_d1CO_2,mean);names(yield_sum1)[1]<-i
    yield_sum2<-aggregate(TeCS~Var+Lon+Lat,yield_d2CO_3,mean)
    #yield_sum2<-aggregate(TeCS~SLA+CN_min+CN_range+senNrate+senCrate+k_N+N_dem_re,yield_d2CO,mean);
    #yield.summary_loc <- yield_sum1 %>%
    yield.summary_loc <- yield_sum2 %>%
      group_by(Var) %>%
      summarise(
        sd_TeCS_locat = sd(TeCS, na.rm = TRUE),
        mean_TeCS_locat= mean(TeCS)
      )
    yield.summary[[i]]<-join(yield.summary_all,yield.summary_loc)
    
    plotf[[i]]=ggplot(yield.summary[[i]]) +
      geom_errorbar(aes(x = mean_TeCS, y = mean_TeCS,
                        ymin = mean_TeCS-sd_TeCS,ymax = mean_TeCS+sd_TeCS,
                        color = as.character(Var)),width = 0.01,size = 2) +
      geom_errorbarh(aes(y = mean_TeCS, xmin = mean_TeCS_locat-sd_TeCS_locat, xmax = mean_TeCS_locat+sd_TeCS_locat,
                         color = as.character(Var)),height = 0.01,size = 2,linetype = "longdash") +
      geom_point(size = 4, aes(x = mean_TeCS, y = mean_TeCS,color = as.character(Var))) + theme_light() + ggtitle(paste(i,j))+
      theme(legend.title = element_blank(),legend.position= c(0.5,0.05),plot.title = element_text(hjust = 0.5),
            legend.background = element_rect(fill = "white", color = "black"),legend.direction="horizontal")+
      xlab("Mean with one Std. over locations") + ylab("Mean with one Std. over all simulations") + xlim(me-sdev-0.06,me+sdev+0.11)+ylim(me-sdev-0.06,me+sdev+0.11)
  }
  dev.new()
  do.call("grid.arrange",c(plotf, nrow = 2))
}

#range bars Wheat
yield_d1WW<-read.table("yield_d1WW.out",h=T)
yield_d2WW<-read.table("yield_d2WW.out",h=T)

#range bars
plotfw<-list()
yield.summaryw<-list()
for (i in names(yield_d1WW)[2:8]){
#for (i in names(yield_d2WW)[2:8]){
  yield.summary_all<- yield_d1WW %>%
  #yield.summary_all<- yield_d2WW %>%
    group_by(get(i)) %>%
    summarise(
      sd_TeWW = sd(TeWW, na.rm = TRUE),
      mean_TeWW = mean(TeWW)
    )
  yield_sum1<-aggregate(TeWW~get(i)+Lon+Lat,yield_d1WW,mean);names(yield_sum1)[1]<-i
  #yield_sum2<-aggregate(TeWW~get(i)+Lon+Lat,yield_d2WW,mean);names(yield_sum2)[1]<-i
  yield.summary_loc <- yield_sum1 %>%
  #yield.summary_loc <- yield_sum2 %>%
    group_by(get(i)) %>%
    summarise(
      sd_TeWW_locat = sd(TeWW, na.rm = TRUE),
      mean_TeWW_locat= mean(TeWW)
    )
  yield.summaryw[[i]]<-join(yield.summary_all,yield.summary_loc);names(yield.summaryw[[i]])[1]<-i
  
  plotfw[[i]]=ggplot(yield.summaryw[[i]]) +
    geom_errorbar(aes(x = mean_TeWW, y = mean_TeWW,
                      ymin = mean_TeWW-sd_TeWW,ymax = mean_TeWW+sd_TeWW,
                      color = as.character(get(i))),width = 0.01,size = 2) +
    geom_errorbarh(aes(y = mean_TeWW, xmin = mean_TeWW_locat-sd_TeWW_locat, xmax = mean_TeWW_locat+sd_TeWW_locat,
                       color = as.character(get(i))),height = 0.01,size = 2,linetype = "longdash") + 
    geom_point(size = 4, aes(x = mean_TeWW, y = mean_TeWW,color = as.character(get(i)))) + theme_light() + ggtitle(i)+
    theme(legend.title = element_blank(),legend.position= c(0.5,0.05),plot.title = element_text(hjust = 0.5), 
          legend.background = element_rect(fill = "white", color = "black"),legend.direction="horizontal")+  
    xlab("Mean with one Std. over locations") + ylab("Mean with one Std. over all simulations")+xlim(0.05,0.45)+ylim(0.05,0.45)
  dev.new(width=15,height=10,unit="in")
  print(plotfw[[i]])
}

