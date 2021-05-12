library(data.table)
library(DescTools)
library(stringi)
#Maize
par(mfrow=c(3,1))
plotH<-list()
for (j in c("Low","Medium","High")){
HI_d2CO_2<-subset(HI_d2CO2,Group==j)
sisnmodel<-aov(TeCS ~ SLA*CN_min*CN_range*senNrate*senCrate*k_N*N_dem_re,HI_d2CO_2)
ANOVA<-summary (sisnmodel)
AOVdf<-as.data.frame(ANOVA[[1]])
setDT(AOVdf,keep.rownames = TRUE);names(AOVdf)[1]<-"effects"
AOVdf$effects<-stri_replace_all_fixed(AOVdf$effects," ", "")
total<-sum(AOVdf$`Sum Sq`[-length(AOVdf$`Sum Sq`)])
SSperc<-data.frame(effects=names(HI_d2CO)[2:8])
for (i in names(HI_d2CO_2)[2:8]){
  temp<- subset(AOVdf,effects==i)
  temp2<-AOVdf[-c(1:7),]
  temp3<-temp2[temp2$effects %like% paste0("%",i,"%"), ]
  SSperc[SSperc$effects==i,2]<-(temp$`Sum Sq`/total)*100
  SSperc[SSperc$effects==i,3]<-(sum(temp3$`Sum Sq`)/total)*100
  names(SSperc)[c(2,3)]<-c("simple","interaction")
}

test<- melt(SSperc, id.vars='effects')
plotH[[j]]<-ggplot(test, aes(x=value, y=effects, fill=variable)) + geom_bar(stat = "identity", position="dodge")+scale_fill_hue(c = 40)+
  theme(legend.title = element_blank(),legend.position= ifelse(j=="High","bottom","none"),legend.background = element_rect(fill = "white", color = "black"),plot.title = element_text(hjust = 0.5))+
  xlab("%SS") + ylab("Parameter")+ggtitle(j)
}
dev.new()
do.call("grid.arrange",c(plotH, nrow = 3))


#Wheat
siswnmodel<-aov(TeWW ~ SLA*CN_min*CN_range*senNrate*senCrate*k_N*N_dem_re,HI_d2WW)
ANOVA<-summary (siswnmodel)
AOVdf<-as.data.frame(ANOVA[[1]])
setDT(AOVdf,keep.rownames = TRUE);names(AOVdf)[1]<-"effects"
AOVdf$effects<-stri_replace_all_fixed(AOVdf$effects," ", "")
total<-sum(AOVdf$`Sum Sq`[-length(AOVdf$`Sum Sq`)])
SSperc<-data.frame(effects=names(HI_d2WW)[2:8])
for (i in names(HI_d2WW)[2:8]){
  temp<- subset(AOVdf,effects==i)
  temp2<-AOVdf[-c(1:7),]
  temp3<-temp2[temp2$effects %like% paste0("%",i,"%"), ]
  SSperc[SSperc$effects==i,2]<-(temp$`Sum Sq`/total)*100
  SSperc[SSperc$effects==i,3]<-(sum(temp3$`Sum Sq`)/total)*100
  names(SSperc)[c(2,3)]<-c("simple","interaction")
}

test<- melt(SSperc, id.vars='effects')
dev.new()
ggplot(test, aes(x=value, y=effects, fill=variable)) + geom_bar(stat = "identity", position="dodge")+scale_fill_hue(c = 40)+
  theme(legend.title = element_blank(),legend.position= "bottom",legend.background = element_rect(fill = "white", color = "black"))+
  xlab("%SS") + ylab("Parameter")        

