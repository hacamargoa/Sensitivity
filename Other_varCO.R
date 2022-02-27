library(plyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)

#LCmass creation (dead+alive cmass)
CO4<-CO2[,-c(12:17)]
for (i in 1:5){
  CO4[,i+11]<-CO2[,i+11]+CO3[,i+11]
}
CO4[,17]<-CO2[,17]
names(CO4)[c(12:17)]<-names(CO2)[c(12:17)]
CO2=CO4


#Rangescovariables
setwd("C:/Users/hac809/Desktop/newcrops_sensitivity_old/Bugfix_cond_sens/ret/CO")
VarFiles<-list.files(pattern =".out$")
vars=c("aet","GPP","LCmass","NPP", "C:N_ho", "C:N_Leaf","dLAI", "dLCmass", "LAI")
varnames=c("Sret","SLA","C:Nmin", "C:Nran","N-ret","C-ret","kN","N_red")
cu=0
for (k in VarFiles){
COa<-read.table(k,h=T);COa$Sret=1
COb<-read.table(paste0("../../Nort/CO/",k),h=T);COb$Sret=0
CO<-rbind(COa,COb);
CO<-CO %>% relocate(Sret, .before = SLA)
CO2<-join(CO,yield_av[,c(1,2,4)])
cu=cu+1
for (j in c("Medium","High")){
  #range bars Maize
  plotf<-list()
  co.summary<-list()
  CO_2<-subset(CO2,Group==j)
  sdev=sd(CO_2$TeCSi)
  me=mean(CO_2$TeCSi)
  counter = 0
  for (i in colnames(CO_2)[2:9]){
    CO_3<-CO_2[,c(i,"Lon","Lat","TeCSi")];names(CO_3)[1]<-"Var"
    co.summary_all<- CO_3 %>%
      group_by(Var) %>%
      summarise(
        sd_TeCSi = sd(TeCSi, na.rm = TRUE),
        mean_TeCSi = mean(TeCSi)
      )
    
    sum2<-aggregate(TeCSi~Sret+SLA+CN_min+CN_range+senNrate+senCrate+k_N+N_dem_re,CO_2,mean)
    sum2<-sum2[,c(i,"TeCSi")];names(sum2)[1]<-"Var"
    co.summary_loc <- sum2 %>%
      group_by(Var) %>%
      summarise(
        sd_TeCSi_locat = sd(TeCSi, na.rm = TRUE),
        mean_TeCSi_locat= mean(TeCSi)
      )
    co.summary[[i]]<-join(co.summary_all,co.summary_loc)
    counter=counter+1
    plotf[[i]]=ggplot(co.summary[[i]]) +
      geom_errorbar(aes(x = mean_TeCSi, y = mean_TeCSi,
                        ymin = mean_TeCSi-sd_TeCSi,ymax = mean_TeCSi+sd_TeCSi,
                        color = as.character(Var)),width = co.summary_all$sd_TeCSi/5,size = 1) +
      geom_errorbarh(aes(y = mean_TeCSi, xmin = mean_TeCSi_locat-sd_TeCSi_locat, xmax = mean_TeCSi_locat+sd_TeCSi_locat,
                         color = as.character(Var)),height = co.summary_loc$sd_TeCSi_locat/5,size = 1,linetype = "longdash") +
      geom_point(size = 4, aes(x = mean_TeCSi, y = mean_TeCSi,color = as.character(Var))) + theme_light() + ggtitle(paste(varnames[counter],vars[cu],j))+
      theme(legend.title = element_blank(),legend.position= c(0.5,0.05),plot.title = element_text(hjust = 0.5),
            legend.background = element_rect(fill = "white", color = "black"),legend.direction="horizontal")+
      xlab("Mean with one Std. without locations") + ylab("Mean with one Std. over all simulations") + xlim(me-2*sdev,me+2*sdev)+ylim(me-2*sdev,me+2*sdev)
  }
  dev.new()
  do.call("grid.arrange",c(plotf, nrow = 2))
  dev.copy2pdf(file = paste0("C:/Users/hac809/Desktop/newcrops_sensitivity_old/Bugfix_cond_sens/",k,j,"irrM.pdf"),width=16,height=9)
}

}


#identifying low senescense simulations
dlaiCOa<-read.table("dlaiCO.out",h=T);dlaiCOa$Sret=1
dlaiCOb<-read.table("dlaiCOnort.out",h=T);dlaiCOb$Sret=0
dlaiCO<-rbind(dlaiCOa,dlaiCOb);
dlaiCO<-dlaiCO %>% relocate(Sret, .before = SLA)
dlaiCO_av<-aggregate(TeCSi~Sret+SLA+CN_min+CN_range+senNrate+senCrate+k_N+N_dem_re,dlaiCO,mean);names(dlaiCO_av)[9]<-"lai_TeCSi"

dcmassCOa<-read.table("dcmassCO.out",h=T);dcmassCOa$Sret=1
dcmassCOb<-read.table("dcmassCOnort.out",h=T);dcmassCOb$Sret=0
dcmassCO<-rbind(dcmassCOa,dcmassCOb);
dcmassCO<-dcmassCO %>% relocate(Sret, .before = SLA)
dcmassCO_av<-aggregate(TeCSi~Sret+SLA+CN_min+CN_range+senNrate+senCrate+k_N+N_dem_re,dcmassCO,mean);names(dcmassCO_av)[9]<-"cmass_TeCSi"
dleafCO<-join(dlaiCO_av,dcmassCO_av)
cor.test(dleafCO$lai_TeCSi,dleafCO$cmass_TeCSi)
dleafCO$sen=ifelse(dleafCO$lai_TeCSi<quantile(dleafCO$lai_TeCSi,1/50),0,1)

#anpp
anppCOa<-read.table("anppCO.out",h=T);anppCOa$Sret=1
anppCOb<-read.table("anppCOnort.out",h=T);anppCOb$Sret=0
anppCO<-rbind(anppCOa,anppCOb);
anppCO<-anppCO %>% relocate(Sret, .before = SLA)
anppCO_av<-aggregate(TeCSi~Sret+SLA+CN_min+CN_range+senNrate+senCrate+k_N+N_dem_re,anppCO,mean);names(anppCO_av)[9]<-"npp_TeCSi"

#npool
npoolCOa<-read.table("npoolCO.out",h=T);npoolCOa$Sret=1
npoolCOb<-read.table("npoolCOnort.out",h=T);npoolCOb$Sret=0
npoolCO<-rbind(npoolCOa,npoolCOb);
npoolCO<-npoolCO %>% relocate(Sret, .before = SLA)
npoolCO<-join(npoolCO,yield_av[,c(1,2,4)])
npoolCO_av<-aggregate(SoilN~Group,npoolCO,mean);names(npoolCO_av)[2]="mean"
npoolCO_av2<-aggregate(SoilN~Group,npoolCO,sd);names(npoolCO_av2)[2]="sd"
npoolCO_av<-join(npoolCO_av,npoolCO_av2)
