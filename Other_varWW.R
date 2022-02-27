library(plyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(grid)

#LCmass creation (dead+alive cmass)
WW4=WW3
WW5<-WW3[,-c(19:20)]
WW5[,19]<-WW3[,19]+WW4[,19]
WW5[,20]<-WW3[,20]
names(WW5)[c(19:20)]<-names(WW3)[c(19:20)]
WW3=WW5


#Rangescovariables
setwd("C:/Users/hac809/Desktop/newcrops_sensitivity_old/Bugfix_cond_sens/ret/WW")
VarFiles<-list.files(pattern =".out$")
varnames=c("Sret","SLA","C:Nmin", "C:Nran","N-ret","C-ret","kN","N_red")
cu=0
for (k in VarFiles){
  WWa<-read.table(k,h=T);WWa$Sret=1
  WWb<-read.table(paste0("../../Nort/WW/",k),h=T);WWb$Sret=0
  WW<-rbind(WWa,WWb);
  WW<-WW[-c(9:15)]
  WW<-WW %>% relocate(Sret, .before = SLA)
  WW2<-join(WW,coordw)
  WW2$TeWi<-ifelse(WW2$wheat=="Spring",WW2$TeSWi,WW2$TeWWi)
  WW3<-join(WW2,yield_avw[,c(1,2,4)])
  varnames=c("Sret","SLA","C:Nmin", "C:Nran","N ret","C ret","kN","N_red")
  cu=cu+1
  for (j in c("Medium","High")){
    #range bars Maize
    plotf<-list()
    WW.summary<-list()
    WW_2<-subset(WW3,Group==j)
    sdev=sd(WW_2$TeWi)
    me=mean(WW_2$TeWi)
    counter = 0
    for (i in colnames(WW_2)[2:9]){
      WW_3<-WW_2[,c(i,"Lon","Lat","TeWi")];names(WW_3)[1]<-"Var"
      WW.summary_all<- WW_3 %>%
        group_by(Var) %>%
        summarise(
          sd_TeWi = sd(TeWi, na.rm = TRUE),
          mean_TeWi = mean(TeWi)
        )
      
      sum2<-aggregate(TeWi~Sret+SLA+CN_min+CN_range+senNrate+senCrate+k_N+N_dem_re,WW_2,mean)
      sum2<-sum2[,c(i,"TeWi")];names(sum2)[1]<-"Var"
      WW.summary_loc <- sum2 %>%
        group_by(Var) %>%
        summarise(
          sd_TeWi_locat = sd(TeWi, na.rm = TRUE),
          mean_TeWi_locat= mean(TeWi)
        )
      WW.summary[[i]]<-join(WW.summary_all,WW.summary_loc)
      counter=counter+1
      plotf[[i]]=ggplot(WW.summary[[i]]) +
        geom_errorbar(aes(x = mean_TeWi, y = mean_TeWi,
                          ymin = mean_TeWi-sd_TeWi,ymax = mean_TeWi+sd_TeWi,
                          color = as.character(Var)),width = WW.summary_all$sd_TeWi/5,size = 1) +
        geom_errorbarh(aes(y = mean_TeWi, xmin = mean_TeWi_locat-sd_TeWi_locat, xmax = mean_TeWi_locat+sd_TeWi_locat,
                           color = as.character(Var)),height = WW.summary_loc$sd_TeWi_locat/5,size = 1,linetype = "longdash") +
        geom_point(size = 4, aes(x = mean_TeWi, y = mean_TeWi,color = as.character(Var))) + theme_light() + ggtitle(paste(varnames[counter],vars[cu],j))+
        theme(legend.title = element_blank(),legend.position= c(0.5,0.05),plot.title = element_text(hjust = 0.5),
              legend.background = element_rect(fill = "white", color = "black"),legend.direction="horizontal")+
        xlab("Mean with one Std. without locations") + ylab("Mean with one Std. over all simulations") + xlim(me-2*sdev,me+2.1*sdev)+ylim(me-2*sdev,me+2.1*sdev)
    }
    dev.new()
    do.call("grid.arrange",c(plotf, nrow = 2))
    dev.copy2pdf(file = paste0("C:/Users/hac809/Desktop/newcrops_sensitivity_old/Bugfix_cond_sens/",k,j,".pdf"),width=16,height=9)
  }
  
}



#identifying low senescense simulations
dlaiWWa<-read.table("dlaiWW.out",h=T);dlaiWWa$Sret=1
dlaiWWb<-read.table("dlaiWWnort.out",h=T);dlaiWWb$Sret=0
dlaiWW<-rbind(dlaiWWa,dlaiWWb);
dlaiWW<-dlaiWW %>% relocate(Sret, .before = SLA)
dlaiWW_av<-aggregate(TeSWi~Sret+SLA+CN_min+CN_range+senNrate+senCrate+k_N+N_dem_re,dlaiWW,mean);names(dlaiWW_av)[9]<-"lai_TeSWi"

dcmassWWa<-read.table("dcmassWW.out",h=T);dcmassWWa$Sret=1
dcmassWWb<-read.table("dcmassWWnort.out",h=T);dcmassWWb$Sret=0
dcmassWW<-rbind(dcmassWWa,dcmassWWb);
dcmassWW<-dcmassWW %>% relocate(Sret, .before = SLA)
dcmassWW_av<-aggregate(TeSWi~Sret+SLA+CN_min+CN_range+senNrate+senCrate+k_N+N_dem_re,dcmassWW,mean);names(dcmassWW_av)[9]<-"cmass_TeSWi"
dleafWW<-join(dlaiWW_av,dcmassWW_av)
WWr.test(dleafWW$lai_TeSWi,dleafWW$cmass_TeSWi)
dleafWW$sen=ifelse(dleafWW$lai_TeSWi<quantile(dleafWW$lai_TeSWi,1/50),0,1)

#anpp
anppWWa<-read.table("anppWW.out",h=T);anppWWa$Sret=1
anppWWb<-read.table("anppWWnort.out",h=T);anppWWb$Sret=0
anppWW<-rbind(anppWWa,anppWWb);
anppWW<-anppWW %>% relocate(Sret, .before = SLA)
anppWW_av<-aggregate(TeSWi~Sret+SLA+CN_min+CN_range+senNrate+senCrate+k_N+N_dem_re,anppWW,mean);names(anppWW_av)[9]<-"npp_TeSWi"

#npool
npoolWWa<-read.table("npoolWW.out",h=T);npoolWWa$Sret=1
npoolWWb<-read.table("npoolWWnort.out",h=T);npoolWWb$Sret=0
npoolWW<-rbind(npoolWWa,npoolWWb);
npoolWW<-npoolWW %>% relocate(Sret, .before = SLA)
npoolWW<-join(npoolWW,yield_avw[,c(1,2,4)])
npoolWW_av<-aggregate(SoilN~Group,npoolWW,mean);names(npoolWW_av)[2]="mean"
npoolWW_av2<-aggregate(SoilN~Group,npoolWW,sd);names(npoolWW_av2)[2]="sd"
npoolWW_av<-join(npoolWW_av,npoolWW_av2)
