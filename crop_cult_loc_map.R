library(ggplot2)
library(rworldmap)
ObsC_<-Yi_HIm[c(1:22),c(10:11,18)]
ObsC_$color=ifelse(ObsC_$Cluster==1,"red","purple")
ObsW$color=ifelse(ObsW$wheat=="Spring","black","blue")
dev.new()
par(mai=c(0.5,0,0.2,0),xaxs="i",yaxs="i")
mapBubbles( dF=getMap(), 
            , oceanCol="lightblue"
            , landCol="wheat" )

points(ObsC_$Lon, ObsC_$Lat, pch =  16, col=ObsC_$color)
points(ObsW$Lon, ObsW$Lat, pch =  17, col=ObsW$color)
legend(x="bottom", legend =c("Maize-High yield", "Maize-Low yield","Wheat-Spring","Wheat-Winter") ,col=c("red","purple","black","blue"),pch=c(16,16,17,17), horiz=T)
title(main="Cultivar location distribution", size =20, cex.main=2,line=-6)
