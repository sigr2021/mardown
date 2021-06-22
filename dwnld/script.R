# Voici mon bloc de code qui fait une carte en cercles proportionnels en utilisant le package mapsf. Les valeurs sont ordonnées afin de faire apparaître les cinq premières valeurs sur la carte en tant que label. 
library(sf); library(mapsf); library(dplyr)
communespopulation2019<-st_read(dsn="data/oleron.gpkg",layer="com",quiet=TRUE)
communes30kmautour<-st_read(dsn="data/oleron.gpkg",layer="cnrsBuf",quiet=TRUE)
mf_map(communes30kmautour,col="lightblue",border=NA);mf_map(communespopulation2019,col="white",add=TRUE)
mf_map(communespopulation2019,type="prop",var="POP_2019",leg_pos="topleft",col="grey34",border="white",leg_title="Population 2019")
communespopulation2019<-arrange(communespopulation2019,desc(POP_2019))
mf_label(x=communespopulation2019[1:5,],var="LAU_NAME",halo=TRUE,overlap=FALSE,lines=FALSE,bg="white")
mf_map(communes30kmautour,col=NA,lwd=2,add=TRUE)
mf_layout(title=paste0(round(sum(communespopulation2019$POP_2019),-2)," habitants dans les bourgs des environs"),credits="Ecole Thématique SIG-R, RIATE,2021\n,Source : Eurostat - GISCO, 2021",arrow=FALSE)

## ---- NB : Ce code est volontairement (très) moche pour l'exercice --- ##
## ---- NB2 : Remplacer oleron, par "frejus", "gerberal", ou "plantiers" pour modifier le centre CNRS --- ##
## ---- AMUSEZ-VOUS BIEN ! --- ##