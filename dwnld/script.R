###################################################################
#                  EXERCICE "MISE EN FORME DU CODE"               #
###################################################################

# Visualisation 1 (population 30 km autour du centre)
# Voici mon bloc de code qui importe les données, fait une carte en cercles proportionnels en utilisant le package mapsf. Les valeurs sont ordonnées afin de faire apparaître les cinq premières valeurs sur la carte en tant que label. 
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
## ---- AMUSEZ-VOUS BIEN ! --- ##




###################################################################
#     SI VOUS SOUHAITEZ REPRODUIRE LES AUTRES VISUALISATIONS      #
###################################################################

# Sélection d'un centre de vacances CNRS (choisir "oleron", "frejus", "gerberal", "plantiers" ou "aussois")
centre <- "oleron"

# Import des données 
cnrs <- st_read(dsn = paste0("data/", centre, ".gpkg"), layer = "cnrs",  quiet = TRUE)
buf <- st_read(dsn = paste0("data/", centre, ".gpkg"), layer = "cnrsBuf", quiet = TRUE)
com <- st_read(dsn = paste0("data/", centre, ".gpkg"), layer = "com", quiet = TRUE)
country <- st_read(dsn = paste0("data/", centre, ".gpkg"), layer = "country", quiet = TRUE)
grid <- st_read(dsn = paste0("data/", centre, ".gpkg"), layer = "grid", quiet = TRUE)
osm <- st_read(dsn = paste0("data/", centre, ".gpkg"), layer = "osm", quiet = TRUE)


######### Visualisation 2 (pollution lumineuse 30 km autour du centre) #########
# Discrétisation et palette de couleur (carte et histogramme)
brks <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 1, 1.5, 2, max(grid$World_Atlas_2015, na.rm = TRUE))
pal <- hcl.colors(n = 9, palette = "Lajolla", rev = TRUE)

# Cartographie
mf_map(grid, var = "World_Atlas_2015", type = "choro", 
       breaks = brks, pal = pal,  border = NA,  
       leg_pos = "topleft", leg_no_data = "Valeur maximale",
       leg_title = "Pollution lumineuse en 2015\n(Radiance en nano W/cm²)")

mf_map(grid[which.max(grid$World_Atlas_2015),], col = NA, add = TRUE) # Max
mf_map(country, border = "white", col = NA, add = TRUE) # Front de mer
mf_map(cnrs, add = TRUE, pch = 17, cex = 2, col = "#ffffff90") # centre CNRS
mf_map(buf, col = NA, lwd = 2, add = TRUE) # Périmètre d'analyse

# Histogramme (inset)
mf_inset_on(fig = c(0.75, .99, 0.75, 1))
par(mar = c(0,0,1.7,0))
hist(grid$World_Atlas_2015, breaks = brks, col = pal, border = "white", axes = F, 
     labels = "",  xlab="", ylab = "", main ="")
axis(side = 1, at = round(brks, 2), las = 2, tick = FALSE, line = -.9, 
     cex.axis = .7)
title("Histogramme de répartition", cex.main = .8, font.main = 1, adj = 0)
mf_inset_off()

# Mise en page
mf_layout(title = paste0("Voir les étoiles à ", cnrs$lieu, " (Moyenne = ",
                         round(mean(grid$World_Atlas_2015, na.rm = TRUE), 2), ")"),
          credits = paste0("Ecole Thématique SIG-R, RIATE, 2021\n",
                           "Source : Falchi et al., Supplement to the New World",
                           "Atlas of Artificial Night Sky Brightness. GFZ Data Services"), 
          arrow = FALSE)


############ Visualisation 3a (équipements touristiques OpenStreetMap) ###########
# Dénombrement des équipements
eqSum <- sort(table(osm$value), decreasing = TRUE)

# Gestion des couleurs
cols <- data.frame(pal = c("#f2b200", "#f26500", "#524b46", "#783c00", "#009921"),
                   val = c("museum", "artwork", "monument", "fort", "viewpoint"))
cols <- cols[match(names(eqSum), cols$val),]  

# Cartographie
mf_map(buf, col = "lightblue", border = NA)
mf_map(country, col = "white", border = NA, add = TRUE)

mf_map(x = osm, var = "value", type = "typo", cex = 0.9, border = NA, pch = 18,
       pal = cols$pal, val_order = cols$val,
       leg_pos = "topleft", leg_title = "Points d'intérêt", add = TRUE)

mf_map(cnrs, add = TRUE, pch = 24, cex = 2, col = "black", bg = NA)
mf_map(buf, col = NA, lwd = 2, add = TRUE) # Périmètre d'analyse

# Barplot (inset)
mf_inset_on(fig = c(0.77, .99, 0.75, 1))
par(mar = c(0,0,1.7,0))
x <- barplot(eqSum, col = cols$pal, border = "white", axes = FALSE, axisnames = FALSE)
axis(side = 1, at = x, labels = cols$val, tick = FALSE, line = -.9, cex.axis = .5)
axis(side = 2, at = seq(0,round(max(eqSum), -2),100), las = 2, tick = FALSE, 
     cex.axis = 0.6)
title("Dénombrement des points d'intérêts", cex.main = .8, font.main = 1, adj = 0)
mf_inset_off()

# Mise en page
mf_layout(title = paste0(nrow(osm), " points d'intérêt autour de ", cnrs$lieu),
          credits = paste0("Ecole Thématique SIG-R, RIATE, 2021\n",
                           "Source : OpenStreetMap et Contributeurs, 2021"), 
          arrow = FALSE)


############ Visualisation 3b (densité d'équipements - grille 5 km) ###########
# Création d'une grille de 5000 m
grid <- st_make_grid(x = buf, cellsize = 5000)

# Mise en forme des données
osm$NB <- 1 # Un équipement par ligne
grid <- aggregate(osm["NB"], grid, sum) # Compter le nombre de points OSM par carreau de grille
grid <- st_intersection(grid, buf) # Intersecter avec le buffer
grid <- grid[!is.na(grid$NB),] # Supprimer les carreaux sans équipements

# Cartographie
mf_map(buf, col = "lightblue")
mf_map(country, add = TRUE)

mf_map(grid, var = "NB", type = "choro", border = NA, 
       breaks = c(1, 4, 16, 64, 128, max(grid$NB)), pal = "YlOrRd", 
       leg_title = paste0("Nombre d'équipements OSM en 2021\npar carreaux de grille de 5000m\n",
                          "(Musées, centres d'art, points de vue, forts, monuments)"),
       leg_pos = "topleft", add = TRUE)

mf_map(osm, add = TRUE, pch = 20, col = "#ffffff80", cex = .6)
mf_map(cnrs, add = TRUE, pch = 17, cex = 2, col = "blue")
mf_map(buf, col = NA, lwd = 2, add = TRUE) # Périmètre d'analyse

mf_layout(title = "Où concentrer ses visites ? ",
          credits = paste0("Ecole Thématique SIG-R, RIATE, 2021\n", 
                           "Source : Contributeurs OpenStreetMap, 2021"), 
          arrow = FALSE)
