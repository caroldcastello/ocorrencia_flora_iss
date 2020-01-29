## PLOT FLORA SPECIES DATA OF IIS


### LOAD PACKAGES ####
library(raster)
library(rgdal)
library(ggplot2)
library(cowplot)
library(ggspatial)
library(sp)
library(dplyr)
library(tidyverse)

## READ IN DATA ####

spp <- read.csv("~/Desktop/Explorando_ocorrencias/data/registroslimpos.csv")
head(spp)
spp <- spp[,-1]
head(spp)

bhrd.shp <- shapefile("~/Desktop/Explorando_ocorrencias/shapes/bhrd_sirgas_dissol.shp")

br <- shapefile("~/Desktop/Explorando_ocorrencias/shapes/Brasil.shp")

ma <- shapefile("~/Desktop/Explorando_ocorrencias/shapes/ma_ecorregions_wwf.shp")


## TRANSFORM DATA SPECIES TO SHAPEFILE

spp.shp <- spp
head(spp.shp)

coordinates(spp.shp) <- ~lon+lat

## SAVE THE SPECIES SHAPEFILE

writeOGR(spp.shp, dsn = "/Users/carol.dcastello/Desktop/Explorando_ocorrencias/shapes", layer ='spp.flora.total', driver = 'ESRI Shapefile')

## READ THE SPECIES SHAPEFILE

spp.total <- shapefile("/Users/carol.dcastello/Desktop/Explorando_ocorrencias/shapes/spp.flora.total.shp")


### VERIFY PROJECTIONS ####
crs(spp.total)
crs(bhrd.shp)
crs(ma)
crs(br)

#set and change the projections
crs(spp.total) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

bhrd.shp <- spTransform(bhrd.shp, crs(spp.total))

crs(spp.total)
crs(bhrd.shp)
crs(ma)
crs(br)


### CROP ####

# SPECIES POINTS WITH THE BHRD LIMITS
spp.bhrd <- crop(spp.total, bhrd.shp)

ma.bhrd <- aggregate(ma)
ma.bhrd <- crop(ma, bhrd.shp)


### SAVE THE NEW SHAPEFILE ONLY WITH THE OCCURRENCES WITHIN THE BHRD ###

writeOGR(spp.bhrd, dsn = "/Users/carol.dcastello/Desktop/Explorando_ocorrencias/shapes", layer ='spp.flora.bhrd', driver = 'ESRI Shapefile')


### SAVE A NEW .CSV ONLY WITH THE OCCURRENCES WITHIN THE BHRD ###
write.csv(spp.bhrd, file="/Users/carol.dcastello/Desktop/Explorando_ocorrencias/data/registros.bacia.csv")

### READ THE BHRD SPECIES DATA ####

spp.bhrd.list <- read.csv("/Users/carol.dcastello/Desktop/Explorando_ocorrencias/data/registros.bacia.csv")

spp.bhrd.shp  <- shapefile("/Users/carol.dcastello/Desktop/Explorando_ocorrencias/shapes/spp.flora.bhrd.shp")

## PLOT  ####

# ALL OCCURRENCES
map.geral <- ggplot() + 
  geom_polygon(data = br, aes(x = long, y = lat, group = group), colour = NA, fill = "gray")+
  geom_polygon(data = ma, aes(x = long, y = lat, group = group), colour = NA, fill = "dimgrey")+
  geom_point(data=spp, aes(x=spp$lon, y=spp$lat), size=0.5) +
  xlab("") + ylab("") +
  annotation_scale(location = "bl", width_hint = 0.5, plot_unit ="km") +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.9, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme_bw()

map.geral

## SAVE THE MAP

ggsave("Ocorrencias_total.tif", plot = map.geral, device = "tiff", path = "/Users/carol.dcastello/Desktop/Explorando_ocorrencias/figures")

# ONLY BHRD

map.bhrd <- ggplot() + 
  geom_polygon(data = bhrd.shp, aes(x = long, y = lat, group = group), colour = NA, fill = "gray")+
  geom_polygon(data = ma.bhrd, aes(x = long, y = lat, group = group), colour = NA, fill = "dimgrey")+
  geom_point(data=spp.bhrd.list, aes(x=spp.bhrd.list$coords.x1, y=spp.bhrd.list$coords.x2), size=0.5) +
  xlab("") + ylab("") +
  annotation_scale(location = "br", width_hint = 0.25, plot_unit ="km") +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.2, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme_bw()
  
map.bhrd


## SAVE THE MAP

ggsave("Ocorrencias_bhrd.tif", plot = map.bhrd, device = "tiff", path = "/Users/carol.dcastello/Desktop/Explorando_ocorrencias/figures")


### MANIPULATING THE DATA

#writing a table with a list of the species within the bhrd

head(spp.bhrd.list)

#counting the occurrencies
nrecords <- spp.bhrd.list %>% group_by(FINAL) %>% count

nrecords <- as.data.frame(nrecords)

names(nrecords)[names(nrecords) == "FINAL"] <- "species"
names(nrecords)[names(nrecords) == "n"] <- "n.occurrences"

head(nrecords)

write.csv(nrecords, "/Users/carol.dcastello/Desktop/Explorando_ocorrencias/data/species_list_bhrd.csv")


### EXPLORING OCCURRENCE AND SPECIES NUMBER
head(spp.bhrd.list)

#order occurrences by longitude

spp.bhrd.by.long <- spp.bhrd.list[order(spp.bhrd.list$coords.x1),]

spp.bhrd.by.long <- spp.bhrd.by.long[,-1]
spp.bhrd.by.long <- spp.bhrd.by.long[,-4]

names(spp.bhrd.by.long)[names(spp.bhrd.by.long) == "FINAL"] <- "species"
names(spp.bhrd.by.long)[names(spp.bhrd.by.long) == "coords.x1"] <- "long"
names(spp.bhrd.by.long)[names(spp.bhrd.by.long) == "coords.x2"] <- "lat"

head(spp.bhrd.by.long)
length(spp.bhrd.by.long$species)
#13105

unique.spp.data <- unique(spp.bhrd.by.long$species)

length(unique.spp.data)
#1408

unique.long.data <- unique(spp.bhrd.by.long$long)

length(unique.long.data)
#51