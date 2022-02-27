# ############################################## #
#                                                #
# Autor: Lucas Bicalho                           #
# Bacharelado em Estatística - UFG - 2022        #
#                                                #
# ##############################################

# Os arquivos shape dos bairros do município de Goiânia e do Município de Goiânia foram obtidos por solicitação à SEPLAN-GO em 2014.
# De acordo com a LAI - Lei de Acesso à Informação.

library(rgdal)
library(foreign)
library(ggplot2)
library(ggmap)
library(maptools)
library(sp)
library(plotly)
library(sf)
library(rgeos)

#Shapefiles
setwd("shapes/")
shpBairrosGoiania = readOGR(".", "Goiania")
# 
# #Georreferenciando shapes
shpBairrosGoiania = spTransform(shpBairrosGoiania, CRS("+proj=longlat +datum=WGS84"))
shpBairrosGoiania = fortify(shpBairrosGoiania)
shpGO = readOGR(".", "GO_Municipios_2020")

shpGoiania_pol <- shpGO[shpGO$NM_MUN == "Goiânia",]
centroidGoiania = gCentroid(shpGoiania_pol,byid=TRUE)
# #Georreferenciando shapes
shpGoiania = spTransform(shpGoiania_pol, CRS("+proj=longlat +datum=WGS84"))
shpGoiania = fortify(shpGoiania)

rm(shpGO)

shpGO_raster = raster::shapefile("GO_Municipios_2020.shp")
shpGoiania_raster <- shpGO_raster[shpGO_raster$NM_MUN == "Goiânia",]
rm(shpGO_raster)

setwd("/cloud/project")


# https://r-spatial.github.io/sf/articles/sf3.html


