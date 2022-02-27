# ############################################## #
#                                                #
# Autor: Lucas Bicalho                           #
# Bacharelado em Estatística - UFG - 2022        #
#                                                #
# ##############################################

grid <- grid_geopoints(qtd = 14) 

spdf <- SpatialPointsDataFrame(coords = grid[, c("long", "lat")], data = grid,
                               proj4string = CRS("+proj=longlat +datum=WGS84"))

shpGoiania_pol1<-spTransform(shpGoiania_pol, CRS("+proj=longlat +datum=WGS84"))

whatever <- spdf[!is.na(over(spdf, as(shpGoiania_pol1, "SpatialPolygons"))),]
                 
grid <- as.data.frame(whatever)

rm(list=c('spdf', 'shpGoiania_pol1', 'shpGoiania_raster', 'whatever'))
