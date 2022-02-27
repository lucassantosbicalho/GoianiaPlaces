# ############################################## #
#                                                #
# Autor: Lucas Bicalho                           #
# Bacharelado em Estatística - UFG - 2022        #
#                                                #
# ##############################################

#rm(list=ls())

library(rgdal)
library(foreign)
library(ggplot2)
library(ggmap)
library(maptools)
library(sp)
library(plotly)
library(sf)
library(rgeos)

setwd("/cloud/project")

source("funcoes.R")
source("dados_shape_goiania.R")
source("obter_grid_points.R")
source("obter_dados_google_places.R")

# Removendo dados da API fora do perimetro de Goiania
spdf <- SpatialPointsDataFrame(coords = dat_unique[, c("lng", "lat")], data = dat_unique,
                               proj4string = CRS("+proj=longlat +datum=WGS84"))
shpGoiania_pol1<-spTransform(shpGoiania_pol, CRS("+proj=longlat +datum=WGS84"))
whatever <- spdf[!is.na(over(spdf, as(shpGoiania_pol1, "SpatialPolygons"))),]
places_goiania <- as.data.frame(whatever)
rm(list=c('spdf', 'shpGoiania_pol1', 'whatever', 'shpGoiania_pol', 'log', 'log_pag', 'fix_df', 'base_url', 'x'))

# GRID - Pontos para chamada da API
ggplot() +
  geom_polygon(data = shpBairrosGoiania, aes( x = long, y = lat, group = group, alpha = 1), size = 0.1, fill="white", color="darkblue") +
  geom_polygon(data = shpGoiania, aes( x = long, y = lat, group = group, alpha=1), size = 0.8, fill="white", color="darkblue") +
  geom_point(aes(x=long, y=lat), data = grid, colour = "black", size = 0.2) + 
  coord_equal() +
  scalebar(shpGoiania, dist = 5, dist_unit = "km",
           transform = TRUE, model = "WGS84", height = 0.015, st.size = 3, border.size = 0.3) +
  theme_bw() +
  xlab("Longitude") +
  ylab("Latitude") +
  guides(fill = guide_legend(label.position = "left", label.hjust = 1)) +
  labs(title = "GRID", 
       subtitle = "Pontos para requisição na API Google Places") +
  theme(legend.position="none")

# Intersecções nos pontos da GRID
ggplot() +
  geom_polygon(data = shpBairrosGoiania, aes( x = long, y = lat, group = group, alpha = 1), size = 0.1, fill="white", color="darkblue") +
  geom_polygon(data = shpGoiania, aes( x = long, y = lat, group = group, alpha=1), size = 0.8, fill="white", color="darkblue") +
  geom_point(aes(x=long, y=lat), data = grid, colour = "black", size = 0.5) + 
  geom_point(aes(x=long, y=lat), data = grid, colour = "black", size = 105, alpha = 0.05) +
  coord_equal() +
  scalebar(shpGoiania, dist = 5, dist_unit = "km",
           transform = TRUE, model = "WGS84", height = 0.015, st.size = 3, border.size = 0.3) +
  theme_bw() +
  xlab("Longitude") +
  ylab("Latitude") +
  guides(fill = guide_legend(label.position = "left", label.hjust = 1)) +
  labs(title = "GRID", 
       subtitle = "Intersecção nos raios de 1,5km em torno dos pontos") +
  theme(legend.position="none") +
  coord_cartesian(ylim=c(-16.706,-16.664), xlim = c(-49.250, -49.205))


# Estabelecimentos coletados pela API distando 1,5km de cada ponto geografico da GRID.
places_goiania %>%
ggplot() +
  geom_polygon(data = shpBairrosGoiania, aes( x = long, y = lat, group = group, alpha = 1), size = 0.1, fill="white", color="darkblue") +
  geom_polygon(data = shpGoiania, aes( x = long, y = lat, group = group, alpha=1), size = 0.8, fill="white", color="darkblue") +
  geom_point(aes(x=lng, y=lat), colour = "black", size = 0.1) + 
  coord_equal() +
  scalebar(shpGoiania, dist = 5, dist_unit = "km",
           transform = TRUE, model = "WGS84", height = 0.015, st.size = 3, border.size = 0.3) +
  theme_bw() +
  xlab("Longitude") +
  ylab("Latitude") +
  guides(fill = guide_legend(label.position = "left", label.hjust = 1)) +
  labs(title = "Estabelecimentos coletados via API Google Places") +
  theme(legend.position="none")

source("cluster.R")

