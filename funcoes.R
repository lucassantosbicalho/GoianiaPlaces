# ############################################## #
#                                                #
# Autor: Lucas Bicalho                           #
# Bacharelado em Estatística - UFG - 2022        #
#                                                #
# ##############################################

fix_df <- function(df, columns){
  check <- columns %in% names(df)
  for(c in 1:length(columns)){
    if(!check[c]){
      df[,columns[c]] <- NA
    }
  }
  return(df)
}

grid_geopoints <- function (qtd){
  
  library(dplyr)
  
  lat_points_1500m <- function(lat_init, long_init, qtd, lat_unit){
    
    x1 = c()
    y1 = c()
    x2 = c()
    y2 = c()
    
    lat_temp1 = lat_init
    lat_temp2 = lat_init
    long_temp = long_init
    
    for(x in 1:qtd){
      x1[x] = lat_temp1 - lat_unit
      y1[x] = long_temp
      lat_temp1 = lat_temp1 - lat_unit
      x2[x] = lat_temp2 + lat_unit
      y2[x] = long_temp
      lat_temp2 = lat_temp2 + lat_unit
    }
    
    x = c(lat_init,x1,x2)
    y = c(long_init,y1,y2)
    
    return(data.frame(x,y))
  }
  
  
  # a partir do centroid do poligono de Goiania vamos criar grade coordenadas geograficas, distantes 1,5 km entre pontos
  # iterando na latitude 1500m
  lat_init = centroidGoiania@coords[2]
  lat_unit = 0.01355
  # iterando na longitude 1500m
  long_init = centroidGoiania@coords[1]
  long_unit = 0.01406
  
  long1_ = long_init
  long2_ = long_init
  a = list()
  b = list()
  c = list()
  c[[1]] = lat_points_1500m(lat_init = lat_init, long_init = long_init, qtd = qtd, lat_unit = lat_unit)
  for(x in 1:qtd){
    long1_ = long1_ + long_unit
    a[[x]] = lat_points_1500m(lat_init = lat_init, long_init = long1_, qtd = qtd, lat_unit = lat_unit)
    long2_ = long2_ - long_unit
    b[[x]] = lat_points_1500m(lat_init = lat_init, long_init = long2_, qtd = qtd, lat_unit = lat_unit)
  }
  
  bind_rows(a,b,c) %>% as.data.frame() -> points
  names(points) <- c("lat", "long")
  return(points)
}
