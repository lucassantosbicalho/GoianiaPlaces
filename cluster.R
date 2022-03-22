# ############################################## #
#                                                #
# Autor: Lucas Bicalho                           #
# Bacharelado em Estatística - UFG - 2022        #
#                                                #
# ##############################################

library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(gridExtra) 
library(xtable)
library(scales)
library(ggsn)

# Removendo duplicados
places_goiania_ %>%
  group_by(lat, lng, place_id, name) %>% 
  filter(user_ratings_total == max(user_ratings_total)) %>%
  select (place_id, name, rating, price_level, user_ratings_total) %>% 
  arrange(place_id) %>%
  unique() -> df0

# Estabelecimentos considerados na análise
df0 %>%
  ggplot() +
  geom_polygon(data = shpBairrosGoiania, aes( x = long, y = lat, group = group, alpha = 1), size = 0.1, fill="white", color="darkblue") +
  geom_polygon(data = shpGoiania, aes( x = long, y = lat, group = group, alpha=1), size = 0.8, fill="white", color="darkblue") +
  geom_point(aes(x=lng, y=lat), colour = "black", size = 0.2) + 
  coord_equal() +
  scalebar(shpGoiania, dist = 5, dist_unit = "km",
           transform = TRUE, model = "WGS84", height = 0.015, st.size = 3, border.size = 0.3) +
  theme_bw() +
  xlab("Longitude") +
  ylab("Latitude") +
  guides(fill = guide_legend(label.position = "left", label.hjust = 1)) +
  labs(title = ("Estabelecimentos coletados via API Google \nPlaces"),
       subtitle = "Quantidade: 1.155") +
  theme(legend.position="none") -> df0_plot

# Análise descritiva
summary(df0)

ggplot() +
  aes(x = df0$rating) +
  geom_histogram() +
  theme_bw() +
  xlab("rating") +
  ylab("Frequência") +
  ggtitle("Variável avaliação") -> h1

ggplot() +
  aes(x = df0$user_ratings_total) +
  geom_histogram() +
  theme_bw() +
  xlab("user_ratings_total") +
  ylab("Frequência") +
  ggtitle("Variável total de avaliações") -> h2

ggplot() +
  aes(x = df0$price_level) +
  geom_histogram() +
  theme_bw() +
  xlab("price_level") +
  ylab("Frequência") +
  ggtitle("Variável nível de preço") -> h3

grid.arrange(h1,
             h2,
             h3,
             nrow = 1) 

plot(df0$price_level, df0$user_ratings_total)
plot(df0$price_level, df0$rating)

# % de NAs por variável
(colMeans(is.na(df0))) -> na_percent 
label_percent()(na_percent) %>% t()
# print(xtable(label_percent()(na_percent) %>% t(), type = "latex"), file = "prints/na_percent.html")

# registros com valor 0 em rating e user_ratings_total
df0 %>% filter(rating == 0) %>% group_by(rating) %>% count(rating)
df0 %>% filter(user_ratings_total == 0) %>% group_by(user_ratings_total) %>% count(user_ratings_total)

# removendo NAs
df <- na.omit(df0)

# Estabelecimentos considerados na análise
df %>%
  ggplot() +
  geom_polygon(data = shpBairrosGoiania, aes( x = long, y = lat, group = group, alpha = 1), size = 0.1, fill="white", color="darkblue") +
  geom_polygon(data = shpGoiania, aes( x = long, y = lat, group = group, alpha=1), size = 0.8, fill="white", color="darkblue") +
  geom_point(aes(x=lng, y=lat), colour = "black", size = 0.2) + 
  coord_equal() +
  scalebar(shpGoiania, dist = 5, dist_unit = "km",
           transform = TRUE, model = "WGS84", height = 0.015, st.size = 3, border.size = 0.3) +
  theme_bw() +
  xlab("Longitude") +
  ylab("Latitude") +
  guides(fill = guide_legend(label.position = "left", label.hjust = 1)) +
  labs(title = ("Estabelecimentos considerados na análise \nde cluster"),
       subtitle = "Quantidade: 509") +
  theme(legend.position="none") -> df_plot

grid.arrange(df0_plot, df_plot, nrow = 1)

# Criando df para cluster
dfcl <- df %>% ungroup() %>% select (rating, price_level, user_ratings_total) %>% as.data.frame()
row.names(dfcl) <- df$place_id

# Colocando dados em escala
dfcls <- scale(dfcl)

# Clustering Distance Measures
distance <- get_dist(dfcls)

# Optimal clusters
set.seed(567)
fviz_nbclust(dfcls, kmeans, method = "wss")

# GAP method
# compute gap statistic
set.seed(567)
gap_stat <- clusGap(dfcls, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
# Print the result
print(gap_stat, method = "firstmax")
fviz_gap_stat(gap_stat)

# export table to latex
# print(xtable(gap_stat$Tab, type = "latex"), file = "prints/gap_statistic_table.html")
# mais clusters
k2 <- kmeans(dfcls, centers = 2, nstart = 25)
k3 <- kmeans(dfcls, centers = 3, nstart = 25)
k4 <- kmeans(dfcls, centers = 4, nstart = 25)
k5 <- kmeans(dfcls, centers = 5, nstart = 25)

# plots to compare
p2 <- fviz_cluster(k2, geom = "point", data = dfcls)  + ggtitle("k = 2")
p3 <- fviz_cluster(k3, geom = "point",  data = dfcls) + ggtitle("k = 3")
p4 <- fviz_cluster(k4, geom = "point",  data = dfcls) + ggtitle("k = 4")
p5 <- fviz_cluster(k5, geom = "point",  data = dfcls) + ggtitle("k = 5")

grid.arrange(p2,
             p3,
             p4,
             p5, nrow = 2)

# Adding the cluster to original data
dfcl$c2 <- as.factor(k2$cluster)
dfcl$c4 <- as.factor(k4$cluster)
df$c2 <- as.factor(k2$cluster)
df$c4 <- as.factor(k4$cluster)

# Plots
# Clusters 4
df %>%
  ggplot() +
  geom_polygon(data = shpBairrosGoiania, aes( x = long, y = lat, group = group, alpha = 1), size = 0.1, fill="white", color="darkblue", show.legend = F) +
  geom_polygon(data = shpGoiania, aes( x = long, y = lat, group = group, alpha=1), size = 0.8, fill="white", color="darkblue", show.legend = F) +
  geom_point(aes(x=lng, y=lat, colour = c4)) + 
  coord_equal() +
  scalebar(shpGoiania, dist = 5, dist_unit = "km",
           transform = TRUE, model = "WGS84", height = 0.015, st.size = 3, border.size = 0.3) +
  theme_bw() +
  xlab("Longitude") +
  ylab("Latitude") +
  guides(fill = guide_legend(label.position = "right", label.hjust = 1)) +
  ggtitle("Visualização dos estabelecimentos por cluster") + 
  scale_colour_brewer(palette = "Set1") +
  theme(
    legend.box.background = element_rect(),
    legend.box.margin = margin(6, 6, 6, 6),
    legend.position = "right",
  ) -> p

update_labels(p, list(colour="Clusters"))

# Valores medios por cluster
df %>% group_by(c4) %>% summarise(mrating = mean(rating), mprice_level = mean(price_level), muser_ratings_total = mean(user_ratings_total),n = n())

              
