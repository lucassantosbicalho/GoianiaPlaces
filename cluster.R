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

# Removendo outlier ChIJCcvH5tnzXpMRz6ktmMrEzf0
places_goiania %>% filter(place_id != "ChIJCcvH5tnzXpMRz6ktmMrEzf0") -> places_goiania_

# Removendo duplicados
places_goiania_ %>%
  group_by(lat, lng, place_id, name) %>% 
  filter(user_ratings_total == max(user_ratings_total)) %>%
  select (place_id, name, rating, price_level, user_ratings_total) %>% 
  arrange(place_id) %>%
  unique() -> df

# Análise descritiva
summary(df)

df %>%
ggplot() +
  aes(x = rating) +
  geom_histogram() +
  theme_bw() +
  xlab("rating") +
  ylab("Frequência") +
  ggtitle("Histograma da variável avaliação") -> h1

df %>%
ggplot() +
  aes(x = user_ratings_total) +
  geom_histogram() +
  theme_bw() +
  xlab("user_ratings_total") +
  ylab("Frequência") +
  ggtitle("Histograma da variável total de avaliações") -> h2

df %>%
ggplot() +
  aes(x = price_level) +
  geom_histogram() +
  theme_bw() +
  xlab("price_level") +
  ylab("Frequência") +
  ggtitle("Histograma da variável nível de preço") -> h3

grid.arrange(h1,
             h2,
             h3,
             nrow = 1)

# % de NAs por variável
(colMeans(is.na(df))) -> na_percent 
# print(xtable(label_percent()(na_percent) %>% t(), type = "latex"), file = "prints/na_percent.html")

# removendo NAs
df <- na.omit(df)

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
# Estabelecimentos considerados na análise
df %>%
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
  labs(title = ("Estabelecimentos considerados na análise de cluster")) +
  theme(legend.position="none")

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

              