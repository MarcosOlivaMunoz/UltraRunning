library(factoextra)
library(cluster)

# Cargamos toda la información de 'dataframe_limpio.R'
source("dataframe_limpio.R")

ultrarunning <- ultrarunning[c(-56),]
ultrarunning_tot <- ultrarunning_tot[c(-56),]

ultrarunning_test <- ultrarunning %>% mutate(., test = percent_rank(teique_sf) + percent_rank(steu_b) + percent_rank(stem_b)) %>% 
  mutate(test = ordered(cut(.$test, 3),labels=c("Bajo","Medio","Alto")))
data <- ultrarunning_test %>% select(1,4,6,7) %>% scale()

# Primero hacemos ACP
data.acp <- prcomp(data)
lambdas <- get_eigenvalue(data.acp); lambdas
fviz_eig(data.acp, addlabels = TRUE, ylim=c(0,100))
fviz_pca_var(data.acp, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)
fviz_pca_biplot(data.acp, repel = TRUE,
                col.var = "#Fc4E07", # color para las variables
                col.ind = "#EEEEEE"  # color para las observaciones
) + theme_dark()

# Haremos k-means
fviz_nbclust(x = data, FUNcluster = kmeans, method = "wss",
             diss = dist(data, method = "euclidean")) +
  geom_vline(xintercept = 3, linetype = 2)
set.seed(2023)
km_clusters <- kmeans(x = data, centers = 3, nstart = 250)
fviz_cluster(object = km_clusters, data = data, show.clust.cent = TRUE,
             ellipse.type = "euclid", star.plot = TRUE, repel = TRUE) +
  theme_bw() +
  theme(legend.position = "none")

# Haremos K-medoids clustering (PAM)
fviz_nbclust(x = data, FUNcluster = pam, method = "wss",
             diss = dist(data, method = "euclidean")) + 
  geom_vline(xintercept = 3, linetype = 2)

set.seed(2023)
pam_clusters = pam(x = data, k = 3, metric = "manhattan")
fviz_cluster(object = pam_clusters, data = data, ellipse.type = "euclid"
             , repel = TRUE) +
  theme_bw() +
  theme(legend.position = "none")

# Representamos los medoides
medoids <- data.acp$x
medoids <- medoids[pam_clusters$id.med, c("PC1", "PC2")]
medoids <- as.data.frame(medoids)
colnames(medoids) <- c("x", "y")
fviz_cluster(object = pam_clusters, data = data, ellipse.type = "euclid", repel = TRUE) +
  theme_bw() +
  geom_point(data = medoids, color = "gold3", size = 2) +
  theme(legend.position = "none")



# Ahora haremos Hierarchical clustering
mat_dist <- dist(x = data, method = "euclidean")
hc_complete <- hclust(d = mat_dist, method = "complete")
hc_average <- hclust(d = mat_dist, method = "average")
cor(x = mat_dist, cophenetic(hc_complete))
cor(x = mat_dist, cophenetic(hc_average))
# Linkage average representa mejor la similitud ente observaciones
hc_average <- data %>% dist(method = "euclidean") %>%
  hclust(method = "average")
fviz_dend(x = hc_average, k = 5, cex = 0.6)
fviz_cluster(object = list(data = data, cluster = cutree(hc_average, k = 5)),
             ellipse.type = "convex",
             repel = TRUE,
             show.clust.cent = FALSE) +
  theme_bw()


# Observemos si aparece algo a mano
# test
fviz_pca_biplot(data.acp, geom = "point", repel = TRUE, col.ind = ultrarunning_test$test,
                palette = c("red", "gold", "darkgreen"), invisible = "var") + theme_dark()
# teique_sf
fviz_pca_biplot(data.acp, geom = "point", repel = TRUE,
                col.ind = ultrarunning_tot$teique_sf, palette = c("red", "gold", "darkgreen"), invisible = "var") + theme_dark()
# steu_b
fviz_pca_biplot(data.acp, geom = "point", repel = TRUE,
                col.ind = ultrarunning_tot$steu_b, palette = c("red", "gold", "darkgreen"), invisible = "var") + theme_dark()
# stem_b
fviz_pca_biplot(data.acp, geom = "point", repel = TRUE,
                col.ind = ultrarunning_tot$stem_b, palette = c("red", "gold", "darkgreen"), invisible = "var") + theme_dark()

matriz = matrix(0, nrow = 3, ncol = 3)
matriz1 = matrix(0, nrow = 3, ncol = 3)
matriz2 = matrix(0, nrow = 3, ncol = 3)
for (i in 1:124){
  matriz[as.numeric(ultrarunning_test[i,11]), as.numeric(ultrarunning_test[i,11])] = 1 + matriz[as.numeric(ultrarunning_test[i,11]), as.numeric(ultrarunning_test[i,11])]
  matriz1[km_clusters$cluster[i], as.numeric(ultrarunning_test[i,11])] = 1 + matriz1[km_clusters$cluster[i], as.numeric(ultrarunning_test[i,11])]
  matriz2[pam_clusters$clustering[i], as.numeric(ultrarunning_test[i,11])] = 1 + matriz2[pam_clusters$clustering[i], as.numeric(ultrarunning_test[i,11])]
}
