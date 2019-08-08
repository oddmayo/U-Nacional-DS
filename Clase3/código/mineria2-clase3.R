# Instalar paquetes
install.packages(c("tidyverse","tm","FactoMineR","ClusterR","cluster","Rtsne","NbClust","factoextra"))

# Cargar paquetes
paquetes <- c("tidyverse","tm","FactoMineR","ClusterR","cluster","Rtsne","NbClust","factoextra")

lapply(paquetes, require, character.only = TRUE)


# Cargar texto_limpio
load("texto_limpio.RData")

####################
# MACHINE LEARNING # Aprendizaje no supervisado
####################

#--------------#
# Bag of Words #
#--------------#

# Bolsa de palabras - Matriz de términos y 'documentos'
dtm <- Corpus(VectorSource(texto_limpio))
# TF-IDF, relevancia de un término en un documento
dtm <- weightTfIdf(DocumentTermMatrix(dtm))
dtm <- as.matrix(dtm)

#------------#
# Normalizar #
#------------#

# Usar distancia euclideana
norm_eucl <- function(m) m/apply(m, 1 , function(x) sum(x^2)^.5)
BoW <- norm_eucl(dtm)
BoW <- BoW[complete.cases(BoW), ]

# Matriz de frecuencias relativas
# BoW <- t(apply(dtm, 1, function(x) x/sum(x)))
# BoW <- BoW[complete.cases(BoW), ]

#------------------------------#
# Reducción de dimensionalidad #
#------------------------------#

# PCA gráfico con FactoMiner
dev.off()
PCA(BoW,graph = T)

# PCA con matriz cuadrada
BoW.pca <- prcomp(BoW)


# Codo para número óptimo de clusters - ClusterR
dev.off()
opt = Optimal_Clusters_KMeans(BoW.pca$x[,1:5], max_clusters = 10, plot_clusters = T, criterion = 'variance_explained', num_init = 1, initializer = 'optimal_init')


# Incrustación estocástica de vecinos t-distribuido / Manifold - Rtsne
perp <- round((5*ncol(BoW.pca$x))/100)
tiesne <- Rtsne(X = BoW.pca$x[,1:5], dims = 2, perplexity = perp, theta = 0.5, check_duplicates = F, pca = F, partial_pca = F, max_iter = 1000, verbose = T)


# Definir número de clusters para k-medias
n.clusters <- 6

#------------------------------#
# Agrupamiento / Clasificación #
#------------------------------#

# K-medias
kfit <- kmeans(tiesne$Y, n.clusters)
dev.off()
# Graficar PCA
plot(BoW.pca$x[,1:2], col = kfit$cluster)
# Constrastar con TSNE
plot(tiesne$Y, col = kfit$cluster)
# Graficar centros
points(kfit$centers, col = 1:5, pch = 2, cex = 2)



# Gráfico básico - cluster
clusplot(tiesne$Y, kfit$cluster, color=TRUE, shade=TRUE, 
         labels=2, lines=0)

# Gráfico más presentable y sencillo - factoextra
fviz_cluster(kfit, data = as.data.frame(tiesne$Y), stand = FALSE,
             ellipse = T, show.clust.cent = T,
             geom = "point",palette = "jco", ggtheme = theme_classic())


# Gráfico con etiquetas
fviz_cluster(kfit,as.data.frame(tiesne$Y),ellipse.type = 'norm') + 
  scale_color_brewer(palette = "Set2")+
  scale_fill_brewer(palette = "Set2") +
  theme_minimal()

# Paleta de colores personalizada
paleta <- c('#000075','dimgrey','red','#9A6324','navajowhite3','#911eb4','green4','deepskyblue','firebrick3')
paleta <- c('#000075','dimgrey','red', '#9A6324','navajowhite3','#911eb4')

# Gráfico final
fviz_cluster(object = kfit, data = as.data.frame(tiesne$Y), show.clust.cent = TRUE,
             ellipse.type = "norm", star.plot = TRUE, repel = T) +
  scale_color_manual(values = paleta)+
  scale_fill_manual(values = paleta) +
  labs(title = "Resultados clustering K-means") +
  theme_bw() +
  theme_void()

#------------#
# Resultados #
#------------#

pag_cluster <- data.frame(pag = texto_limpio[which(texto_limpio != "NA")], cluster = kfit$cluster)
# Ordenar
pag_cluster <- pag_cluster[order(pag_cluster$cluster, decreasing = TRUE),]

# Agrupar por cluster
resultado <- pag_cluster %>% group_by(cluster) %>% summarise(paste(pag, collapse = " "))

# Función para contar número de palabras por fila
freqfunc <- function(x, n){
  tail(sort(table(unlist(strsplit(as.character(x), " ")))), n)
}


# Guardar cada cluster de palabras en una lista
lista <- c()
for (i in 1:nrow(resultado)) {
  lista[[i]] <- freqfunc(pag_cluster$pag[i], 10)
}

lista[1]
lista[2]
lista[3]
lista[4]
lista[5]
lista[6]

# Sacar las palabras en un data frame
lista_clusters <- c()
i = 1
for (i in 1:n.clusters) {
  lista_clusters[[i]] <- as.data.frame(pag_cluster[which(pag_cluster$cluster == i),])
  i = i + 1
}

cluster_1 <- as.data.frame(lista_clusters[1])

