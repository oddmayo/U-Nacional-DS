#Instalación de paquetes que se utilizarán
install.packages("SnowballC")
install.packages("tm")
install.packages("twitteR")
install.packages("syuzhet")
install.packages("tidyverse")
install.packages("tictoc")
install.packages("todytext")
install.packages("graphTweets")
install.packages("igraph")
install.packages("twitteR")
install.packages("visNetwork")
install.packages("DT")


#Carga de paquetes
library(SnowballC)
library(tm)
library(twitteR)
library(syuzhet)
library(tidyverse)
library(tictoc)
library(tidytext)
library(graphTweets)
library(igraph)
library(twitteR)
library(visNetwork)
library(DT)

# Remover stopwords
RemoveStopwordsFromText <- function(texto, # texto
                                    swords # términos a remover
){
  sapply(texto, function(x){
    y <- strsplit(x," ",T)[[1]]
    paste(y[!(y%in%swords)],collapse=" ")
  }, USE.NAMES = FALSE
  )
}

# Cargo archivo con stop words
sw <- readLines("https://github.com/FoxHound112263/U-Nacional-DS/raw/master/Clase2/entradas/stop_words_spanish.txt",warn = F)


######## Autenticación
Consumer_key="YOUR API KEY"
Consumer_secret="YOUR API SECRET"
access_token="YOUR ACCESS TOKEN"
access_token_secret="YOUR ACCESS TOKEN SECRET"
setup_twitter_oauth(Consumer_key,Consumer_secret,access_token,access_token_secret)

# Hacer 'pull' de tweets - No ejecutar
tributaria = searchTwitter("#LeyDeFinanciamiento", n=5000, lang="sp")
tweets=twListToDF(tweets)

#######


# Twitter
tweets.df2 <- gsub("http.*","",tributaria$text[1:1000])
tweets.df2 <- gsub("https.*","",tweets.df2)

#Quitando los hashtags y usuarios en los tweets
tweets.df2 <- gsub("#\\w+","",tweets.df2)
tweets.df2 <- gsub("@\\w+","",tweets.df2)

# Quitando los signos de puntuación, números y textos con números
tweets.df2 <- gsub("[[:punct:]]","",tweets.df2)
tweets.df2 <- gsub("\\w*[0-9]+\\w*\\s*", "",tweets.df2)

#Transformamos la base de textos importados en un vector para
#poder utilizar la función get_nrc_sentiment
palabra.df <- as.vector(tweets.df2)

#Aplicamos la función indicando el vector y el idioma y creamos
#un nuevo data frame llamado emocion.df

tic()
emocion.df <- get_nrc_sentiment(char_v = palabra.df, language = "spanish")
toc()

#Unimos emocion.df con el vector tweets.df para ver como
#trabajó la función get_nrc_sentiment cada uno de los tweets
emocion.df2 <- cbind(tweets.df2, emocion.df)

#Creamos un data frame en el cual las filas serán las emociones
#y las columnas los puntajes totales

#Empezamos transponiendo emocion.df
emocion.df3 <- data.frame(t(emocion.df))

#Sumamos los puntajes de cada uno de los tweets para cada emocion
emocion.df3 <- data.frame(rowSums(emocion.df3))

#Nombramos la columna de puntajes como cuenta
names(emocion.df3)[1] <- "cuenta"

#Dado que las emociones son los nombres de las filas y no una variable
#transformamos el data frame para incluirlas dentro
emocion.df3 <- cbind("sentimiento" = rownames(emocion.df3), emocion.df3)

#Quitamos el nombre de las filas
rownames(emocion.df3) <- NULL

#Verificamos el data frame
print(emocion.df3)


#Primer gráfico: se detallaran las 8 emociones con sus puntajes respectivos
sentimientos1 <- ggplot(emocion.df3[1:8,],
                        aes(x = sentimiento,
                            y = cuenta, fill = sentimiento)) + 
  geom_bar(stat = "identity") +
  labs(title = "Análisis de sentimiento \n Ocho emociones",
       x = "Sentimiento", y = "Frecuencia") +
  geom_text(aes(label = cuenta),
            vjust = 1.5, color = "black",
            size = 5) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size=12),
        axis.title = element_text(size=14,face = "bold"),
        title = element_text(size=20,face = "bold"),
        legend.position = "none")

print(sentimientos1)


#Segundo gráfico: se detallan los puntajes para las valoraciones
#positiva y negativa
sentimientos2 <- ggplot(emocion.df3[9:10,], 
                        aes(x = sentimiento,
                            y = cuenta, fill = sentimiento)) + 
  geom_bar(stat = "identity") +
  labs(title = "Análisis de sentimiento \n Valoración positiva o negativa", 
       x = "Sentimiento", y = "Frecuencia") +
  geom_text(aes(label = cuenta),
            vjust = 1.5, color = "black",
            size = 5) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size=12),
        axis.title = element_text(size=14,face = "bold"),
        title = element_text(size=20,face = "bold"),
        legend.position = "none")
print(sentimientos2)

# Conteo de palabras
tweet_words <- palabra.df %>% RemoveStopwordsFromText(.,sw) 

tweet_words <- tweet_words %>% tibble(text=tweet_words) %>% unnest_tokens(word,text)

# Graficar
tweet_words %>% count(word,sort=T) %>% slice(1:20) %>% 
  ggplot(aes(x = reorder(word, 
                         n, function(n) -n), y = n)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 60, 
                                                                                                                      hjust = 1)) + xlab("")
#----------------------------------------------------------------------------------------------------------------------
# Visualizar red

tributaria2 <-  tributaria %>% group_by(user_id,screen_name) %>% summarise(text=paste(text,collapse=''))

tributaria_retweets_network <- tributaria[1:200,] %>% 
  filter(retweet_screen_name %in% screen_name) %>%     # <- This is a new line and important.
  gt_edges(screen_name, retweet_screen_name, text) %>%             # It only keep retweets of other senate members
  gt_graph()

# Vértices
tributaria_retweets_nodes <- igraph::as_data_frame(tributaria_retweets_network, what = "vertices")

# Información adicional como nombre del nodo y tamaño de acuerdo al número de retweets
tributaria_retweets_nodes <- tributaria_retweets_nodes %>% 
  mutate(id = name) %>% 
  mutate(label = name) %>% 
  mutate(title = name) %>% 
  mutate(degree = degree(tributaria_retweets_network)) %>% 
  mutate(value = degree)

# Aristas
tributaria_retweets_edges <- igraph::as_data_frame(tributaria_retweets_network, what = "edges")

# Mostrar texto del tweet sobre la arista al pasar el mouse
tributaria_retweets_edges <- tributaria_retweets_edges %>% 
  mutate(title = text)

# Visualizar red
visNetwork(tributaria_retweets_nodes, tributaria_retweets_edges, main = "Red sobre ley de financiamiento 2018") %>% 
  visIgraphLayout(layout = "layout_nicely") %>% 
  visEdges(arrows = "to")

# Tabla con número de retweets
tributaria_retweets_nodes %>%
  select(name, degree) %>%
  datatable()

tributaria_retweets_nodes <- tributaria_retweets_nodes %>% 
  mutate(group = membership(infomap.community(tributaria_retweets_network)))

# Red con grupos
visNetwork(tributaria_retweets_nodes, tributaria_retweets_edges, main = "Red sobre ley de financiamiento 2018") %>% 
  visIgraphLayout(layout = "layout_nicely") %>% 
  visEdges(arrows = "to") %>%   
  visOptions(highlightNearest = T, nodesIdSelection = T, selectedBy = "group")
