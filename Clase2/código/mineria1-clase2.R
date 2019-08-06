############################
# MINERÍA DE TEXTO PARTE 1 #
############################

# Paquete para manipular datos
#install.packages("tidyverse")
library(tidyverse)

# Paquete para leer PDFs no escaneados
#install.packages("pdftools")
library(pdftools)

# Leer CONPES Big Data (subir primero en parte inferior derecha: files, upload)
texto_crudo <- pdf_text("3920.pdf")

#-----------------------#
# Funciones importantes #
#-----------------------#

# Función que construí para preprocesamiento de texto
preproctext <- function(x){
  require(magrittr)
  x[which(is.na(x))] <- ""
  y <- x %>%
    # Quitar tildes
    iconv(.,from="utf-8",to="ASCII//TRANSLIT") %>%
    # Quitar caracteres especiales
    gsub("[^[:print:]]", " ", .) %>%
    # Pasar todo a minúsculas
    tolower %>%
    # Quitar números, símbolos y puntuación
    gsub("[^[:lower:]^[:space:]]", " ", .) %>%
    # Dejar solo un espacio entre caracteres
    gsub("[[:space:]]{1,}", " ", .) %>%
    # Quitar espacios sobrantes
    trimws
  return(y)
}

# Función para remover stopwords
RemoveStopwordsFromText <- function(texto, # texto
                                    swords # términos a remover
){
  sapply(texto, function(x){
    y <- strsplit(x," ",T)[[1]]
    paste(y[!(y%in%swords)],collapse=" ")
  }, USE.NAMES = FALSE
  )
}

#----------------------------#
# Preprocesamiento del texto #
#----------------------------#

# Cargo archivo con stop words
sw <- readLines("https://github.com/FoxHound112263/U-Nacional-DS/raw/master/Clase2/entradas/stop_words_spanish.txt",warn = F)


# Cargar stopwords adicionales
sw_adicionales <- readLines("https://github.com/FoxHound112263/U-Nacional-DS/raw/master/Clase2/entradas/stop_words_adicionales.txt",warn = F)


# Unir stopwords
all_stopwords <- unique(c(sw,sw_adicionales))


# Preprocesamiento del texto
texto_preprocesado <- as.character(sapply(texto_crudo, function(x) preproctext(x)))

# Remover stopwords
texto_limpio <- as.character(sapply(texto_preprocesado, function(x) RemoveStopwordsFromText(x,all_stopwords)))

# Obtener cada caracter del texto - tokenizar
lista_palabras <- unlist(strsplit(texto_limpio, split = ' ',fixed = T))

# Reemplazar palabras adicionales - lematizar
#lista_palabras <- gsub(pattern = "", replacement = "",x = lista_palabras)

# Ordenar de manera decreciente
lista_palabras <- sort(table(lista_palabras),decreasing = T)

#----------#
# Graficar #
#----------#

# Primeras 20 palabras
primeras <- lista_palabras[1:20]

# Graficar
library(esquisse)
esquisser(as.data.frame(primeras))

# Lollipop chart
ggplot(as.data.frame(lista_palabras[1:20]), aes(x=reorder(lista_palabras,Freq), y=Freq, label = Freq, color = lista_palabras)) +
  geom_point(size=3) +
  geom_segment(aes(x=lista_palabras,
                   xend=lista_palabras,
                   y=0,
                   yend=Freq)) +
  labs(title="Lollipop Chart",
       subtitle="Palabras más frecuentes en CONPES de Big Data",
       caption="fuente: CONPES 3920") +
  coord_flip() +
  theme_minimal() +
  geom_text(nudge_y = 35) +
  theme(panel.grid = element_blank(),legend.position = "none")

#------------------#
# Nube de palabras #
#------------------#

# Nube
wordcloud2(lista_palabras[1:100])

# Blue palette - colores personalizables con código hexadecimal
custom_colors <- c("#005073", "#107dac", "#189ad3", "#1ebbd7", "#71c7ec")

# Con más esmero
wordcloud2(lista_palabras[1:100],
           color=rep_len( custom_colors, nrow(lista_palabras[1:100])),backgroundColor = "white",shape = 'circle')


#---------------------------------#
# Construcción de red de bigramas
#---------------------------------#

# Paquete para encontrar bigramas
#install.packages("tidytext")
library(tidytext)

# Convertir en tabla (tibble es casi lo mismo que un dataframe, pero se necesita para la función de encontrar bigramas)
texto_limpio <- tibble(texto = texto_limpio)

# Totalidad de los bigramas
bigramas <- texto_limpio %>% unnest_tokens(bigram, texto, token = "ngrams", n = 2)

# Separar cada palabra de los bigramas en columnas
bigramas_separados <- bigramas %>% separate(bigram, c("word1", "word2"), sep = " ")

# Columna con frecuencia del bigrama
bigramas_conteo <- bigramas_separados %>% count(word1, word2, sort = TRUE)

# Paquete para crear objeto graficable
#install.packages("igraph")
library(igraph)

# Objeto a graficar
bigram_graph <- bigramas_conteo[1:50,] %>% filter(n > 2) %>% 
  graph_from_data_frame()


# Paquete para graficar red
#install.packages("ggraph")
library(ggraph)

# Gráfico más elaborado (esta es la representación gráfica de una cadena de Markov)

set.seed(11234)
a <- grid::arrow(type = 'closed', length = unit(.15, "inches"))
#x11()
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = F,
                 arrow = a,linemitre = 8, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "firebrick3", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  ggtitle('Red de bigramas más utilizados en CONPES 3920') +
  theme_void() +
  theme(plot.title=element_text(hjust=0.5))



