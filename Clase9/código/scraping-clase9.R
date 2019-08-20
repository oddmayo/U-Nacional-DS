install.packages("tictoc")
install.packages("rvest")
install.packages("stringr")
install.packages("tidyr")
install.packages("methods")
install.packages("openxlsx")
install.packages("wordcloud")
install.packages("beepr")

library(tictoc)
library(rvest)
library(stringr)
library(tidyr)
library(methods)
library(openxlsx)
library(wordcloud)
library(beepr)

# Preprocesar texto
preproctext <- function(x){
  require(magrittr)
  x[which(is.na(x))] <- ""
  y <- x %>%
    # Quitar tildes
    #iconv(.,from="utf-8",to="ASCII//TRANSLIT") %>%
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


url <- "http://es.presidencia.gov.co/discursos"
site <- read_html(url)

links <- html_nodes(site,"a") %>%
  html_attr("href")

links <-links[grepl("discursos",links)==T]

links <- links[-c(1,2,3)]

#--------------------------------------------------------------------------------------------------------------------
# Entender el problema para un caso #

url <- links[3]
site <- read_html(url)

# Título
title <- html_nodes(site,"h1") %>% html_text()
title <- preproctext(title)
# Tomar texto antes de cdata
title <-  sub("\\cdata.*", "", title) %>% trimws()


# Cuerpo
texto <- html_nodes(site,"p") %>% html_text()
# Pegar texto
texto <- paste(texto, collapse = '')
texto <-  preproctext(texto) %>% RemoveStopwordsFromText(.,sw)

#-------------------------------------------------------------------------------------------------------------------
# Bucle para todos los discursos
discursos <- data.frame(title=character(),text=character(),stringsAsFactors = F)

tic()
for (i in 1:length(links)){
  url <- links[i]
  site <- read_html(url)
  title <- html_nodes(site,"h1") %>% html_text()
  text <- html_nodes(site,"p") %>% html_text()
  text <- paste(text, collapse = '')
  text <- preproctext(text) %>% RemoveStopwordsFromText(.,sw)
  title <- preproctext(title)
  title <- sub("\\cdata.*", "", title) %>% trimws()
  
  discursos[i,]<- c(title,text)
}
toc()

# Ejemplo primer discurso
lista_p <- strsplit(discursos$text[2], " ")[[1]]
wordcloud(lista_p)


#----------------
# A WIKIPEDIA
scraping_wiki <- read_html("https://en.wikipedia.org/wiki/Web_scraping")

scraping_wiki %>%
  html_nodes("h1") %>%
  html_text()

# Títulos de segundo nivel
scraping_wiki %>%
  html_nodes("h2") %>%
  html_text()

# Texto
p_nodes <- scraping_wiki %>% 
  html_nodes("p") %>%
  html_text()

# unordered list
ul_text <- scraping_wiki %>%
  html_nodes("ul") %>%
  html_text()

# todo
all_text <- scraping_wiki %>%
  html_nodes("div") %>% 
  html_text()

# Texto específico
body_text <- scraping_wiki %>%
  html_nodes("p:nth-child(50)") %>% 
  html_text()



