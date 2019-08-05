# Variables
mensaje = 5
mensaje <- 5

# Funciones y argumentos
sqrt(x = 144)

homicidios <- read.csv(file = "https://github.com/FoxHound112263/DS-Training-DAFP/raw/master/data/homicidios.txt",sep = "\t",fileEncoding = "UTF-8",encoding = "UTF-8")

homicidios[1:10,2]

homicidios$Localidad[1:5]

# Convertir a texto
homicidios$Localidad <- as.character(homicidios$Localidad)

# Reemplazar sin información por Sumapaz
homicidios$Localidad[homicidios$Localidad == "Sin información"] <- "Sumapaz"

# Convertir Total temporalmente en texto para reemplazar caracter " - "
homicidios$Total <- as.character(homicidios$Total)

# Reemplazar guion por cero
homicidios$Total[homicidios$Total == " - "] <- 0

# Convertir en numérico
homicidios$Total <- as.numeric(homicidios$Total)


# Utilizar tidyverse
library(tidyverse)


# Operador de tubo
homicidios_fix <- homicidios %>% 
  # Agrupar
  group_by(Localidad) %>% 
  # Filtrar por máximo valor
  filter(Total == max(Total)) %>% 
  # Desagrupar
  ungroup() %>%
  # Dejar valores únicos
  unique()

# Quitar último valor que no sirve
homicidios_fix <- homicidios_fix[-nrow(homicidios_fix),]

# Gráfico
ggplot(data = homicidios_fix) +
  geom_col(mapping = aes(x = factor(Localidad),y = Total,fill = Localidad)) +
  coord_flip()

# Gráfico interactivo
#install.packages("esquisse")
library(esquisse)
esquisser(homicidios_fix)







