install.packages("tidyverse")
install.packages("neuralnet")
library(tidyverse)
library(neuralnet)

###########################
# APRENDIZAJE SUPERVISADO # - REDES NEURONALES
###########################


#---------------#
# CLASIFICACIÓN #
#---------------#

mydata <- read.csv("https://github.com/MGCodesandStats/statsmodels-sklearn-linear-regression/raw/master/logistic%20python/dividendinfo.csv") 

# Normalizar
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

maxmindf <- as.data.frame(lapply(mydata, normalize))

# Partición de entrenamiento y prueba
trainset <- maxmindf[1:160, ]
testset <- maxmindf[161:200, ]

# Red neuronal
nn <- neuralnet(dividend ~ fcfps + earnings_growth + de + mcap + current_ratio, data=trainset, hidden=c(2,1), linear.output=FALSE, threshold=0.01)
nn$result.matrix
plot(nn)

# Resultado
temp_test <- subset(testset, select = c("fcfps","earnings_growth", "de", "mcap", "current_ratio"))
head(temp_test)
nn.results <- compute(nn, temp_test)
results <- data.frame(actual = testset$dividend, prediction = nn.results$net.result)

# Matriz de confusión
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
table(roundedresultsdf$actual,roundedresultsdf$prediction)

# Precisión
38/40


#-----------#
# REGRESIÓN #
#-----------#

mydata <- read.csv("https://github.com/MGCodesandStats/neural-network-modelling-neuralnet-r/raw/master/gasoline.csv")

# Normalización Max - Min
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

maxmindf <- as.data.frame(lapply(mydata, normalize))

# Entrenamiento y pruba
trainset <- maxmindf[1:32, ]
testset <- maxmindf[33:40, ]

nn <- neuralnet(consumption ~ capacity + price + hours,data=trainset, hidden=c(2,1), linear.output=TRUE, threshold=0.01)
nn$result.matrix

# Graficar
plot(nn)

# Resultado
temp_test <- subset(testset, select = c("capacity","price", "hours"))
head(temp_test)
nn.results <- compute(nn, temp_test)
results <- data.frame(actual = testset$consumption, prediction = nn.results$net.result)
results

# Suma de errores al cuadrado
SSE <- sum((nn.results$net.result - testset[, 1])^2)/2


# Precisión
predicted = results$prediction * abs(diff(range(mydata$consumption))) + min(mydata$consumption)
actual = results$actual * abs(diff(range(mydata$consumption))) + min(mydata$consumption)
comparison = data.frame(predicted,actual)
deviation = abs((actual-predicted)/actual)
comparison = data.frame(predicted,actual,deviation)
accuracy = 1 - abs(mean(deviation))
accuracy



# Red mejorada con neuronas adicionales en la capa escondida
nn <- neuralnet(consumption ~ capacity + price + hours,data=trainset, hidden=c(5,2), linear.output=TRUE, threshold=0.01,act.fct = "tanh")
results <- data.frame(actual = testset$consumption, prediction = nn.results$net.result)

predicted = results$prediction * abs(diff(range(mydata$consumption))) + min(mydata$consumption)
actual = results$actual * abs(diff(range(mydata$consumption))) + min(mydata$consumption)
comparison = data.frame(predicted,actual)
deviation = abs((actual-predicted)/actual)
comparison = data.frame(predicted,actual,deviation)
accuracy= 1-abs(mean(deviation))
accuracy

SSE <- sum((nn.results$net.result - testset[, 1])^2)/2

