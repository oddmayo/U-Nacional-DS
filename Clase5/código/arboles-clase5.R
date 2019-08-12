####################
# MACHINE LEARNING # - APRENDIZAJE SUPERVISADO
####################

# Para variable categórica

#---------------------#
# Árboles de decisión #
#---------------------#

#install.packages("ISLR")
#install.packages("tree")
#install.packages("caret")
#install.packages("adabag")
library(adabag)
library(caret)
library(ISLR)
library(tree)

data(package="ISLR")
carseats<-Carseats

names(carseats)

hist(carseats$Sales)

# Convertir las ventas en variable binaria / categórica
High = ifelse(carseats$Sales<=8, "No", "Yes")
carseats = data.frame(carseats, High)

# Hacer fit del modelo con todas las variables excepto Sales
tree.carseats = tree(High~.-Sales, data=carseats)

# Resumen del modelo
summary(tree.carseats)

# Graficar
plot(tree.carseats)
text(tree.carseats, pretty = 0)


# HACER PREDICCIÓN

# 250 de entrenamiento y 150 de prueba

# Para hacer ejercicio reproducible
set.seed(101)
# Datos de entrenamiento - 250 datos aleatorios de los 400
train=sample(1:nrow(carseats), 250)

# Volver a hacer el fit
tree.carseats = tree(High~.-Sales, carseats, subset=train)
plot(tree.carseats)
text(tree.carseats, pretty=0)

# Predecir sobre el conjunto de datos de prueba
tree.pred = predict(tree.carseats, carseats[-train,], type="class")

# Evaluar error con matriz de confusión
with(carseats[-train,], table(tree.pred, High))

# Exactitud
(74 + 39) / (nrow(carseats[-train,]))

# Usar validación cruzada cuando los árboles son muy complejos
cv.carseats = cv.tree(tree.carseats, FUN = prune.misclass)

cv.carseats

# Graficar
plot(cv.carseats)

# Prune Tree
prune.carseats = prune.misclass(tree.carseats, best = 12)
plot(prune.carseats)
text(prune.carseats, pretty=0)

# Evaluar de nuevo en datos de prueba con el nuevo árbol
tree.pred = predict(prune.carseats, carseats[-train,], type="class")
with(carseats[-train,], table(tree.pred, High))
# Árbol más simple pero los resultados no mejoraron

# Exactitud
(72 + 38) / (nrow(carseats[-train,]))

#------------------#
# Bosque aleatorio #
#------------------#

# Problema anterior con random forest
# Fit del modelo
rf.carseats = randomForest(High~.-Sales, data = carseats[train,])
rf.carseats

pred.random = predict(rf.carseats, carseats[-train,])




# Precisión
precision <- posPredValue(pred.random, as.factor(carseats[-train,]$High), positive="Yes")
#  Recall
recall <- sensitivity(pred.random, carseats[-train,]$High, positive="Yes")
# F score
F1 <- (2 * precision * recall) / (precision + recall)


#----------#
# Boosting #
#----------#


BC.adaboost <- boosting(High ~.-Sales,data=carseats[train,],mfinal=200, coeflearn="Freund",boos=FALSE , control=rpart.control(maxdepth=3))
predmat <- predict.boosting(BC.adaboost,newdata=carseats[-train,], newmfinal=200)

# Exactitud
predmat$confusion
80 + 41 / 150

# Precisión
precision <- posPredValue(as.factor(predmat$class), carseats[-train,]$High, positive="Yes")
# Recall
recall <- sensitivity(predmat$class, carseats[-train,]$High, positive="Yes")
# F score
F1 <- (2 * precision * recall) / (precision + recall)
