###########################
# APRENDIZAJE SUPERVISADO # - Múltiples modelos de clasificación
###########################

# PAQUETES #
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("gridExtra")
install.packages("scales")
install.packages("dplyr")
install.packages("qdap")
install.packages("pROC")
install.packages("caret")
install.packages("kernlab")
install.packages("e1071")

library(ggplot2)
library(ggthemes)
library(gridExtra)
library(scales)
library(dplyr)
library(qdap)
library(pROC)
library(caret)
library(kernlab)
library(e1071)


# DATOS #
train <- read.csv("https://github.com/FoxHound112263/U-Nacional-DS/raw/master/Clase7/data/train.csv", stringsAsFactors = F)
test <- read.csv("https://github.com/FoxHound112263/U-Nacional-DS/raw/master/Clase7/data/test.csv", stringsAsFactors = F)
allSet <- bind_rows(train, test)

glimpse(allSet)

#----------------------------------------------------------------------------------------------------------------------------

#----------#
# LIMPIEZA #
#----------#

# DUPLICADOS
ifelse(length(unique(allSet[,1])) == nrow(allSet),"No hay duplicados","Duplicados detectados")

# DIMENSIÓN
allSetC <- dim(allSet)[2]

# ARREGLAR VARIABLES CATEGÓRICAS
allSet$Survived <- as.factor(allSet$Survived)
allSet$Pclass <- as.factor(allSet$Pclass)
allSet$Sex <- as.factor(allSet$Sex)
allSet$Cabin <- as.factor(allSet$Cabin)
allSet$Embarked <- as.factor(allSet$Embarked)

# VALORES FALTANTES #

# Reemplazar valores faltantes por NAs
for (i in 1:allSetC){
  allSet[,i][allSet[,i]== ""] <- NA
}

# Función para obtener el número de NAs por variable
getNA <- function(dt,NumCol){
  varsNames <- names(dt)
  NAs <- 0
  
  for (i in 1:NumCol){
    NAs <- c(NAs, sum(is.na(dt[,i])))
  }
  
  NAs <- NAs[-1]
  names(NAs)<- varsNames # Vector con nombre de la variable y conteo de NAs
  
  NAs <- NAs[NAs > 0]
  NAs 
}

getNA(allSet,allSetC)

# IMPUTACIÓN DE DATOS FALTANTES #

## Para la variable 'Embarked'
allSet[,c("PassengerId","Pclass","Fare","Embarked")] %>% filter(is.na(Embarked))

# Filtrar para todos los datos de puerto de embarcación
FareClassComp <- allSet %>% filter(!is.na(Embarked))

# Graficar puertos vs tarifa coloreado por clase
FareClassComp %>% 
  ggplot(aes(x = Embarked, y = Fare, fill = Pclass))+
  geom_boxplot()+
  geom_hline(aes(yintercept = 80),
             colour = "red", linetype = "dashed", lwd = 2)+
  scale_y_continuous(labels = dollar_format())+
  theme_few()

# Imputar dato para ambos casos de 'Embarked' faltantes
allSet$Embarked[is.na(allSet$Embarked)] <- "C"

## Para la variable 'Fare'
allSet[,c("PassengerId","Pclass","Fare","Embarked")] %>% filter(is.na(Fare))

# Imputar dato para único caso de 'Fare' faltante
allSet$Fare[allSet$PassengerId == 1044] <-  median(allSet$Fare[allSet$Pclass == 3 & allSet$Embarked == "S"], na.rm = T)

# INGENIERÍA DE VARIABLES #

## Crear variable 'FamSz'
allSet$FamSz <- allSet$SibSp + allSet$Parch + 1

# Crear categorías para tamaño de la familia
allSet$FamSzCat[allSet$FamSz == 1] <- "Singles"
allSet$FamSzCat[allSet$FamSz > 1 & allSet$FamSz <5] <- "Small"
allSet$FamSzCat[allSet$FamSz > 4] <- "Large"

allSet$FamSzCat <- as.factor(allSet$FamSzCat)

# # Crear variable 'surname'
# allSet$Surname <- sapply(allSet$Name, function(x) strsplit(x, split = "[,]")[[1]][1])
# paste(nlevels(factor(allSet$Surname)), "familias en el Titanic")

# Crear variable 'AgeStg'
allSet$AgeStg[allSet$Age < 18 & !is.na(allSet$Age)] <- "Child"
allSet$AgeStg[allSet$Age >= 18 & !is.na(allSet$Age)] <- "Adult"

length(allSet$AgeStg[is.na(allSet$AgeStg)])

# Imputar dato de 'AgeStg'

# Utilizar modelo lineal generalizado

# Vector de variables que ayuden a predecir
varsNames <- c("PassengerId","Pclass", "Sex", "SibSp", "Parch", "Fare", "FamSz", "FamSzCat", "AgeStg")

# Subconjunto para la predicción
allSetAgeStg <- allSet[,varsNames]

# 2 subconjuntos, uno completo y otro de missing
allSetAgeStgComp <- allSetAgeStg[!is.na(allSetAgeStg$AgeStg),]
allSetAgeStgMiss <- allSetAgeStg[is.na(allSetAgeStg$AgeStg),]

# Particionar entre entrenamiento y prueba
nTrain <- 0.75 * nrow(allSetAgeStgComp)

# Muestreo del 75%
set.seed(3030)
sampleTrain <- sample(nrow(allSetAgeStgComp),nTrain)

# Distinguir entre conjuto de entrenamiento y prueba
AgeStgTrain <- allSetAgeStgComp[sampleTrain,]
AgeStgTest <- allSetAgeStgComp[-sampleTrain,]

## Utilizar regresión logística con stepwise
# 1. Construir modelo vacío
#set.seed(3030)
null_model <- glm(factor(AgeStg)~1, data = AgeStgTrain, family = "binomial")

# 2. Construir modelo completo
#set.seed(3030)
full_model <- glm(factor(AgeStg)~Pclass+Sex+SibSp+Parch+Fare+FamSz+FamSzCat, data = AgeStgTrain, family = "binomial")

# Modelo stepwise
step_model <- step(null_model, scope = list(lower= null_model,upper = full_model),direction = "forward")

# Estimar probabilidad con stepwise para entrenamiento y prueba
AgeStgTrain$stepProb <- predict(step_model, data = AgeStgTrain, type = "response")
AgeStgTest$stepProb <- predict(step_model, newdata = AgeStgTest, type = "response")

# Crear curva ROC
ROC_train <- roc(AgeStgTrain$AgeStg,AgeStgTrain$stepProb)
ROC_test <- roc(AgeStgTest$AgeStg,AgeStgTest$stepProb)

# Graficar ROC
plot(ROC_train,col = "red")

plot(ROC_test,col = "red")

# Área bajo la curva (AUC)
auc(ROC_train)
auc(ROC_test)

trainAcc <- percent(auc(ROC_train))
testAcc <- percent(auc(ROC_test))

## Validar predicción en conjunto de prueba antes de predecir para todos los datos

# Número promedio de niños para establecerlo como límite de la probabilidad
AvgChildCount <- mean(AgeStgTrain$AgeStg == "Child")

# Predecir categoría de edad en datos de prueba si la prob. es mayor al promedio
AgeStgTest$AgeStgPred <- ifelse(AgeStgTest$stepProb > AvgChildCount,"Child", "Adult")

# Precisión
acc <- percent(mean(AgeStgTest$AgeStg == AgeStgTest$AgeStgPred))
acc

## Predecir para el conjunto de datos completo
allSetAgeStgMiss$stepProb <- predict(step_model, newdata = allSetAgeStgMiss, type = "response")

allSetAgeStgMiss$AgeStg <- ifelse(allSetAgeStgMiss$stepProb > AvgChildCount,"Child", "Adult")

# Imputar datos faltantes en el conjunto
allSet <- left_join(allSet,allSetAgeStgMiss[,c("PassengerId","AgeStg")], by = "PassengerId", allSet.x = TRUE, allSet.y = FALSE)

allSet$AgeStg <- ifelse(is.na(allSet$AgeStg.x),allSet$AgeStg.y,allSet$AgeStg.x)
allSet <- allSet[,!colnames(allSet) %in% c("AgeStg.x","AgeStg.y")]   

allSet$AgeStg <- as.factor(allSet$AgeStg)

# No más valores faltantes adicionales
getNA(allSet,length(allSet))

#----------------------------------------------------------------------------------------------------------------------------

# Visualizar proporción de sobrevivientes
train <- allSet[!is.na(allSet$Survived),]
test <- allSet[is.na(allSet$Survived),]

ggplot(train) +
  aes(x = Survived, fill = Survived) +
  geom_bar() +
  geom_label(stat='count',aes(label=..count..))+
  scale_fill_hue() +
  theme_minimal()

#----------------------------------------------------------------------------------------------------------------------------

#--------------------------------------------#
# CONSTRUCCIÓN DE MODELOS PARA ENTRENAMIENTO #
#--------------------------------------------#

# Verificar variabilidad de los predictores
selVarsNames <- c("Pclass", "Sex", "SibSp", "Parch", "Fare","Embarked" , "FamSzCat", "AgeStg")

nearZeroVar(train[,selVarsNames], saveMetrics = TRUE)


## PARTICIÓN ##

# Filas para entrenamiento
nTrain <- round(0.75 * nrow(train))

# Muestreo
sampleTrain <- sample(nrow(train),nTrain)

# Datos temporales para solo el conjunto de entrenamiento
trainTemp <- train[sampleTrain,]
testTemp <- train[-sampleTrain,]


## BOSQUE ALEATORIO ##
set.seed(2020)
control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
modelRF <- train(Survived~Pclass+Sex+SibSp+Parch+Fare+Embarked+FamSzCat+AgeStg, data = trainTemp, method = "rf", trControl = control)

print(modelRF)


## GRADIENT BOOSTING ##
#set.seed(2020)
control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
modelGBM <- train(Survived~Pclass+Sex+SibSp+Parch+Fare+Embarked+FamSzCat+AgeStg, data = trainTemp, method = "gbm", trControl = control, verbose = FALSE)

print(modelGBM)


## MÁQUINA DE VECTORES DE SOPORTE ##
#set.seed(2020)
control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
modelSVM <- train(Survived~Pclass+Sex+SibSp+Parch+Fare+Embarked+FamSzCat+AgeStg, data = trainTemp, method = "svmRadial", trControl = control)

print(modelSVM)

#----------------------------------------------------------------------------------------------------------------------------

#------------------------#
# COMPARACIÓN DE MODELOS #
#------------------------#

# Resultados de los remuestreos
results <- resamples(list(RF=modelRF, GBM=modelGBM, SVM = modelSVM))

# Distribuciones
compSummary <- summary(results)
compSummary


# boxplots of results
bwplot(results)

# dot plots of results
dotplot(results)

#----------------------------------------------------------------------------------------------------------------------------

#--------------------------------------------------------------------#
# VALIDACIÓN DE MODELOS SOBRE LOS DATOS DE PRUEBA (DE ENTRENAMIENTO) #
#--------------------------------------------------------------------#

## BOSQUE ALEATORIO ##
rfPred<-predict(modelRF,testTemp)
rfCM<-confusionMatrix(rfPred,testTemp$Survived)
rfCM
# Precisión y error
modelRFacc<-percent(as.numeric(rfCM$overall[1]))
modelRFerr<-percent(1-(as.numeric(rfCM$overall[1])))


## GRADIENT BOOSTING ##
gbmPred<-predict(modelGBM,testTemp)
gbmCM<-confusionMatrix(gbmPred,testTemp$Survived)
gbmCM
# Precisión y error
modelGBMacc<-percent(as.numeric(gbmCM$overall[1]))
modelGBMerr<-percent(1-(as.numeric(gbmCM$overall[1])))

## MÁQUINA DE VECTORES DE SOPORTE ##
svmPred<-predict(modelSVM,testTemp)
svmCM<-confusionMatrix(svmPred,testTemp$Survived)
svmCM
# Precisión y error
modelSVMacc<-percent(as.numeric(svmCM$overall[1]))
modelSVMerr<-percent(1-(as.numeric(svmCM$overall[1])))


# COMPARACIÓN #
tblAcc <- data.frame("Accuracy"=c(modelRFacc, modelGBMacc, modelSVMacc), "Error"= c(modelRFerr,modelGBMerr,modelSVMerr), row.names = c("RF","GBM","SVM"))

tblAcc

#----------------------------------------------------------------------------------------------------------------------------

#------------------------------------#
# PREDICCIÓN CON MODELO SELECCIONADO #
#------------------------------------#

pred <- predict(modelSVM, test)

survival <- data.frame(PassengerId = test$PassengerId, Survived = pred)

subSummary <- table(survival$Survived)
subSummary

survPerc <- percent(subSummary[2]/(subSummary[1]+subSummary[2]))
survPerc

final_table <- test
final_table$Survived <- pred

# Graficar, aunque no tenga mucho sentido
test_plot <- final_table %>% select(Survived,selVarsNames)
svm_plot <- svm(Survived ~ Pclass+Sex+SibSp+Parch+Fare+Embarked+FamSzCat+AgeStg, data = prueba, kernel = "radial")
plot(svm_plot,formula = SibSp ~ Parch, data = test_plot)

