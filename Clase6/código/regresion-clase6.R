install.packages("MASS")
install.packages("ggplot2")
install.packages("caret")
install.packages("corrplot")
install.packages("QuantPsyc")
install.packages("AER")
install.packages("lmtest")
install.packages("DAAG")
install.packages("Boruta")

library(Boruta)
library(DAAG)
library(lmtest)
library(MASS)
library(ggplot2)
library(caret)
library(corrplot)
library(QuantPsyc)



# Cargar datos
data <- mtcars

# Looking at variables
names(data)

# Resumen de los datos
head(data)

# Distribución de las variables
summary(data)

# Preparación de los datos - guardar variables categóricas como tal
data$am   = as.factor(data$am)
data$cyl  = as.factor(data$cyl)
data$vs   = as.factor(data$vs)
data$gear = as.factor(data$gear)

# Identificar y corregir multicolinearidad - quitar variable dependiente
data_a = subset(data, select = -c(mpg))

# Identificar variables numéricas
numericData <- data_a[sapply(data_a, is.numeric)]

#Calculating Correlation
descrCor <- cor(numericData)

# Imprimir
print(descrCor)
#dev.off()
# Matriz de correlaciones decente
corrplot(descrCor, order = "FPC", method = "color", type = "lower", tl.cex = 0.7, tl.col = rgb(0, 0, 0))

# Checar variables altamente correlacionadas - Caret
highlyCorrelated = findCorrelation(descrCor, cutoff=0.7)

# Nombres de las variables anteriores
highlyCorCol = colnames(numericData)[highlyCorrelated]

# Ver cuáles son
highlyCorCol

# Remover variables que se correlacionan
dat3 = data[, -which(colnames(data) %in% highlyCorCol)]
dim(dat3)

# Variable importantes
boruta.bank_train <- Boruta(mpg~., data = dat3)
print(boruta.bank_train)
# fix
boruta.bank <- TentativeRoughFix(boruta.bank_train)
plot(boruta.bank)

getSelectedAttributes(boruta.bank, withTentative = F)

# Construir modelo de regresión
fit = lm(mpg ~ ., data=dat3)

# Resumen
summary(fit)

# Coeficientes
summary(fit)$coeff
anova(fit)

par(mfrow=c(2,2))
plot(fit)

# Métricas
# R cuadrado
summary(fit)$r.squared

# R cuadradado ajustado
summary(fit)$adj.r.squared

# Bondad de ajuste
# Criterio de información de Akaike
AIC(fit)

# Criterio de información bayesiano
BIC(fit)

# Selección de variables a través de regresión Step Wise
# AIC ambos sentidos
step <- stepAIC(fit, direction="both")
summary(step)


# AIC hacia atrás
stepAIC(fit, direction="backward")


# AIC hacia adelante
stepAIC(fit, direction="forward")


# BIC
n = dim(dat3)[1]
stepBIC = stepAIC(fit,k=log(n))
summary(stepBIC)


# Modelo elejido

AIC(step)

BIC(step)

# Inflación de la varianza - menor a 5
vif(step)


# coeficientes estandarizados
modelformula <- mpg ~ cyl + drat + qsec + vs + am + gear + carb
mycars <- lapply(mtcars[, all.vars(modelformula)], scale)
fit2 <-  lm(mpg ~ ., data=mycars)


# Stepwise con coeficinetes estandarizados
step2 <- stepAIC(fit2, k = log(n))
summary(step2)

step2$coefficients

# Test de autocorrelación (p < 0.05)
durbinWatsonTest(step)
bgtest(step)


# Test de Normalidad de residuos (p > 0.05)
res=residuals(step,type="pearson")
shapiro.test(res)

# Test de heterocedasticidad (p > 0.05)
bptest(step)

# Utilizar errores robustos
coeftest(step, vcov = vcovHC(step))

# Outliers – Bonferonni test (p < 0.05)
outlierTest(step)



# Ver valores predecidos
pred = predict(step,dat3)
#See Actual vs. Predicted Value
finaldata = cbind(mtcars,pred)
print(subset(finaldata, select = c(mpg,pred)))

# Validación cruzada
dev.off()
kfold = cv.lm(data=dat3, step, m=10) 



# Predecir

set.seed(100)  
trainingRowIndex <- sample(1:nrow(dat3), 0.8*nrow(dat3)) 
trainingData <- dat3[trainingRowIndex, ]  
testData  <- dat3[-trainingRowIndex, ]   


# Modelo
lmMod <- lm(mpg ~ ., data=dat3)  
distPred <- predict(lmMod, testData)  

actuals_preds <- data.frame(cbind(actuals=testData$mpg, predicteds=distPred)) 
correlation_accuracy <- cor(actuals_preds) 
