setwd("F:/R")
#leer y limpiar datos
data <- unique(read.csv("Customertravel.csv", header = TRUE))
str(data)
data <- data[data$FrequentFlyer != "No Record", ]

#Codificar las variables categóricas como factores
data$FrequentFlyer <- factor(data$FrequentFlyer)
data$AnnualIncomeClass <- factor(data$AnnualIncomeClass)
data$AccountSyncedToSocialMedia <- factor(data$AccountSyncedToSocialMedia)
data$BookedHotelOrNot <- factor(data$BookedHotelOrNot)
data$Target <- factor(data$Target)

write.csv(data, "F:/R/clean.csv")

str(data)
n <- nrow(data)


#dividimos es conjunto de datos
#install.packages("caret")
library(caret)
set.seed(42)
particion <- createDataPartition(data$Target, p = 0.75, list = FALSE)
particion
#creamos el set de entrenamiento tomando los  75%
particion_entrenamiento <- data[particion, ]
particion_entrenamiento
write.csv(particion_entrenamiento, "F:/R/entrenamiento.csv")
#creamos el set de prueba tomando los  25%
particion_prueba <- data[-particion, ]
particion_prueba 

          

# Construir el árbol de decisión
library(rpart)
arbol_modelo <- rpart(Target ~., data=particion_entrenamiento, method="class")

# Visualizar el árbol de decisión
library(rpart.plot)
rpart.plot(arbol_modelo, extra = 104)

#analizamos el poder predictivo, viendo cuanto pesa cada variable en la predicción

#ahora usamos el set de prueba para predecir con el modelo
prediccion <- predict(arbol_modelo, particion_prueba, type="class")
prediccion

#analizamos la predicción
matriz_confucion <- table(particion_prueba$Target, prediccion)
matriz_confucion
#Precision del modelo
presicion <- sum(diag(matriz_confucion))/nrow(particion_prueba)
presicion
#Precision del 0(Customer Doesnt Churn)
presicion_no <-((matriz_confucion[1,1])/sum(matriz_confucion[1,]))
presicion_no 

#Precision del 1(Customer Churns)
presicion_si <-((matriz_confucion[2,2])/sum(matriz_confucion[2,]))
presicion_si
# Ajustar hiperparámetros
control <- rpart.control(minsplit = 20, minbucket = 7, maxdepth=30)

# Construir el árbol de decisión
modelo_arbol_corregido <- rpart(Target ~ ., data = particion_entrenamiento, method="class", control=control)

# Evaluar el modelo
prediccion_corregida <- predict(modelo_arbol_corregido, particion_prueba, type="class")
matriz_confucion_corregida <- table(particion_prueba$Target, prediccion_corregida)
matriz_confucion_corregida
presicion_corregida <- sum(diag(matriz_confucion_corregida))/nrow(particion_prueba)

# Imprimir resultados
print(presicion_corregida)
#analizamos el poder predictivo, viendo cuanto pesa cada variable en la predicción
library(caret)
#install.packages("dplyr")
library(dplyr)
poder_prediccion <- varImp(arbol_modelo)
poder_prediccion %>%
  arrange(desc(Overall))
