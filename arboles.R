#-----------------------------------------------------------------------------------------------------------------------------------------------
# Universidad del Valle de Guatemala
# Autores: Andrea Maria Cordon Mayen, 16076
#          Cristopher Sebastian Recinos Ramírez, 16005
# Fecha: 11/03/2019
# arboles.R
#-----------------------------------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------------------------------------------
# Instalacion de librerias
install.packages("cluster")
library(cluster)
install.packages("factoextra")
library(factoextra) #Para hacer gráficos bonitos de clustering
install.packages("ape")
library(ape)
install.packages("tree")
library(tree)
library(rpart)
library(caret)
library(tree)
library(rpart.plot)
library(randomForest)


# Set de ambientes
# Para Andrea
setwd("~/2019/UVG/Primer Semestre/Minería de Datos/Laboratorios/Laboratorio3/HDT3MineriaDeDatos/Datos/test") # Para los datos para pruebas
setwd("~/2019/UVG/Primer Semestre/Minería de Datos/Laboratorios/Laboratorio3/HDT3MineriaDeDatos/Datos/train") # Para los datos de entrenamiento
setwd("~/2019/UVG/Primer Semestre/Minería de Datos/Laboratorios/Laboratorio3/HDT3MineriaDeDatos/Datos/all_data")# Para el documento con ambos documentos juntos (todos los datos)
# Para Sebas

# Creacion de variables con los datos
testData <- read.csv("test.csv") # Se guardan los datos de pruebas
trainData <- read.csv("train.csv") # Se guardan los datos de entrenamiento
allData <- read.csv("train2.csv") # Se guardan los datos completos
#-----------------------------------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------------------------------------------
# Analisis exploratorio
summary(allData) # Resumen de todos los datos
summary(testData) # Resumen de los datos de prueba
summary(trainData) # Resumen de los datos de entrenamiento
str(allData) # Tipo de todas las variables de las columnas

petType <- table(allData$Type) # Tabla de tipo mascotas
barplot(petType, xlab = "Tipo de mascota", ylab = "Cantidad", main = "Cantidad por tipo de mascota") # Grafico de la tabla de tipo de mascotas

petColors <- table(allData$Color1, allData$Color2, allData$Color3)
plot(petColors)
petColor1 <- table(allData$Color1)
petColor2 <- table(allData$Color2)
petColor3 <- table(allData$Color3)

petBreed <- table(allData$Breed1, allData$Breed2)
barplot(petBreed)
petBreed1 <- table(allData$Breed1)
petBreed1
petBreed2 <- table(allData$Breed2)
petBreed2

petGender <- table(allData$Gender)
barplot(petGender, xlab = "Género de la mascota", ylab = "Cantidad", main = "Géneros de las mascotas")

status <- table(allData$Dewormed, allData$Vaccinated, allData$Sterilized)
status
plot(status)
petDewormed <- table(allData$Dewormed)
petDewormed
petVaccinated <- table(allData$Vaccinated)
petVaccinated
petSterilized <- table(allData$Sterilized)
petSterilized
#-----------------------------------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------------------------------------------
# Clustering
#Gráfico de codo para determinar cuántos clusters utilizar
wss <- (nrow(allData[,c("Type", "Breed1", "Breed2", "Gender", "Color1", "Color2", "Color3", "MaturitySize", "FurLenght", "Vaccinated", "Dewormed", "Sterilized", "Health", "Quantity", "Fee", "State", "RescuerID", "AdoptionSpeed")])-1)*sum(apply(allData[,c("Type", "Breed1", "Breed2", "Gender", "Color1", "Color2", "Color3", "MaturitySize", "FurLenght", "Vaccinated", "Dewormed", "Sterilized", "Health", "Quantity", "Fee", "State", "RescuerID", "AdoptionSpeed")],2,var))

for (i in 2:15) 
  wss[i] <- sum(kmeans(allData[,c("Type", "Breed1", "Breed2","Gender", "Color1", "Color2","Color3", "MaturitySize", "FurLenght", "Vaccinated", "Dewormed", "Sterilized", "Health", "Quantity", "Fee", "State", "RescuerID", "AdoptionSpeed")], centers=i)$withinss)

plot(1:15, wss, type="b", xlab="Number of Clusters",  ylab="Within groups sum of squares")


#-----------------------------------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------------------------------------------
# Arboles
porciento <- 70/100 # Porcentaje de grupos de entrenamiento
set.seed(123)
trainRowsNumber<-sample(1:nrow(allData),porciento*nrow(allData))
train<-allData[trainRowsNumber,] # Grupo de entrenamiento
test<-allData[-trainRowsNumber,] # Grupo de pruebas
#-----------------------------------------------------------------------------------------------------------------------------------------------