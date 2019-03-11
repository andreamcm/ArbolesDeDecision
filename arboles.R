#-----------------------------------------------------------------------------------------------------------------------------------------------
# Universidad del Valle de Guatemala
# Autores: Andrea Maria Cordon Mayen, 16076
#          Cristopher Sebastian Recinos Ramírez, 16005
# Fecha: 11/03/2019
# arboles.R
#-----------------------------------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------------------------------------------
# Instalacion de librerias
install.packages("qqplot2")
library(qqplot2)

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
#-----------------------------------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------------------------------------------------------------------------------------------
# Arboles
#-----------------------------------------------------------------------------------------------------------------------------------------------