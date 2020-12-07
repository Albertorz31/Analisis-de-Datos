#Laboratorio 2 - Análisis de Datos
#Integrantes:
#            -Nicolás López
#            -Alberto Rodríguez
library(dplyr) 
library(ggplot2)
library(ggpubr)
url<-"https://raw.githubusercontent.com/Albertorz31/Analisis-de-Datos/main/agaricus-lepiota.data"

# Leemos los datos
mushrooms <- read.table(url, header = TRUE, sep = ",")

#Semilla: se usa porque como se eligen los centroides al azar, usamos esta funcion para que obtengamos siempre 
#los mismo resultadoss
set.seed(88)


######PRE-PROCESAMIENTO######

#Eliminamos el atributo "classes" o como sale escrito "p".
mushrooms$p=NULL
#Eliminamos todas las filas donde el atributo stalk.root tiene datos faltantes
mushrooms <- mushrooms[which(mushrooms$e.1 != '?'),]
# Eliminamos atributo innecesario (solo tiene un valor posible)
mushrooms$p.2 <- NULL









