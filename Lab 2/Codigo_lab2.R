#Laboratorio 2 - Análisis de Datos
#Integrantes:
#            -Nicolás López
#            -Alberto Rodríguez
library(dplyr) 
library(ggplot2)
library(ggpubr)
library(factoextra)
library(FactoMineR)
url<-"https://raw.githubusercontent.com/Albertorz31/Analisis-de-Datos/main/agaricus-lepiota.data"

# Leemos los datos
mushrooms <- read.table(url, header = TRUE, sep = ",")

#Semilla: se usa porque como se eligen los centroides al azar, usamos esta funcion para que obtengamos siempre 
#los mismo resultadoss
set.seed(88)


######PRE-PROCESAMIENTO######

#Eliminamos el atributo "classes" o como sale escrito "p".
mushrooms$p=NULL

#Usamos summary para calcular la frecuencia de cada variables categórica
summary(mushrooms)
# Se puede observar que el atributo “veil-type” o p.2 
#solo tiene un posible valor, entonces se elimina
#Eliminamos atributo innecesario (solo tiene un valor posible)
mushrooms$p.2 <- NULL

#Eliminamos todas las filas donde el atributo stalk.root tiene datos faltantes
#mushrooms <- mushrooms[which(mushrooms$e.1 != '?'),]

#Realizamos análisis de componentes múltiple
res.mca <- MCA(mushrooms, graph = FALSE)
#Para visualizar la proporción de variaciones retenidas por las diferentes dimensiones.
eig.val <- get_eigenvalue(res.mca)
#Para visualizar los porcentajes de inercia explicados por cada dimensión de MCA
fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 45))









