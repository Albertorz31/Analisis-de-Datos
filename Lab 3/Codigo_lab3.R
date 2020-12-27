#Laboratorio 3 - Análisis de Datos
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