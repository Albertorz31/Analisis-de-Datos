#Laboratorio 1 - Análisis de Datos
#Integrantes:
#            -Nicolás López
#            -Alberto Rodríguez


url<- "https://raw.githubusercontent.com/Albertorz31/Analisis-de-Datos/main/agaricus-lepiota.data"

# Leemos los datos
mushrooms <- read.table(url, header = TRUE, sep = ",")
