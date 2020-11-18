#Laboratorio 1 - Análisis de Datos
#Integrantes:
#            -Nicolás López
#            -Alberto Rodríguez
library(dplyr) 
library(ggplot2)
library(ggpubr)
url<-"https://raw.githubusercontent.com/Albertorz31/Analisis-de-Datos/main/agaricus-lepiota.data"

# Leemos los datos
mushrooms <- read.table(url, header = TRUE, sep = ",")


#Separamos los hongos en los que son venenosos y los que son comestibles
venenosos <- mushrooms %>% filter(p == "e") 
comestibles <-mushrooms %>% filter(p == "p")
olorV<-as.data.frame(table(venenosos[["p.1"]]))
colnames(olorV)<-c("Olor","Frecuencia")
olorC<-as.data.frame(table(comestibles[["p.1"]]))
colnames(olorC)<-c("Olor","Frecuencia")

#Graficos de barras, se realiza un grafico de barra a cada uno de los datos anteriores (venenoso y comestible)
#esto para cuales valores del atributo olor son los mas frecuentes en cada clase de hongo.
p1<-ggbarplot(olorV, x="Olor",y="Frecuencia",label = TRUE,lab.pos = "out", lab.col = "black",
              fill = "Olor", palette = "jco", title = "Frecuencia de olores en hongos venenosos")
p1<-p1 + rotate_x_text(angle=45)
plot(p1)

p2<-ggbarplot(olorC, x="Olor",y="Frecuencia",label = TRUE,lab.pos = "out", lab.col = "black",
              fill = "Olor", palette = "jco", title = "Frecuencia de olores en hongos comestibles")
p2<-p2 + rotate_x_text(angle=45)
plot(p2)



###################Reglas Lógicas##################

#Primera regla: R1

#primero definimos que valores de olor se evaluan en esta regla
almendra<- "a"
anis<- "l"
ninguno<- "n"








