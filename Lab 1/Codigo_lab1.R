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
n.v<-nrow(venenosos)

comestibles <-mushrooms %>% filter(p == "p")
n.c<-nrow(comestibles)

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

#Lo evaluamos para los comestibles y para los venenosos
VV <- venenosos[which(venenosos$p.1 == almendra | venenosos$p.1 == anis | venenosos$p.1 == ninguno),] #VV: verdaderos venenosos
n.VV<-nrow(VV)
n.FV<- (n.v - n.VV)  #FV: falsos venenosos
FC<- comestibles[which(comestibles$p.1 == almendra | comestibles$p.1 == anis | comestibles$p.1 == ninguno),] #FC: falsos comestibles
n.FC<-nrow(FC)
n.VC<- (n.c - n.FC) #Verdaderos comestibles

#Por lo tanto en resumen de lo anterior se muestra una matriz de confusión 
#               Venenosos     Comestibles
# Venenosos       4208            0
# Comestibles     120           3795

accuracy<-((n.VV+n.VC)/(n.VV+n.FV+n.VC+n.FC))
sensibilidad<-(n.VV/(n.VV+n.FC))
especificidad<-(n.VC/(n.VC+n.FV))
indice.exito<-(n.VV/(n.VV+n.FV+n.FC))

metricas<-matrix(c(accuracy,sensibilidad,especificidad,indice.exito))
colnames(metricas)<-c("Valor")
rownames(metricas)<-c("Presición","Sensibilidad","Especificidad","Índice de exito")
print(metricas)














