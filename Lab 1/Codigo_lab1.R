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
venenosos <- mushrooms %>% filter(p == "p") 
n.v<-nrow(venenosos)

comestibles <-mushrooms %>% filter(p == "e")
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


#Regla: R1

#primero definimos que valores de olor se evaluan en esta regla
almendra<- "a"
anis<- "l"
ninguno<- "n"

#Lo evaluamos para los comestibles y para los venenosos
VC<- comestibles[which(comestibles$p.1 == almendra | comestibles$p.1 == anis | comestibles$p.1 == ninguno),] #VC: verdaderos comestibles
n.VC<-nrow(VC)
n.FV<- (n.c - n.VC) #falsos venenosos

FC <- venenosos[which(venenosos$p.1 == almendra | venenosos$p.1 == anis | venenosos$p.1 == ninguno),] #FC: falsos comestibles
n.FC<-nrow(FC)
n.VV<- (n.v - n.FC)  #VV: verdaderos venenosos


#Por lo tanto en resumen de lo anterior se muestra una matriz de confusión 
#               Venenosos     Comestibles
# Venenosos       3796            0
# Comestibles     120           4208

accuracy<-((n.VV+n.VC)/(n.VV+n.FV+n.VC+n.FC))
precision<-(n.VV/(n.VV+n.FV))
sensibilidad<-(n.VV/(n.VV+n.FC))
especificidad<-(n.VC/(n.VC+n.FV))
indice.exito<-(n.VV/(n.VV+n.FV+n.FC))

metricas<-matrix(c(accuracy,precision,sensibilidad,especificidad,indice.exito))
colnames(metricas)<-c("Valor")
rownames(metricas)<-c("Exactitud","Precision","Sensibilidad","Especificidad","Índice de exito")
cat("\n","Conjunto de Métricas", "\n")
print(metricas)


#Regla 2

#Se realizan 2 gráficos de barra para ver la frecuencia de los valores del atributo color de esporas de cada clase de hongo.
colorV<-as.data.frame(table(venenosos[["k.1"]]))
colnames(colorV)<-c("color Esporas","Frecuencia")
colorC<-as.data.frame(table(comestibles[["k.1"]]))
colnames(colorC)<-c("color Esporas","Frecuencia")

p3<-ggbarplot(colorV, x="color Esporas",y="Frecuencia",label = TRUE,lab.pos = "out", lab.col = "black",
              fill = "color Esporas", palette = "jco", title = "Frecuencia de color de esporas en hongos venenosos")
p3<-p3+ rotate_x_text(angle=45)
plot(p3)

p4<-ggbarplot(colorC, x="color Esporas",y="Frecuencia",label = TRUE,lab.pos = "out", lab.col = "black",
              fill = "color Esporas", palette = "jco", title = "Frecuencia de color de esporas en hongos comestibles")
p4<-p4 + rotate_x_text(angle=45)
plot(p4)

#Test de Fisher
#H0: El olor no determina la clase de hongo. Las variables son independientes
#H1: El olor determina la clase de un hongo. Las variables no son independientes
olor<- fisher.test(mushrooms$p, mushrooms$p.1,simulate.p.value=TRUE)
print(olor$p.value)









