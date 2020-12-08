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


#Relacion entre 2 variables del estudio

library(tidyverse)
library(purrr)

#Se renombran las columnas
colnames(mushrooms) <- c("edibility", "cap_shape", "cap_surface", 
                        "cap_color", "bruises", "odor", 
                        "gill_attachement", "gill_spacing", "gill_size", 
                        "gill_color", "stalk_shape", "stalk_root", 
                        "stalk_surface_above_ring", "stalk_surface_below_ring", "stalk_color_above_ring", 
                        "stalk_color_below_ring", "veil_type", "veil_color", 
                        "ring_number", "ring_type", "spore_print_color", 
                        "population", "habitat")

#Se convierten las variables a factor

mushrooms <- mushrooms %>% map_df(function(.x) as.factor(.x))

#Se entregan niveles para poder trabajar los graficos 

levels(mushrooms$edibility) <- c("edible", "poisonous")
levels(mushrooms$cap_shape) <- c("bell", "conical", "flat", "knobbed", "sunken", "convex")
levels(mushrooms$cap_color) <- c("buff", "cinnamon", "red", "gray", "brown", "pink", 
                                "green", "purple", "white", "yellow")
levels(mushrooms$cap_surface) <- c("fibrous", "grooves", "scaly", "smooth")
levels(mushrooms$bruises) <- c("no", "yes")
levels(mushrooms$odor) <- c("almond", "creosote", "foul", "anise", "musty", "none", "pungent", "spicy", "fishy")
levels(mushrooms$gill_attachement) <- c("attached", "free")
levels(mushrooms$gill_spacing) <- c("close", "crowded")
levels(mushrooms$gill_size) <- c("broad", "narrow")
levels(mushrooms$gill_color) <- c("buff", "red", "gray", "chocolate", "black", "brown", "orange", 
                                 "pink", "green", "purple", "white", "yellow")
levels(mushrooms$stalk_shape) <- c("enlarging", "tapering")
levels(mushrooms$stalk_root) <- c("missing", "bulbous", "club", "equal", "rooted")
levels(mushrooms$stalk_surface_above_ring) <- c("fibrous", "silky", "smooth", "scaly")
levels(mushrooms$stalk_surface_below_ring) <- c("fibrous", "silky", "smooth", "scaly")
levels(mushrooms$stalk_color_above_ring) <- c("buff", "cinnamon", "red", "gray", "brown", "pink", 
                                             "green", "purple", "white", "yellow")
levels(mushrooms$stalk_color_below_ring) <- c("buff", "cinnamon", "red", "gray", "brown", "pink", 
                                             "green", "purple", "white", "yellow")
levels(mushrooms$veil_type) <- "partial"
levels(mushrooms$veil_color) <- c("brown", "orange", "white", "yellow")
levels(mushrooms$ring_number) <- c("none", "one", "two")
levels(mushrooms$ring_type) <- c("evanescent", "flaring", "large", "none", "pendant")
levels(mushrooms$spore_print_color) <- c("buff", "chocolate", "black", "brown", "orange", 
                                        "green", "purple", "white", "yellow")
levels(mushrooms$population) <- c("abundant", "clustered", "numerous", "scattered", "several", "solitary")
levels(mushrooms$habitat) <- c("wood", "grasses", "leaves", "meadows", "paths", "urban", "waste")

mushrooms <- mushrooms %>% select(- veil_type)

mushrooms <- mushrooms %>% select(- edibility)

#Se observa si hay datos faltantes

cat("\n","Elementos vacios en el dataset", "\n")

map_dbl(mushrooms, function(.x) {sum(is.na(.x))})

#Se puede ver que no hay datos faltantes

#Grafico comestibilidad vs olor del hongo

p5 <- ggplot(mushrooms, aes(x = edibility, y = stalk_root, col = edibility)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("edible", "poisonous"), 
                     values = c("green", "red"))
plot(p5)

#Grafico textura de la copa vs color de la copa del hongo

p6 <- ggplot(mushrooms, aes(x = cap_surface, y = cap_color, col = edibility)) + 
  geom_jitter(alpha = 0.5) + 
  scale_color_manual(breaks = c("edible", "poisonous"), 
                     values = c("green", "red"))
plot(p6)
