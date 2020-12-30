#Laboratorio 3 - Análisis de Datos
#Integrantes:
#            -Nicolás López
#            -Alberto Rodríguez
library(dplyr) 
library(arulesViz)

url<-"https://raw.githubusercontent.com/Albertorz31/Analisis-de-Datos/main/agaricus-lepiota.data"

# Leemos los datos
mushrooms <- read.table(url, header = TRUE, sep = ",")

######PRE-PROCESAMIENTO######

#Se renombran las columnas
colnames(mushrooms) <- c("edibility", "cap_shape", "cap_surface", 
                         "cap_color", "bruises", "odor", 
                         "gill_attachement", "gill_spacing", "gill_size", 
                         "gill_color", "stalk_shape", "stalk_root", 
                         "stalk_surface_above_ring", "stalk_surface_below_ring", "stalk_color_above_ring", 
                         "stalk_color_below_ring", "veil_type", "veil_color", 
                         "ring_number", "ring_type", "spore_print_color", 
                         "population", "habitat")

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

#Se guarda bd original, por si en necesaria en el futuro
original<-mushrooms 
#Usamos summary para calcular la frecuencia de cada variables categórica
summary(mushrooms)
# Se puede observar que el atributo veil-type 
#solo tiene un posible valor, entonces se elimina
#Eliminamos atributo innecesario (solo tiene un valor posible)
mushrooms <- mushrooms %>% select(- veil_type)

# Convertimos las columnas a factores
for(i in 1:ncol(mushrooms)) mushrooms[,i] <- as.factor(mushrooms[,i])

# Usamos el algoritmo apriori para crear las posibles reglas
#Se intentó inicialmente con un soprte de 0.2, pero al generar muchas reglas se intento con uno  mayor
reglas = apriori(data = mushrooms, parameter = list(support = 0.35, minlen = 1, maxlen = 4),
                appearance = list(rhs = c("edibility=poisonous", "edibility=edible")))

#Se usa la función inspect para imprimir todas las reglas
# Se imprime las reglas en base a la confianza
con<-sort(x = reglas, decreasing = TRUE, by = "confidence") #ordena regla
# Se imprime las reglas en base al soporte
sop<-sort(x = reglas, decreasing = TRUE, by = "support") #ordena regla


inspect(head(con,10)) #Muestra las 8 reglas com mayor confianza 
inspect(head(sop,10)) #Muestra las 8 reglas com mayor soporte

#Se aplican reglas
regla1 <- mushrooms %>% filter(edibility == "edible" & odor == "none" ) 
summary(regla1)
regla2 <- mushrooms %>% filter(edibility == "edible" & gill_size == "broad" & stalk_surface_above_ring == "smooth") 
summary(regla2)
regla7 <- mushrooms %>% filter(edibility == "poisonous" & bruises == "no" & gill_spacing == "close") 
summary(regla7)

#Ser observa en cada summary, que existen 3 atributos que se repetían tanto en la reglas comestibles como en la venenosa.
#Al no aportar información se eliminan del data ser
mushrooms <- mushrooms %>% select(- gill_attachement, - veil_color, - ring_number)

# Usamos nuevamente el algoritmo apriori para crear las posibles reglas
reglas = apriori(data = mushrooms, parameter = list(support = 0.35, minlen = 1, maxlen = 4),
                 appearance = list(rhs = c("edibility=poisonous", "edibility=edible")))
#Se usa la función inspect para imprimir todas las reglas
# Se imprime las reglas en base a la confianza
inspect(sort(x = reglas, decreasing = TRUE, by = "confidence"))
# Se imprime las reglas en base al soporte
inspect(sort(x = reglas, decreasing = TRUE, by = "support"))




