#Laboratorio 4 - Análisis de Datos
#Integrantes:
#            -Nicolás López
#            -Alberto Rodríguez
library("C50")
library("caret")
library('dplyr')

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

# Semilla
set.seed(88)
#Se guarda bd original, por si en necesaria en el futuro
original<-mushrooms 

#Usamos summary para calcular la frecuencia de cada variables categórica
summary(mushrooms)
# Se puede observar que el atributo veil-type 
#solo tiene un posible valor, entonces se elimina
#Eliminamos atributo innecesario (solo tiene un valor posible)
mushrooms <- mushrooms %>% select(- veil_type)

#Tambien notamos que existen 3 atributos, los cuales tenián casi un 100% de un solo valor, por lo que
#al nno aportar información se eliminan del data set
mushrooms <- mushrooms %>% select(- gill_attachement, - veil_color, - ring_number)

#Se coloca el atributo class en la última posición
mushrooms <- mushrooms %>% select(-edibility,edibility)

##########OBTENCIÓN DEL ÁRBOL############

#Se crea la muestra de pruebas y la de entrenamiento
training.index = createDataPartition(mushrooms$edibility, p=0.7)$Resample1
training.set <- mushrooms[training.index, ]
test.set <- mushrooms[-training.index, ]

#Se genera el arbol y las reglas asociadas a este 
tree = C5.0(edibility ~ ., training.set)
tree.rules <- C5.0(x = training.set[, -19], y = training.set$edibility, rules = T)

#Se prueba el arbol con la muestra de pruebas
tree.pred.class <- predict(tree, test.set[,-19], type = "class")
tree.pred.prob <- predict(tree, test.set[,-19], type = "prob")

#Obtener la información
head(tree.pred.prob)
plot(tree)
summary(tree)
summary(tree.rules)
conf.matrix.tree = confusionMatrix(table(test.set$edibility, tree.pred.class))
print(conf.matrix.tree)  


