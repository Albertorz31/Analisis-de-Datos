#Laboratorio 2 - Análisis de Datos
#Integrantes:
#            -Nicolás López
#            -Alberto Rodríguez
library(dplyr) 
library(ggplot2)
library(ggpubr)
library(purrr)
library(factoextra)
library(FactoMineR)
library(cluster)
url<-"https://raw.githubusercontent.com/Albertorz31/Analisis-de-Datos/main/agaricus-lepiota.data"

# Leemos los datos
mushrooms <- read.table(url, header = TRUE, sep = ",")
#Semilla: se usa porque como se eligen los centroides al azar, usamos esta funcion para que obtengamos siempre 
#los mismo resultadoss
set.seed(88)

######Transformacion de los datos para poder trabajarlos#######

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

#Se guarda bd original
original<-mushrooms 



######PRE-PROCESAMIENTO######

#Eliminamos el atributo "classes" que lo contiene edibility o como sale escrito "p".
mushrooms <- mushrooms %>% select(- edibility)

#Usamos summary para calcular la frecuencia de cada variables categórica
summary(mushrooms)
# Se puede observar que el atributo veil-type 
#solo tiene un posible valor, entonces se elimina
#Eliminamos atributo innecesario (solo tiene un valor posible)
mushrooms <- mushrooms %>% select(- veil_type)

#Eliminamos todas las filas donde el atributo stalk.root tiene datos faltantes
#mushrooms <- mushrooms[which(mushrooms$e.1 != '?'),]

#Realizamos analisis de componentes multiple
res.mca <- MCA(mushrooms, graph = TRUE)

#Para visualizar la proporción de variaciones retenidas por las diferentes dimensiones.
eig.val <- get_eigenvalue(res.mca)

#Para visualizar los porcentajes de inercia explicados por cada dimensión de MCA
fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 45))+ggtitle("Analisis MCA")

#Se muestran las variables con mas contribucion para el estudio

#Dimension 1
fviz_contrib(res.mca, choice = "var", axes = 1, top = 37)+ggtitle("Contribución de variables en Dimension 1")
#Dimension 2
fviz_contrib(res.mca, choice = "var", axes = 2, top = 37)+ggtitle("Contribución de variables en Dimension 2")


#######OBTENCION DEL CLUSTER############

#Se utliza la distancia de gower
gower.dist <- daisy(mushrooms, metric = c("gower"))
gower.matrix = as.matrix(gower.dist)

#Filas mas similares
similar<-mushrooms[which(gower.matrix == min(gower.matrix[gower.matrix != min(gower.matrix)]), arr.ind = TRUE)[1, ], ]
#Filas menos similares
dissimilar<-mushrooms[which(gower.matrix == max(gower.matrix[gower.matrix != max(gower.matrix)]), arr.ind = TRUE)[1, ], ]


#Se intentó ancho de siluetas, pero este parte del código demora demasiado en ejecutar
#sil_width <- c(NA)
#for(i in 2:21){  
# pam_fit <- pam(gower.dist, diss = TRUE, k = i)  
#  sil_width[i] <- pam_fit$silinfo$avg.width  
#}
#plot(1:21, sil_width,
#     xlab = "Number of clusters",
#     ylab = "Silhouette Width")
#lines(1:21, sil_width)

#Se trabjará con 2 cluster, ya que se sabe que son 2 varolres en clases (comestible y venenoso)
#Entonces se quiere ver, cuantos valores pertenecen a cada grupo (cluster)
k<-2
pam_fit <- pam(gower.dist, diss = TRUE, k)
pam_results <- mushrooms %>%  mutate(cluster = pam_fit$clustering) %>%  group_by(cluster) %>%  do(the_summary = summary(.))
pam_results$the_summary

#Se muestra distribución de cada cluster
library(Rtsne)
tsne_obj <- Rtsne(gower.dist, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering))
ggplot(aes(x = X, y = Y), data = tsne_data) + geom_point(aes(color = cluster))


# Se agrega el atributo cluster al dataframe
mushrooms <- cbind(mushrooms, cluster = pam_fit$cluster, class = original$edibility)

#Separamos los hongos en los que pertenecen al cluster 1 y al 2
cluster_separados<-as.data.frame(table(mushrooms[["cluster"]]))
colnames(cluster_separados)<-c("Cluster","Frecuencia")
p1<-ggbarplot(cluster_separados, x="Cluster",y="Frecuencia",label = TRUE,lab.pos = "out", lab.col = "black",
              fill = "Cluster", palette = "jco", title = "Cantidad de hongos en cada cluster")
p1<-p1 + rotate_x_text(angle=45)
plot(p1)
