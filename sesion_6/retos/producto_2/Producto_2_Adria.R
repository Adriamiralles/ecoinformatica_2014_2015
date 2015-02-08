###PRODUCTO 2###

#El producto final será un mapa o una tabla que muestre a qué grupo pertenece cada píxel 
#del mapa de distribución del robledal en Sierra Nevada. Es decir, se indicarán los distintos “tipos” de robledal en función 
#de las variables biofísicas utilizadas en la clasificación.

#Establecemos primero el directorio en nuestra carpeta
setwd("~/")

#Paquetes a cargar

library (stats)

robles <- read.csv (file.choose(), header = TRUE, sep = ",", dec=".") #Abrimos los datos de los robles. Para hacerlo mas fácil, sin problemas de directorio, usamos la función file.choose.

v_ambientales<- subset(robles, select=-c(x,y)) #Sacamos la columna de "x" y "y" ya que no son variables ambientales

n_cluster<-3 #Variable con el número de clusters que estamos usando.

cluster<-kmeans(v_ambientales,n_cluster, iter.max=200) # Ejecutamos la funcion kmeans para hacer las agrupaciones con los clusters

resultado<-subset(robles,select=c(x,y)) #Ahora hacemos lo contrario que al principio, con el subset solo seleccionamos las columnas "y" y "x"

resultado # Visualizamos el resultado

cluster #Abrimos la variable Cluster y vemos las agrupaciones y que componentes hay. 

resultado<-cbind(resultado, cluster$cluster) #Juntamos como columnas la variable resultado y el componente 1 (cluster), donde hay los resultados de agrupar los datos con el nº de clusters elegidos

resultado #Visualizamos resultado 

str(resultado) #Visualizamos resultado 

colnames(resultado)[3]<-"cluster" #Paso necesario para poder realizar el plot


#Creamos y pintamos el mapa

## variable con el número de cluster con el que estamos probando
n_cluster <- 3

#Cargamos los paquetes necesarios para la creación del mapa
library(sp)
library(rgdal)
library(classInt)
library(RColorBrewer)

## definimos las coordenadas de los puntos

coordinates(resultado) =~x+y

## definimos el sistema de coordenadas WGS84
proj4string(resultado)=CRS("+init=epsg:23030")

## obtenemos n_cluster colores para una paleta de colores que se llama "Spectral", para cada cluster creado
plotclr <- rev(brewer.pal(n_cluster, "Spectral"))

## plot, asignando el color en función del cluster al que pertenece
plot(resultado, col=plotclr[resultado$cluster], pch=19, cex = .200, main = "Mapa de grupos de roble")



#Posible otra forma para encontrar los clusters más adecuados para nuestros datos

#Vamos a utilizar una función del paquete vegan para ver cual es el nº de clusters mas adecuados para nuestros datos
library(vegan)
fit <- cascadeKM(scale(v_ambientales, center = TRUE,  scale = TRUE), 1, 10, iter = 1000) #La 
fit
plot(fit, sortg = TRUE, grpmts.plot = TRUE)
calinski.best <- as.numeric(which.max(fit$results[2,]))
calinski.best
cat("Calinski criterion optimal number of clusters:", calinski.best, "\n")

# El resultado obtenido es de 2, pero en el método que hemos utilizado para crear el mapa no nos deja meter un cluster = 2.



####### Se han usado 3 clusters porqué al realizar el algoritmo probando con clusters = 3,4,5, el 3 es el que mejor representaba los
robledales ya que se ve en los mapas obtenidos que hay unas zonas que tanto en 3,4,5 se conservan y otras que en 4 y 5 se van
fragmentando y quizá no representen los diferentes tipos de robledales al ir aumentando a medida que aumentamos los clusters.
