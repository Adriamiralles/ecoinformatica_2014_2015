###PRODUCTO 1###

#El producto final será un mapa que muestre las tendencias en la duración 
#de la nieve para cada píxel en toda la serie temporal (2000-2012).


#Establecemos primero el directorio en nuestra carpeta
setwd("~/")

# Cargamos los paquetes necesarios para el análisis
library(Kendall)
library(raster)
library(plyr) 

#Cálculo de las tendencias para el NDVI

ndvi<-read.csv("ndvi_robledal.csv", sep=";", header=TRUE)


#Creación de dos tablas para poder guardar los datos obtenidos del bucle
tendencia_ndvi<-data.frame()    

aux_tendencia<-data.frame(iv_malla_modi_id=NA,tau=NA,pvalor=NA) 


pixels<-unique(ndvi$iv_malla_modi_id) #Con la función unique se cogen solo los valores del pixel de iv_malla_modi_id

for(i in pixels){   #para los valores(i) de pixels hazme todo lo de dentro del bucle
  aux<-ndvi[ndvi$iv_malla_modi_id==i,]
 
  Kendall<-MannKendall(aux$ndvi_i) #Análisis de MannKendall 
 
  aux_tendencia$iv_malla_modi_id<-i #Obtenemos el ID de los pixels
  
  aux_tendencia$tau<-Kendall[[1]][1] #Obtenemos tau
  
  aux_tendencia$pvalor<-Kendall[[2]][1] #Obtenemos pvalor
   
  tendencia_ndvi<-rbind(tendencia_ndvi,aux_tendencia) #Juntamos los datos
}

#Para los datos de ndvi procesamos los datos para obtener la dataframe para el mapa
ndvi<-ndvi[,c(1,4,5)]
unique_datos<-unique(ndvi)
ndvi_resultado<-join(unique_datos,tendencia_ndvi, by="iv_malla_modi_id") #Combinamos las tablas


## Ahora pintamos y creamos el mapa de ndvi

#Cargamos los paquetes para poder pintar el mapa
library(sp)
library(rgdal)
library(classInt)
library(RColorBrewer)

coordinates(ndvi_resultado) =~lng+lat
proj4string(ndvi_resultado)=CRS("+init=epsg:4326")
clases <- classIntervals(ndvi_resultado$tau, n = 5)
plotclr <- rev(brewer.pal(5, "Spectral"))
colcode <- findColours(clases, plotclr)
plot(ndvi_resultado, col=colcode, pch=19, cex = .6, main = "Mapa de tendencias de ndvi en robledal")
legend("topright",legend=names(attr(colcode, "table")), fill=attr(colcode, "palette"), bty="n")




# Cálculo de las tendencias para la duración de la cobertura de la nieve


nieve<-read.csv("nieve_robledal.csv", sep=";", header=TRUE) #Leemos la tabla de nieve

#Creación de dos tablas para poder guardar los datos obtenidos del bucle
tendencia_nieve<-data.frame()

aux_tendencia<-data.frame(nie_malla_modi_id=NA,tau=NA,pvalor=NA)

pixels<-unique(nieve$nie_malla_modi_id)  #Con la función unique se cogen solo los valores del pixel de nie_malla_modi_id

for(i in pixels){  #Para los valores(i) de pixels hazme todo lo de dentro del bucle
 
  aux<-nieve[nieve$nie_malla_modi_id==i,] 
 
  Kendall<-MannKendall(aux$scd) #Análisis de MannKendall 
 
  aux_tendencia$nie_malla_modi_id<-i  #Obtenemos el ID de los pixels
 
  aux_tendencia$tau<-Kendall[[1]][1]  #Obtenemos tau
 
  aux_tendencia$pvalor<-Kendall[[2]][1] #Obtenemos pvalor
  
  tendencia_nieve<-rbind(tendencia_nieve,aux_tendencia) #Juntamos los datos
}


#Para los datos de cobertura de la nieve, procesamos los datos para obtener la dataframe para el mapa

nieve<-nieve[,c(2,10,11)]
unique_datos<-unique(nieve)
nieve_resultado<-join(unique_datos,tendencia_nieve, by="nie_malla_modi_id") #Combinamos las tablas


## Ahora pintamos y creamos el mapa de nieve

coordinates(nieve_resultado) =~lng+lat
proj4string(nieve_resultado)=CRS("+init=epsg:4326")
clases <- classIntervals(nieve_resultado$tau, n = 5)
plotclr <- rev(brewer.pal(5, "Spectral"))
colcode <- findColours(clases, plotclr)
pdf(file="mi_pdf2.pdf",height=8, width=10)
plot(nieve_resultado, col=colcode, pch=19, cex = .6, main = "Mapa de tendencias de nieve en robledal")
legend("topright",legend=names(attr(colcode, "table")), fill=attr(colcode, "palette"), bty="n")


