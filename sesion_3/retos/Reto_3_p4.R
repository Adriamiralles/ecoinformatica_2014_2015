## Dado el conjunto de datos ndvi.zip. 
## Cada imagen raster se tomó un día, a una hora y minutos concretos. 
## Queremos hacer un script que muestre la evolución del NDVI medio para las horas del día.

#Cargamos los paquetes necesarios para realizar el ejercicio
library(raster) .
library (sp) 
library (rgdal) 

#Establecemos primero el directorio en nuestra carpeta

horas <- c("12", "13", "14", "15") #Creamos el vector de las horas
v0 <- c() # Creamos el vector vacío
for (valores in horas) { # Para los valores de horas haz todo lo de dentro del bucle
  imagenes <- list.files(path="./ndvi", full.names = TRUE,pattern=paste("_", valores, "..\\.jpg\\.asc$", sep="")) #Abrimos las imagenes del ndvi del directorio elegido
  apilado <- stack (imagenes) # Hacemos un apilado de las tres imagénes de cada hora
  media <- mean (apilado) #Hacemos una media del apilado de cada hora
  mediaunica <- cellStats(media, stat='mean') #Obtenemos un único valor medio de cada hora
  v0 <- rbind (v0, mediaunica) #El vector vacío acumula las mediaunicas de cada hora
} #Fin del bucle


plot (horas, v0, type = "o", xlab="Horas", ylab="NDVI medio", main =" Evolución del NVDI medio de las 12:00 a las 15:00") #Hacemos un gráfico de las medias de cada hora


