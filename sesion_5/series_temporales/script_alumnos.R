
# Load packages
# install.packages(c('Kendall','wq'))
library('Kendall')  # For Kendall analysis
library('wq')       # For Kendall and Theil-Sen Slope analysis 
library('ggplot2')  # For advanced plots 
library('reshape2') # Manipule data
library('zoo')      # Temporal series 

# set working directory 
di <- '/Users/ajpelu/ownCloud/myrepos/ts_CAdrought'
setwd(di)

#####################################################
##### Trend analysis Annual SCPDSI ##################

# Read data
anual <- read.table('http://www.iecolab.es/ecoinfo/scpdsi_annual.csv', header = TRUE, sep=';')

robles <- read.table('/ndvi_robledal.csv',  header = TRUE, sep=';')

raster <- raster (nieve)

########## PRODUCTO 1

getwd() # Directorio actual


ndvi <- read.table (file.choose(), header = TRUE, sep=';')
nieve <- read.table (file.choose(), header = TRUE, sep';')



Kendall <- MannKendall(
  
  ndvi$ndvi_i, margins=TRUE)







# Explore data EJERCICIO
str(ndvi)
head(ndvi)

str (nieve)
head(nieve)

# Exploratory plot  EJERCICIO
plot(ndvi$ano, ndvi$ndvi_i, type='o',  xlab='Años', pch=19, col='#325B84', ylab='ndvi', ylim=c(0,1))




### Para el siguiente fragmento de script, suponemos que:
### - Tienes una variable que se llama tendencias
### - La variable tendencias es un data.frame
### - que el data.frame tiene una columna que se llama tau
### - que el data.frame tiene una columna que se llama pvalue
### - que tiene dos columnas, lat y lng

library(sp)
library(rgdal)
library(classInt)
library(RColorBrewer)
library(raster)


datanvdi <-data.frame (ndvi)
datanvdi

## definimos las coordenadas de los puntos
coordinates(datanvdi) =~lng+lat
## definimos el sistema de coordenadas WGS84
proj4string(datanvdi)=CRS("+init=epsg:4326")

## partimos los valores de tau en 5 clases
clases <- classIntervals(datanvdi$ndvi_i, n = 5)
## obtenemos cinco colores para una paleta de colores que se llama "Spectral"
plotclr <- rev(brewer.pal(5, "Spectral"))
## Asociamos los valores de tau a su valor correspondiente
colcode <- findColours(clases, plotclr)


## plot sin tener en cuenta
plot(datanvdi, col=10, pch=19, cex = .6, main = "Mapa de tendencias")
## mostramos la leyenda
legend("topright", legend=names(attr(colcode, "table")), fill=attr(colcode, "palette"), bty="n")

###### Otra forma de pintar el mapa

datanvdi$significativa <- ifelse(datanvdi$pvalue < 0.05, 1, 2)
## plot sin tener en cuenta
plot(tendencias, col=colcode, pch=c(17, 19)[as.numeric(tendencias$significativa)], cex = .6, main = "Mapa de tendencias")
## mostramos la leyenda
legend("topright", legend=names(attr(colcode, "table")), fill=attr(colcode, "palette"), bty="n")





















### Run the Trend analysis
m <- MannKendall(ndvi$ndvi_i)
m



##Probamos de hacer una matrix









### Test a linear regression
ml <- lm(anual$year~anual$scpdsi)
summary(ml)

# Plot residuals 
par(mfrow=c(2,2))
plot(ml)
#####################################################


#####################################################
##### Trend analysis Monthly SCPDSI #################

# Read data
mensual <- read.table('http://www.iecolab.es/ecoinfo/scpdsi_monthly.csv', header = TRUE, sep=',')

# Explore monthly data
str(mensual)
head(mensual)

# Manipule data 
mensuales <- dcast(mensual, year ~ month, value.var = 'value')   
names(mensuales) <- c('year','jun','jul','aug','sep')

# Convert data into zoo object 
mizoo <- zoo(mensuales[-1], mensuales[,1])
str(mizoo)

# Exploratory plot 
plot(mizoo, type='o', pch=19, main='scPDSI index', 
	col='#325B84', cex.lab=2.5, cex.axis=1.5)


### Run the Theil-Sen and Trend analysis
theil <- mannKen(as.ts(mizoo))
theil
#####################################################
