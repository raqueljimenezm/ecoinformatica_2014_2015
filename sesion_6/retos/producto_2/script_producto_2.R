# Selecciono mi directorio de trabajo.
setwd("~/ecoinformatica_2014_2015/sesion_6/retos/producto_2")

# Abro y miro mis datos de robles_ecoinfo.csv
robles<-read.csv("robles_ecoinfo.csv", header = T, sep = ",", dec=".")

# No incluyo las dos primeras columnas "x" e "y" para el cluster.
variables<- subset(robles, select=-c(x,y))

#Defino el número de clusters (o grupos) que quiero hacer de los píxeles
n_cluster<-3
cluster<-kmeans(variables,n_cluster, iter.max=200, nstart=3)
cluster[[1]]
cluster$size

resultado<-subset(robles,select=c(x,y))
head(resultado)

resultado<-cbind(resultado, cluster[[1]])
head(resultado)
str(resultado)

# Defino el nombre de la columna con los valores del cluster
colnames(resultado)[3]<-"cluster"
head(resultado)

n_cluster <- 3 

# Pinto el mapa
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
plot(resultado, col=plotclr[resultado$cluster], pch=19, cex = .6, main = "Mapa de grupos de roble")

# Añado la leyenda
legend("topright", legend=names(attr(colcode, "table")),
       fill=attr(colcode, "palette"), bty="n")
