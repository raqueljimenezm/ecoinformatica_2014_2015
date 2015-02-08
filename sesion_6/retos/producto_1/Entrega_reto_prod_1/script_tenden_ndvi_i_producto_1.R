# Establezco mi directorio de trabajo.
setwd("~/ecoinformatica_2014_2015/sesion_6/retos/producto_1")

library(Kendall)

# Asigno un nombre a mi tabla de tados de ndvi_robledal.
datos<-read.csv("ndvi_robledal.csv", sep=";")
str(datos)
head(datos)

# La variable que me interesa es "ndvi_i. 
# Queremos los valores de "tau" y "pvalor" para ver si la tendencia es ascendente/descendente y si es significativa o no.
# Para ello, creo una tabla que recoge los valores de: indicador del píxel, valores de "tau" y valores de "pvalor".
mk <- data.frame()

# Necesito también otra tabla vacía donde se irán guardando los datos finales para la representación en el mapa.
mk_aux <- data.frame(iv_malla_modi_id=NA,
                     tau=NA,
                     pvalue=NA)
view(mk_aux)

pixeles <- unique(datos$iv_malla_modi_id)

for (k in pixeles){ 
  # Subset de los valors de ndvi por año para cada pixel.
  aux <- datos[datos$iv_malla_modi_id==k,]
  
  # mann-kendall para el subset 
  m <- MannKendall(aux$ndvi_i) 
  
  mk_aux$iv_malla_modi_id <- k # identificador del pixel 
  mk_aux$tau <-m$tau[1]# tau
  mk_aux$pvalue <- m$sl[1] # pvalue
  
  mk <- rbind(mk, mk_aux)
} 

plot(mk$tau)

library(plyr)

# Selecciono las columnas que me interesan: "ndvi_i", "lat" y "lng" para pintarlas en el mapa final.
datos1 <- datos[,c(1,4:5)]
coord_pixel <- unique(datos1)

#cosa = tendencia

cosa <- join(mk, coord_pixel, "iv_malla_modi_id")

## Pintamos mapa para tendencia del ndvi_i

library(sp)
library(rgdal)
library(classInt)
library(RColorBrewer)

## definimos las coordenadas de los puntos
coordinates(cosa) =~lng+lat
## definimos el sistema de coordenadas WGS84
proj4string(cosa)=CRS("+init=epsg:4326")

## partimos los valores de tau en 5 clases
clases <- classIntervals(cosa$tau, n = 5)
## obtenemos cinco colores para una paleta de colores que se llama
"Spectral"
plotclr <- rev(brewer.pal(5, "Spectral"))
## Asociamos los valores de tau a su valor correspondiente
colcode <- findColours(clases, plotclr)

plot(cosa, col=colcode, pch=19, cex = .6, main = "Mapa de tendencias")
## mostramos la leyenda
legend("topright", legend=names(attr(colcode, "table")),
       fill=attr(colcode, "palette"), bty="n")

###### Otra forma de pintar el mapa

cosa$significativa <- ifelse(cosa$pvalue < 0.05, 1, 2)
## plot sin tener en cuenta
plot(cosa, col=colcode, pch=c(19, 20)[as.numeric(cosa$significativa)],
     cex = .6, main = "Mapa de tendencias")
## mostramos la leyenda
legend("topright", legend=names(attr(colcode, "table")),
       fill=attr(colcode, "palette"), bty="n")
