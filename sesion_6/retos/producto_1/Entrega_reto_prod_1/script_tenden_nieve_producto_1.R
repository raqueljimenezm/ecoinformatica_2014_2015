# Establezco mi directorio de trabajo.
setwd("~/ecoinformatica_2014_2015/sesion_6/retos/producto_1")

library(Kendall)
# Asigno un nombre a mi tabla de tados de nieve_robledal.
datosnieve<-read.csv("nieve_robledal.csv", sep=";")
str(datos)
head(datos)

# Queremos analizar la tendencia de la duración de la nieve.
# La variable que me interesa es "scd". 
# Queremos los valores de "tau" y "pvalor" para ver si la tendencia es ascendente/descendente y si es significativa o no.
# Para ello, creo una tabla que recoge los valores de: indicador del píxel, valores de "tau" y valores de "pvalor".mk2 <- data.frame()
mk2 <- data.frame()

# Necesito también otra tabla vacía donde se irán guardando los datos finales para la representación en el mapa.
mk_aux2 <- data.frame(nie_malla_modi_id=NA,
                      tau=NA,
                      pvalue=NA)
view(mk_aux2)

# Tengo datos de píxeles repetidos, por lo que con la función "unique" elimino los repetidos.
pixeles2 <- unique(datosnieve$nie_malla_modi_id)

# Ejecuto la librería para Kendall y así poder realizar el bucle.
for (k in pixeles2){ 
  # Subset de los valores de ndvi por año para cada pixel.
  aux2 <- datos[datosnieve$nie_malla_modi_id==k,]
  
  # mann-kendall para el subset 
  m <- MannKendall(aux2$scd) 
  
  mk_aux2$nie_malla_modi_id <- k # identificador del pixel 
  mk_aux2$tau <-m$tau[1]# tau
  mk_aux2$pvalue <- m$sl[1] # pvalue
  
  mk2 <- rbind(mk2, mk_aux2)
} 

plot(mk2$tau)

library(plyr)

# Selecciono las columnas que me interesan: "scd", "lat" y "lng" para pintarlas en el mapa final.
datos1_2 <- datos2[, c(2, 10:11)]
coord_pixel2 <- unique(datos1_2)

##Ejecuto "join" para el cálculo de la tendencia
#tend_nieve
cosanieve <- join(mk2, coord_pixel2, "nie_malla_modi_id")

# Pintamos mapa

library(sp)
library(rgdal)
library(classInt)
library(RColorBrewer)

## definimos las coordenadas de los puntos
coordinates(cosanieve) =~lng+lat 
## definimos el sistema de coordenadas WGS84
proj4string(cosanieve)=CRS("+init=epsg:4326")

## partimos los valores de tau en 5 clases
clases2 <- classIntervals(cosanieve$tau, n = 5)
## obtenemos cinco colores para una paleta de colores que se llama
"Spectral"
plotclr2 <- rev(brewer.pal(5, "Spectral"))
## Asociamos los valores de tau a su valor correspondiente
colcode2 <- findColours(clases2, plotclr2)

plot(cosanieve, col=colcode, pch=19, cex = .6, main = "Mapa de tendencias")
## mostramos la leyenda
legend("topright", legend=names(attr(colcode, "table")),
       fill=attr(colcode, "palette"), bty="n")

###### Otra forma de pintar el mapa

cosanieve$significativa <- ifelse(cosanieve$pvalue < 0.05, 1, 2)
## plot sin tener en cuenta
plot(cosanieve, col=colcode, pch=c(19, 20)[as.numeric(cosanieve$significativa)],
     cex = .6, main = "Mapa de tendencias")
## mostramos la leyenda
legend("topright", legend=names(attr(colcode, "table")),
       fill=attr(colcode, "palette"), bty="n")
