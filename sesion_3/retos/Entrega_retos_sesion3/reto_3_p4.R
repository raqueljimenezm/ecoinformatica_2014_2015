# Establezco mi directorio de trabajo.
setwd("~/ecoinformatica_2014_2015/sesion_3/retos")

# Instalo paquete Raster.
# install.packages(c('raster'), dep=T)
library(raster)

# install.packages(c('sp'), dep=T)

horas <- c(12,13,14,15)

# Creo un vector vacío para que me guarde los valores de NDVI.
valores_ndvi <- c()

# Hago el bucle para que me repita la función en cada grupo de imágenes por horas.
for (k in horas){
  # Indico que lea todas las imágenes de una hora concreta.
  imagen_horas <- list.files(path="./ndvi", full.names=TRUE, recursive=TRUE, 
                             pattern=paste("_", k ,"..\\.jpg\\.asc$", sep=""))
  
  # Hacemos el stack para juntar las imágenes de cada hora indicada. 
  stack_imagenes <-stack(imagen_horas)
  
  # Hacemos la media para cada pixel de las imágenes del stack.
  media_pixeles <- mean(stack_imagenes)
  valores_ndvi <- rbind(valores_ndvi, mean(media_pixel[]))
}

# Hago el plot para que genere la gráfica.
plot(valores_ndvi, xlab='Horas', ylab='media NDVI', pch=19, col='red', main='Valor medio de NDVI por cada hora')
