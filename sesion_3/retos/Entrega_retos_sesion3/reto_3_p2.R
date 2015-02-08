# Introduzco la cantidad de números según el enunciado, 10.
num<-scan(n=10)

# Establecemos el umbral deseado.
umbral<-20

# La cuenta de los números que superan el umbral comienza en cero.
suma<- (suma=0)

# Script para el bucle. Mientras que el número sea superior al umbrar debe de seguir sumando.
# Cuando el número esté por debajo del umbral, para de contar.
for (valor in num [1:10]){
  if (valor>umbral){
    suma<-suma+1}
}
# Escribe la suma.
print(suma)

