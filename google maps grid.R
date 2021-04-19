latitud_inicial <- 19.366502
longitud_inicial <- -99.255025
    
latitud_final <-  19.341319
longitud_final <- -99.204789

distancia <- 0.1 #km

radio_tierra <- 6371 #km

suma_latitud<- function(latitud, distancia) {
  nueva_latitud <- latitud  - (distancia / radio_tierra) * (180 / pi)
  return(nueva_latitud)
}

suma_longitud<- function(longitud, distancia) {
  nueva_longitud <- longitud + (distancia / radio_tierra) * (180 / pi)
  return(nueva_longitud)
}

latitude <- c()
longitude <- c()


while (latitud_inicial > latitud_final) {
  longitud_inicial <- -99.255025
  while (longitud_inicial < longitud_final) {
    longitude <- append(longitude, longitud_inicial)
    latitude <- append(latitude, latitud_inicial)
    longitud_inicial <- suma_longitud(longitud_inicial, distancia)
  }
  
  latitud_inicial <- suma_latitud(latitud_inicial, distancia)
}

datos_finales <- data.frame(longitude,latitude)
write.csv(datos_finales,'grid.csv')


################################
#   Genera puntos centrales
# No se usa despues del anterior, sino despues de limpiar el csv del grid manualmente

dat <- read.csv("grid2.csv",header=T)
longitude <- dat[["longitude"]]
latitude <- dat[["latitude"]] 

#genera coordenadas de puntos centrales
longitude <- longitude + (suma_longitud(longitude, distancia) - longitude)/2
latitude <- latitude + (suma_latitud(latitude, distancia) - latitude)/2

#genera nombres para puntos centrales
LETRAS <- LETTERS
LETRAS <- append(LETRAS, "AA")
LETRAS <- append(LETRAS, "AB")
nombres <- c()
numero <- 1
letra <- 1
for(i in (1:(length(latitude)-1))){
  if(latitude[i] != latitude[i+1]){
    numero <- 1
    letra <- letra + 1
    next
  }
  nombres[i] <- paste(LETRAS[letra],numero,sep="")
  numero <- numero + 1
}

#agrega un valor para que coincidan las coordenadas
nombres <- append(nombres,"XXX")

datos_finales <- data.frame(nombres,longitude,latitude)
write.csv(datos_finales,'nombres.csv')


