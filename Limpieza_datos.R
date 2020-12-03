# Script para todo el proceso de limpieza, creación y enriquecimiento de datos.
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
dir()
cat


# Librerías


datos = read.csv("Datos/covid19-pcr-positives.csv",stringsAsFactors = FALSE)
datos$date = as.POSIXct(datos$date,format = "%Y-%m-%d")




summary(datos)

print(result)
