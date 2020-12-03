# Script para todo el proceso de limpieza, creación y enriquecimiento de datos.
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
dir()
cat


# Librerías
library(tidyverse)

# Datos generales
datos_generales = read.csv("Datos/Generales.csv",stringsAsFactors = FALSE, sep=';')

# Nos quedamos con las columnas que nos interesa.
datos_generales = subset( datos_generales,
                          select = c(Fecha,
                                     PCR.totales,
                                     Casos.positivos.PCR,
                                     Hospitalizados.en.UCI)
                          )
# Cambiamos la columna fecha de formato String a Date.
datos_generales$Fecha = as.Date(datos_generales$Fecha, format = "%d/%m/%Y")
datos_generales$PCR_diarios = NA
datos_generales[1,5] = datos_generales[1, 2]
for (i in 1:nrow(datos_generales)) { 
  prim = datos_generales[i, 2]
  sec = datos_generales[i+1, 2]
  res = sec - prim
  
  datos_generales[(i+1),5] = res
  
}

summary(datos_generales)
