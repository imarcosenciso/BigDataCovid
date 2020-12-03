# Script para todo el proceso de limpieza, creación y enriquecimiento de datos.
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
dir()
cat


# Librerías


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

summary(datos_generales)
