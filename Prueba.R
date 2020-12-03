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

# Cambiamos la columna fecha de formato String a Date y renombramos columnas.
datos_generales$Fecha = as.Date(datos_generales$Fecha, format = "%d/%m/%Y")
names(datos_generales)[names(datos_generales) == "Fecha"] <- "fecha"
names(datos_generales)[names(datos_generales) == "PCR.totales"] <- "PCR_acumuladas"
names(datos_generales)[names(datos_generales) == "Casos.positivos.PCR"] <- "positivos_acumulados"
names(datos_generales)[names(datos_generales) == "Hospitalizados.en.UCI"] <- "diarios_en_UCI"


# Queremos enriquecer el dataset con datos diarios, además de los acumulados.
# Para ello, creamos las dos nuevas columnas 'PCR_diarias' y 'positivos_diarios'.
# La primera columna la calculamos. La segunda la obtenemos de otro CSV (sin bucle for).

# Para 'PCR_diarias', no nos queda otra que obtener el numero iterando sobre cada fila y calculándolo.
datos_generales$PCR_diarios = NA
datos_generales[1,5] = datos_generales[1, 2]
for (i in 1:nrow(datos_generales)) { 
  datos_generales[(i+1),5] = datos_generales[i+1, 2] - datos_generales[i, 2]
}

# Para 'positivos_diarios' seguimos un procedimiento parecido al anterior dataset.
datos_positivos = read.csv("Datos/Casos_positivos.csv",stringsAsFactors = FALSE, sep=';')
datos_positivos = subset(datos_positivos,select = c(Fecha,Casos))
datos_positivos$Fecha = as.Date(datos_positivos$Fecha, format = "%d/%m/%Y")
names(datos_positivos)[names(datos_positivos) == "Fecha"] <- "fecha"
names(datos_positivos)[names(datos_positivos) == "Casos"] <- "positivos_diarios"
datos_generales = merge(x=datos_generales, y=datos_positivos, by='fecha', all.x= T)

datos_generales = datos_generales[-c(284), ] # Quitamos la fila extra que se crea.

i1 = is.na(datos_generales$positivos_diarios)
datos_generales[i1, "ratio_positivos"] = datos_generales$positivos_diarios[i1] + datos_generales$PCR_diarios[i1]

summary(datos_generales) 
