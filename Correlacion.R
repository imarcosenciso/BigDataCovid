# Script para generar el gráfico de correlación
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
dir()
cat

# Librerías
library(stringr)
library(dplyr)
library(corrplot)

# Factores que se tendrán en cuenta para la correlación:
#   - población (06.csv)
#   - ratio de positivos por cada 100k habitantes (06.csv)
#   - nº total de fallecidos (06.csv)
#   - fatalidad (%fallecidos por infectados) (06.csv)
#   - densidad de población 
#   - densidad de viviendas en suelo residencial
#   - si el municipio cuenta con hospital
#   - tiempo medio al hospital de referencia
#   - plazas de alojamiento turistico
#   - establecimientos de bebidas


#########################
##~~~~~~~~~~~~~~~~~~~~~##
##       |DATOS|       ##
##~~~~~~~~~~~~~~~~~~~~~##
#########################

# Covid por municipio
datos_06 = read.csv("Datos/06.csv",stringsAsFactors = FALSE, sep=';')
datos_06 = subset(datos_06,
                  select = c(1, 3, 4, 6)
                  )
names(datos_06)[1] <- "municipio"
names(datos_06)[2] <- "poblacion"
names(datos_06)[3] <- "tasa_infectados"
names(datos_06)[4] <- "fatalidad"

datos_06 = datos_06[-c(nrow(datos_06)), ] # Quitamos la última fila.
datos_06$municipio = tolower(datos_06$municipio)

# Formato de los doubles...
datos_06$poblacion = as.double(datos_06$poblacion)
datos_06$tasa_infectados = as.double(str_replace(datos_06$tasa_infectados, "," , "."))
datos_06$fatalidad = as.double(str_replace(datos_06$fatalidad, "," , "."))



# Datos generales del municipio
# Densidad de población
densidad_poblacion = read.csv("Datos/Municipios/Densidad_poblacional.csv",stringsAsFactors = FALSE, sep=';')
densidad_poblacion = subset(densidad_poblacion,
                  select = c(1, 2, 3)
)
names(densidad_poblacion)[1] <- "cod_municipio"
names(densidad_poblacion)[2] <- "municipio"
names(densidad_poblacion)[3] <- "hab_km"

densidad_poblacion$hab_km = gsub(".", "", densidad_poblacion$hab_km, fixed = TRUE) # str_replace no funcionaba...
densidad_poblacion$hab_km = as.double(str_replace(densidad_poblacion$hab_km, "," , "."))


# Densidad de viviendas en suelo residencial
densidad_viviendas = read.csv("Datos/Municipios/Densidad_viviendas_suelo_residencial.csv",stringsAsFactors = FALSE, sep=';')
densidad_viviendas = subset(densidad_viviendas,
                            select = c(1, 2, 3)
)
names(densidad_viviendas)[1] <- "cod_municipio"
names(densidad_viviendas)[2] <- "municipio"
names(densidad_viviendas)[3] <- "densidad_residencial"

densidad_viviendas$densidad_residencial = as.double(str_replace(densidad_viviendas$densidad_residencial, "," , "."))


# Establecimientos de bebidad por población
densidad_bares = read.csv("Datos/Municipios/Establecimientos_bebidas.csv",stringsAsFactors = FALSE, sep=';')
densidad_bares = subset(densidad_bares,
                            select = c(1, 2, 3)
)
names(densidad_bares)[1] <- "cod_municipio"
names(densidad_bares)[2] <- "municipio"
names(densidad_bares)[3] <- "densidad_bares"

densidad_bares$densidad_bares = as.double(str_replace(densidad_bares$densidad_bares, "," , "."))


# Establecimientos de hostelería y restauración
densidad_hosteleria = read.csv("Datos/Municipios/Establecimientos_hosteleria_y_restauracion.csv",stringsAsFactors = FALSE, sep=';')
densidad_hosteleria = subset(densidad_hosteleria,
                        select = c(1, 2, 3)
)
names(densidad_hosteleria)[1] <- "cod_municipio"
names(densidad_hosteleria)[2] <- "municipio"
names(densidad_hosteleria)[3] <- "densidad_hosteleria"

densidad_hosteleria$densidad_hosteleria = as.double(str_replace(densidad_hosteleria$densidad_hosteleria, "," , "."))

# Tiempo medio en llegar al hospital
t_medio_hospital = read.csv("Datos/Municipios/t_medio_hospital.csv",stringsAsFactors = FALSE, sep=';')

names(t_medio_hospital)[1] <- "cod_municipio"
names(t_medio_hospital)[2] <- "municipio"
names(t_medio_hospital)[3] <- "t_medio"

t_medio_hospital$t_medio = as.double(str_replace(t_medio_hospital$t_medio, "," , "."))


# Merge de datos
datos_generales = merge(x=densidad_poblacion, y=densidad_viviendas, by=c('cod_municipio',"municipio"))
datos_generales = merge(x=datos_generales, y=densidad_hosteleria, by=c('cod_municipio',"municipio"))
datos_generales = merge(x=datos_generales, y=densidad_bares, by=c('cod_municipio',"municipio"))
datos_generales = merge(x=datos_generales, y=t_medio_hospital, by=c('cod_municipio',"municipio"))

datos_generales$municipio = tolower(datos_generales$municipio)
datos_generales_merged = merge(x=datos_generales, y=datos_06, by='municipio')


datos_generales_merged = subset(datos_generales_merged, select = -c(1) )

# Era obvio que iba a haber problemas con este merge: datos_06 tiene 227 municipios,
# mientras que datos_generales tiene 251. Por tanto, el máximo en común que podrán
# tener es de 227. datos_generales_merged tiene 210, por lo que habría que cambiar 
# a mano el nombre de los 17 municipio que no han sido incluidos en el merge, con el 
# objetivo de tener la mejor representación de datos posible.
# anti_join(datos_generales,datos_06)[2]
# Así obtenemos los municipios disjuntos, que hay que comparar con los de datos_06,
# para reescribir el nombre, que este coincida, y coger toda la información.


# Mientras tanto, hacemos la matriz de correlación con lo que tenemos:
M = cor(datos_generales_merged[,-1])
corrplot(M, method="pie")

# Entre 0-25%
dgm_25 = datos_generales_merged[datos_generales_merged$poblacion
                       <= quantile(datos_generales_merged$poblacion)[[2]]
                       ,]
M = cor(dgm_25[,-1])
corrplot(M, method="pie")


# Entre 25-50%
dgm_50 = datos_generales_merged[datos_generales_merged$poblacion
                       > quantile(datos_generales_merged$poblacion)[[2]] &
                        datos_generales_merged$poblacion
                       <= quantile(datos_generales_merged$poblacion)[[3]]
                       ,]

# Entre 50-75%
dgm_75 = datos_generales_merged[datos_generales_merged$poblacion
                       > quantile(datos_generales_merged$poblacion)[[3]] &
                         datos_generales_merged$poblacion
                       <= quantile(datos_generales_merged$poblacion)[[4]]
                       ,]

# Entre 75-100%
dgm_100 = datos_generales_merged[datos_generales_merged$poblacion
                       > quantile(datos_generales_merged$poblacion)[[4]]
                       ,]

M = cor(dgm_25[,-1])
corrplot(M, method="pie")

M = cor(dgm_50[,-1])
corrplot(M, method="pie")

M = cor(dgm_75[,-1])
corrplot(M, method="pie")

M = cor(dgm_100[,-1])
corrplot(M, method="pie")
