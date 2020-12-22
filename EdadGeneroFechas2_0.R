# Script para todo el proceso de limpieza, creación y enriquecimiento de datos.
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
dir()
cat


# Librerías
library(ggplot2)
library(ggrepel)
library(hrbrthemes)
library(dplyr)
library(tidyr)
library(viridis)
library(reshape2)
library(BBmisc)
library(lubridate)


#########################
##~~~~~~~~~~~~~~~~~~~~~##
##       |DATOS|       ##
##~~~~~~~~~~~~~~~~~~~~~##
#########################


#Edad genero por fechas#

datos_edad = read.csv("Datos/09.csv",stringsAsFactors = FALSE, sep=';')[1:277,1:13]

datos_edad = replace(datos_edad,is.na(datos_edad),0)

names(datos_edad)[1] <- "fecha"
names(datos_edad)[2] <- "positivosM"
names(datos_edad)[3] <- "ositivosF"
names(datos_edad)[4] <- "0-9"
names(datos_edad)[5] <- "10-19"
names(datos_edad)[6] <- "20-29"
names(datos_edad)[7] <- "30-39"
names(datos_edad)[8] <- "40-49"
names(datos_edad)[9] <- "50-59"
names(datos_edad)[10] <- "60-69"
names(datos_edad)[11] <- "70-79"
names(datos_edad)[12] <- "80-89"
names(datos_edad)[13] <- "90+"

edades<-c("0-9", "10-19", "20-29", "30-39","40-49","50-59","60-69","70-79","80-89","90+")



datos_edad$fecha = as.Date(datos_edad$fecha, format = "%Y/%m/%d")
summary(datos_edad)




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

# Añadimos el ratio de PCR positivas.
# Quitamos todas filas que tuvieran NA, ya que al hacer el ratio daría error.
i1 = datos_generales[,c(1,5,6)]
i1 = i1[complete.cases(i1), ]
i1$Fecha = as.Date(datos_positivos$Fecha, format = "%d/%m/%Y")
i1$ratio_positivos = i1$positivos_diarios/ i1$PCR_diarios # Creamos ratios.
i1 = i1[, c(1,4)] # Nos quedamos con fecha y ratio para el merge.
datos_generales = merge(x=datos_generales, y=i1, by='fecha', all.x = T, )
datos_generales = merge(x=datos_generales, y=datos_edad, by='fecha', all.x = T, )

# Sustituimos valores NA por 0 (pensando en los gráficos).
datos_generales = replace(datos_generales,is.na(datos_generales),0)

#######################
##     |CAMBIOS|     ##
#######################

# Normalizar las columnas PCR_diarios y positivos_diarios y añadirlas al dataframe.
for (i in 10:19) {
    
  
  
  datos_generales$cero_nueve_normalizado = normalize(datos_generales[1:(nrow(datos_generales)),i],
                                                      method = "range",
                                                      range = c(0, 100) # De 0 a 100 de manera arbitraria
  )
  
  
  datos_generales$positivos_diarios_normalizado = normalize(datos_generales$positivos_diarios,
                                                            method = "range",
                                                            range = c(0, 100)
  )
  
  # Eliminamos los fines de semana.
  # wday trata los domingos como primer día de la semana, de ahí el 2:6.
  datos_generales = datos_generales[lubridate::wday(datos_generales$fecha) %in% 2:6,]
  
  
  #######################
  ##~~~~~~~~~~~~~~~~~~~##
  ##     |GRÁFICO|     ##
  ##      |FINAL|      ##
  ##~~~~~~~~~~~~~~~~~~~##
  #######################
  # * ADAPTADO A LA NORMALIZACIÓN DE LOS DATOS * #
  
  # Colorintxus.
  color_PCR <- rgb(0.4, 0.6, 0.9, 1)
  color_pos <- "#D62246"
  
  
  ############################
  # * CAMBIO CAMBIO CAMBIO * #
  ############################
  # Dataframe con hitos y fechas
  hitos = data.frame(
    x = c(
      as.Date("16/03/2020", "%d/%m/%Y"), # 16 en lugar de 15 al haber eliminado los fines de semana.
      as.Date("27/03/2020", "%d/%m/%Y"), # Fin actividad no esencial. 27 en lugar de 28.
      as.Date("11/05/2020", "%d/%m/%Y"), # Fase 1. Se puede salir a la calle.
      as.Date("25/05/2020", "%d/%m/%Y"), # Fase 2.
      as.Date(" 8/06/2020", "%d/%m/%Y"), # Fase 3.
      as.Date("19/06/2020", "%d/%m/%Y"), # Fin estado de alarma.
      as.Date("01/07/2020", "%d/%m/%Y"), # Apertura al turismo.
      as.Date("28/07/2020", "%d/%m/%Y"), # Uso obligatorio de mascarillas
      as.Date("07/09/2020", "%d/%m/%Y"), # Comienzo clases
      as.Date("28/09/2020", "%d/%m/%Y"), # Toque de queda. 28 en lugar de 26. +3 por formato.
      as.Date("09/11/2020", "%d/%m/%Y") # Cierre hostelería. 9 en lugar de 7.
    ),
    y = c(
      77,77,77,77,77,77,77,77,77,77,50
    ),
    label = c(
      "Inicio confinamiento",
      "Cese actividad no esencial",
      "Desescalada, fase 1",
      "Fase 2",
      "Fase 3",
      "Fin estado de alarma",
      "Turismo internacional",
      "Uso obligatorio de mascarillas",
      "Comienzo de las clases",
      "Toque de queda y\ncierre de la comunidad",
      "Cierre de la hostelería"
    )
  )
  
  dates_vline = which(datos_generales$fecha %in% hitos$x) # Para que luego aparezcan en el gráfico.
  
  
  MiPLot = ggplot(datos_generales, aes(x=fecha)) +
    
    geom_line( aes(y=cero_nueve_normalizado), size=1.5, color=color_PCR) + 
    geom_line( aes(y=positivos_diarios_normalizado), size=1.5, color=color_pos) +
    
    scale_y_continuous(
      # Features of the first axis
      name = i,
      
      # Add a second axis and specify its features
      sec.axis = sec_axis(~.*1, name="Positivos diarios")
    ) +
  ############################
  # * CAMBIO CAMBIO CAMBIO * #
  ############################
  # Mejorada la escala del eje X --> se ve mes a mes y con el nombre en lugar del número.
  scale_x_date(
    date_labels = "%b",
    date_breaks = "1 months"
  ) +
    theme_ipsum() +
    theme(
      axis.title.y = element_text(color = color_PCR, size=15),
      axis.title.y.right = element_text(color = color_pos, size=15)
    ) +
    ggtitle("Pruebas PCR y número de positivos darios (datos normalizados)") +
  ############################
  # * CAMBIO CAMBIO CAMBIO * #
  ############################
  geom_vline( xintercept = as.numeric(datos_generales$fecha[dates_vline]),
              col = "black", lwd=0.5, linetype = "longdash") + 
    annotate(geom="text", x = hitos$x+3,
             y=hitos$y,
             label=hitos$label,
             color="black",
             angle = 90
    )
}
MiPLot

