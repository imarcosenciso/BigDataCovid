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

#Edad genero por fechas#

datos_generales = read.csv("Datos/09.csv",stringsAsFactors = FALSE, sep=';')[1:277,1:13]

datos_generales = replace(datos_generales,is.na(datos_generales),0)

names(datos_generales)[1] <- "Fecha"
names(datos_generales)[2] <- "PositivosM"
names(datos_generales)[3] <- "PositivosF"
names(datos_generales)[4] <- "0-9"
names(datos_generales)[5] <- "10-19"
names(datos_generales)[6] <- "20-29"
names(datos_generales)[7] <- "30-39"
names(datos_generales)[8] <- "40-49"
names(datos_generales)[9] <- "50-59"
names(datos_generales)[10] <- "60-69"
names(datos_generales)[11] <- "70-79"
names(datos_generales)[12] <- "80-89"
names(datos_generales)[13] <- "90+"

datos_generales$Fecha = as.Date(datos_generales$Fecha, format = "%Y/%m/%d")
summary(datos_generales)



#########################
#       NORMALIZAR      #
#########################
datos_generales$cero_nueve_norm = normalize(datos_generales$"0-9",
                                                    method = "range",
                                                    range = c(0, 100) # De 0 a 100 de manera arbitraria
)
datos_generales$cero_nueve_norm<-as.numeric(as.character(datos_generales$cero_nueve_norm))
summary(datos_generales)
##########################
# QUITAR FINES DE SEMANA #
##########################
datos_generales = datos_generales[lubridate::wday(datos_generales$Fecha) %in% 2:6,]



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

dates_vline = which(datos_generales$Fecha %in% hitos$x) # Para que luego aparezcan en el gráfico.

ggplot(datos_generales, aes(x=Fecha)) +

  geom_line( aes(y=cero_nueve_norm), size=1.5, color=color_PCR) +
  scale_y_discrete(
   name = "PCR diarios",
  ) +

  # Mejorada la escala del eje X --> se ve mes a mes y con el nombre en lugar del número.
  scale_x_date(
    date_labels = "%b",
    date_breaks = "1 months"
  ) +
  theme_ipsum() +
  theme(
    axis.title.y = element_text(color = color_PCR, size=15),
    #axis.title.y.right = element_text(color = color_pos, size=15)
  ) +
  ggtitle("Pruebas PCR y número de positivos darios (datos normalizados)") +
  ############################
# * CAMBIO CAMBIO CAMBIO * #
############################
  geom_vline( xintercept = as.numeric(datos_generales$Fecha[dates_vline]),
            col = "black", lwd=0.5, linetype = "longdash") + 
  annotate(geom="text", x = hitos$x+3,
           y=hitos$y,
           label=hitos$label,
           color="black",
           angle = 90
  )
  
  