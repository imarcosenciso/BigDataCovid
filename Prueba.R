# Script para todo el proceso de limpieza, creación y enriquecimiento de datos.
rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
dir()
cat


# Librerías
library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(tidyr)
library(viridis)
library(reshape2)


#########################
##~~~~~~~~~~~~~~~~~~~~~##
##       |DATOS|       ##
##~~~~~~~~~~~~~~~~~~~~~##
#########################

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

# Añadimos el ratio de PCR positivas.s
i1 = datos_generales[,c(1,5,6)]
i1 = i1[complete.cases(i1), ]
i1$Fecha = as.Date(datos_positivos$Fecha, format = "%d/%m/%Y")
i1$ratio_positivos = i1$positivos_diarios/ i1$PCR_diarios
i1 = i1[, c(1,4)]
datos_generales = merge(x=datos_generales, y=i1, by='fecha', all.x = T, )

datos_generales$positivos_diarios = replace(datos_generales$positivos_diarios,
                                            is.na(datos_generales$positivos_diarios),
                                            0)

########################
##~~~~~~~~~~~~~~~~~~~~##
##     |GRÁFICOS|     ##
##~~~~~~~~~~~~~~~~~~~~##
########################

p2 <- ggplot(data=datos_generales, aes(x=PCR_diarios, group=positivos_diarios, fill=positivos_diarios)) +
  geom_density(adjust=1.5, alpha=.4) +
  theme_ipsum()

p <- datos_generales %>%
  ggplot( aes(x=fecha, fill=positivos_diarios)) + # ERROR EN FILL!!
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")
p

summary(datos_generales) 

p = datos_generales %>%
ggplot( aes(x=fecha, y=c(PCR_diarios, ))) +
  geom_line() +
geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.5)


datos_generales.long <- melt(datos_generales,
                             id = "fecha",
                             measure = c("PCR_diarios", "positivos_diarios"))
ggplot(datos_generales.long,
       aes(fecha, value, colour = variable)) +
       geom_line() +
       geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.5)

ggplot(datos_generales, aes(x=fecha)) + 
  geom_line(aes(y = positivos_diarios), color = "darkred") + 
  geom_line(aes(y = PCR_diarios), color="steelblue", linetype="twodash") 





#######################
##~~~~~~~~~~~~~~~~~~~##
##     |GRÁFICO|     ##
##      |FINAL|      ##
##~~~~~~~~~~~~~~~~~~~##
#######################
# Value used to transform the data
coeff <- max(datos_generales$PCR_diarios, na.rm = TRUE) / 
         max(datos_generales$positivos_diarios, na.rm = TRUE)

# A few constants
color_PCR <- rgb(0.4, 0.6, 0.9, 1)
color_pos <- "#D62246"

ggplot(datos_generales, aes(x=fecha)) +
  
  geom_line( aes(y=PCR_diarios / coeff), size=1.5, color=color_PCR) + 
  geom_line( aes(y=positivos_diarios), size=1.5, color=color_pos) +
  
  scale_y_continuous(
    # Features of the first axis
    name = "PCR diarios",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Positivos diarios")
  ) + 
  theme_ipsum() +
  theme(
    axis.title.y = element_text(color = color_PCR, size=15),
    axis.title.y.right = element_text(color = color_pos, size=15)
  ) +
  ggtitle("Pruebas PCR y número de positivos darios (a escala)")

# Dudas:
# 1.- Forma de hacer el grafico smooth.
# 2.- Añadir hitos (desconfinamiento, vacaciones, último estado de alarma).
# 3.- Mejor representación del eje X (mes a mes).
