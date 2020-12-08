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

# Sustituimos valores NA por 0 (pensando en los gráficos).
datos_generales$positivos_diarios = replace(datos_generales$positivos_diarios,
                                            is.na(datos_generales$positivos_diarios),
                                            0)


#######################
##     |CAMBIOS|     ##
#######################

# Normalizar las columnas PCR_diarios y positivos_diarios y añadirlas al dataframe.
datos_generales$PCR_diarios_normalizado = normalize(datos_generales$PCR_diarios,
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

############################
##     | FIN CAMBIOS|     ##
############################

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
       geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=1.5)

ggplot(datos_generales, aes(x=fecha)) + 
  geom_line(aes(y = positivos_diarios), color = "darkred") + 
  geom_line(aes(y = PCR_diarios), color="steelblue", linetype="twodash") 



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
# TODO: añadir fechas importantes con sus respectivos labels en un dataframe.
dates_vline = as.Date("05/08/2020", "%d/%m/%Y")
dates_vline = which(datos_generales$fecha %in% dates_vline)

ggplot(datos_generales, aes(x=fecha)) +
  
  geom_line( aes(y=PCR_diarios_normalizado), size=1.5, color=color_PCR) + 
  geom_line( aes(y=positivos_diarios_normalizado), size=1.5, color=color_pos) +
  
  scale_y_continuous(
    # Features of the first axis
    name = "PCR diarios",
    
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
              col = "black", lwd=0.5 ) + 
  annotate(geom="text", x = as.Date("05/08/2020", "%d/%m/%Y")+17,
           y=77,
           label="Inicio cuarentena",
           color="black"
           )
  # Buen intento, pero el texto no es lo suficientemente legible... :C
  #geom_text(aes (x = as.Date("05/08/2020", "%d/%m/%Y")+5,
  #               y = 70,
  #               label = "Inicio\ncuarentena"),
  #          color = "black",
  #          size = 3, angle = 0, fontface = "plain"
  #          )


# Dudas/mejoras:
# 0.- --> normalizar datos. DONE
# 1.- Forma de hacer el grafico smooth. --> aislar fines de semana (unir viernes con lunes). DONE.
# 2.- Añadir hitos (desconfinamiento, vacaciones, último estado de alarma). --> mejor fuera de R. Mirar por si acaso. DONE.
# 3.- Mejor representación del eje X (mes a mes). --> visualización mes a mes individual. DONE
# 4.- Fecha en formato incorrecto. --> who knows. NO AFECTA. DONE.

# Trabajo futuro:
# 1.- Dataset de infectados por género, edad, etc. y mortalidad/casos graves.
#   1.1 - Limpieza, enriquecimiento, etc.
#   1.2 - Matriz de correlación y estudio de resultados.
# 2.- Estudiar la animación del heat-map de infectados por municipio en Euskadi.
#   2.1 - Generar mapa de calor de cada día.
#   2.2 - Guardar cada mapa.
#   2.3 - Animarlo en un programa externo.
