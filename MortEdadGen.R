rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
dir()
cat
library(ggplot2)
library(ggpubr)
library(stringr)
theme_set(theme_pubr())


#Adquirir datos
datos_generales = read.csv("Datos/03.csv",stringsAsFactors = FALSE, sep=';')[1:12,1:17]

#Cambiar nombres
names(datos_generales)[1] <- "Edad"
names(datos_generales)[3] <- "PositivosM"
names(datos_generales)[4] <- "PositivosH"
names(datos_generales)[16] <- "LetalM"
names(datos_generales)[17] <- "LetalH"

#hacer datasets indviduales para los ggplots
datos_generoEd = datos_generales[1:10,c(1,3,4)]
muerte_genEd = datos_generales[1:10,c(1,16,17)]
#cambiar string a Int cambiando , a .
muerte_genEd$LetalH = str_replace(muerte_genEd$LetalH, "," , ".")
muerte_genEd$LetalM = str_replace(muerte_genEd$LetalM, "," , ".")
muerte_genEd$LetalH = as.double(muerte_genEd$LetalH)
muerte_genEd$LetalM = as.double(muerte_genEd$LetalM)

summary(muerte_genEd)


######################
#     Gráficos       #
######################
H=ggplot(datos_generoEd, aes(x=Edad, y=(PositivosH))) + 
  geom_bar(stat = "identity")+
  geom_bar(stat = "identity")+
  coord_flip()+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        )+
  scale_y_reverse(limits=c(10000,0))+
  
  scale_x_discrete(position = "top")
  
M=ggplot(datos_generoEd, aes(x=Edad, y=(PositivosM))) + 
  geom_bar(stat = "identity")+
  geom_bar(stat = "identity")+
  coord_flip()+
  ylim(0,10000)+
  theme(axis.title.y=element_blank())

figure <- ggarrange(H, M,
                    ncol = 2, nrow = 2)
figure

MM=ggplot(muerte_genEd, aes(x=Edad, y=(LetalM))) + 
  geom_bar(stat = "identity")+
  geom_bar(stat = "identity")+
  coord_flip()+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
  )+
  scale_y_reverse(limits=c(0.4,0))+
  
  
  scale_x_discrete(position = "top")

HM=ggplot(muerte_genEd, aes(x=Edad, y=(LetalH))) + 
  geom_bar(stat = "identity")+
  geom_bar(stat = "identity")+
  coord_flip()+
  ylim(0,0.4)+
  theme(axis.title.y=element_blank())

figureM <- ggarrange(MM, HM,
                    ncol = 2, nrow = 2)
figureM




