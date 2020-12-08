rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
dir()
cat
library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())



datos_generales = read.csv("Datos/03.csv",stringsAsFactors = FALSE, sep=';')[1:12,1:17]
names(datos_generales)[1] <- "Edad"
names(datos_generales)[3] <- "PositivosM"
names(datos_generales)[4] <- "PositivosF"

datos_generoEd = datos_generales[1:10,c(1,3,4)]

F=ggplot(datos_generoEd, aes(x=Edad, y=(PositivosF))) + 
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

figure <- ggarrange(F, M,
                    ncol = 2, nrow = 2)
figure
