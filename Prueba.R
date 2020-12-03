rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
dir()
cat("\014")
library("rjson")
datos = read.csv("Datos/covid19-pcr-positives.csv",stringsAsFactors = FALSE)

summary(datos)

print(result)
