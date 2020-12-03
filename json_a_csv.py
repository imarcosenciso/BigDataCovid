# Script para convertir los ficheros JSON a CSV
import pandas as pd

data = pd.read_json('Datos/covid19-pcr-positives.json', encoding = "ISO-8859-1")
data.to_csv('Datos/prueba.csv')
