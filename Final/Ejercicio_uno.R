# MODELO ARIMA DE LA VARIABLE M2
library(feasts) 
library(ggplot2) 
library(dplyr)
library(reshape2) 
library(fable) 
library(lubridate) 
library(tsibble) 
library(zoo)  
library(tsibbledata) 
library(DataExplorer) 
library(fpp3) 
library(forecast)
library(tseries)

#LECTURA DEL SHEETS'S
link = 'https://docs.google.com/spreadsheets/d/e/2PACX-1vT9v2BjI1jJ6Q1acHEUk48WQA9IxBDMm-Jv7nezLzF7Bmb2CR4f6IH-UOiL67GcLCERTTP1chx7g_DS/pub?output=csv'
datos = read.csv(link,sep = ',')

#GENERAR LA COLUMNA Fecha QUE EMPIEZA EN ENERO DEL 2000
datos$Fecha = seq(as.Date("2000/1/1"),by = 'month', length.out = 176)

# PASO A FORMATO MES -AÑO
datos = datos %>%
  dplyr::mutate(Fecha = tsibble::yearmonth(Fecha))

#SELECCIONO SOLO LA COLUMNA M2
datos = datos %>%
  select(Fecha,M2)

# USAMOS TIME SERIES
tsData = ts(datos$M2, start = c(2000,1), frequency = 12)

# GRAFICAMOS LA SERIE DE TIEMPO
dygraphs::dygraph(tsData)

# PLOTEO SU DESCOMPOSICION Y APARENTEMENTE TIENE CIERTA ESTACIONALIDAD
autoplot(stl(tsData, s.window = 'periodic'))

# ME ASEGURO CON EL TEST DE FULLER Y ME EQUIVOCO CON LA ESTACIONALIDAD
adf.test(tsData, alternative = 'stationary')

#CONCLUIMOS QUE LA SERIE NO ES ESTACIONARIA 
diferenciaUno = diff(tsData, differences = 1)
plot(diferenciaUno)

#REALIZAMOS OTRA VEZ EL TEST DE FULLER Y ES ESTACIONARIA
adf.test(diferenciaUno, alternative = "stationary")

#CONCLUIMOS QUE AHORA SI ES ESTACIONARIA POR SU p-value < 0.05
# AHORA LA AUTOCORRELACION PUEDE AYUDAR A ELEGIR EL P O Q

#------------------
# AJUSTAMOS EL MODELO
#------------------
modelo = auto.arima(tsData, seasonal = FALSE)
modelo

# REALIZAMOS LA PREDICCION DE 12 MESES Y PLOTEAMOS, USANDO EL MODELO AJUSTADO
prediccion = forecast(modelo, h = 12)

# PLOTEO LA PREDICCION
plot(prediccion)
