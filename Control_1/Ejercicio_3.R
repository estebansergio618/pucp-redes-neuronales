#install.packages('feasts')
library(feasts)
library(ggplot2)
library(dplyr)
library(tidyverse)
#install.packages('reshape2')
library(reshape2)
#install.packages('fable')
library(fable)
library(lubridate)
library(tsibble)
#install.packages('zoo')
library(zoo)
#install.packages('readxl')
library(readxl)
#install.packages('tsibbledata')
library(tsibbledata)

#LECTURA DE BASE DE DATOS
link = 'https://docs.google.com/spreadsheets/d/e/2PACX-1vQtp6Pm6kSlFrgFOy5pOrsX95PPNhp2uO0u2g0IxSNesnQkWV950ydewWje2UpoxP2eyxhBm6eEA_gT/pub?output=csv'
datos = read.csv(link, sep = ',')

#CONDICIONES PREVIAS
datos = datos %>% 
  filter(Activo_id == 16) %>%
  select(s3,s4,s14,s15) 

#CREA COLUMNA DE FECHAS QUE EMPIEZA 2010-1-1
datos$fecha = seq(as.Date('2010/1/1'), by = 'month',length.out = 209)

#PASA A FORMATO MES AÑO
datos = datos %>%
  dplyr::mutate(fecha = tsibble::yearmonth(fecha))

# COLOCO LA FECHA COMO INDICE
datostibble = as_tibble(datos,
                        index = 'fecha')

#PERMUTO FECHA
datostibble = datostibble[,c(5,1,2,3,4)]

#REALIZO MELT (EQUIV TABLA DINÁMICA EXCEL)
mtibble = reshape2::melt(datostibble,'fecha')

library(fpp3)
#ME RE ASEGURO QUE FECHA ES INDICE
mtibble = as_tsibble(mtibble, 
                     index ='fecha'
                     ,key='variable')

#GENERO LOS MODELOS (SIN ANALISIS PREVIO)
#ARIMA
#SUAVIZACION EXPONENCIAL
#SNAIVE
modelos = mtibble %>%
  model(
    arima = ARIMA(value)
    ,ets = ETS(value)
    ,snaive = SNAIVE(value)
  )

# REALIZO PRONOSTICO DE UN AÑO
pronostico = modelos %>%
  forecast(h='1 year')

# GRAFICO PRONOSTICO DE LOS 4 SENSORES
pronostico%>%
  autoplot(mtibble)
