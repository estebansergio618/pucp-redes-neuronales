library(feasts) 
library(ggplot2) 
library(dplyr) 
library(tidyverse) 
library(reshape2) 
library(fable) 
library(lubridate) 
library(tsibble) 
library(zoo)  
library(tsibbledata) 
library(DataExplorer) 
library(fpp3) 

# ESTA A NIVEL MES
ruta = 'https://docs.google.com/spreadsheets/d/e/2PACX-1vQrGe1t-qGFfGu-9SMzUW1MhHF8SUgIesdsiRHPxtT51zYw0UYMkdjgtOjC5mAXXCPoma9-4wz9MLcw/pub?gid=863908305&single=true&output=csv'
datos = read.csv(ruta,sep = ',')

# COMO NO ESTA Period NO ESTA EN TIPO FECHA LO CAMBIAMOS
datos$Period= as.Date(datos$Period,tryFormats = c("%d%-%m-%Y", "%d.%m.%Y"))

datos = datos %>%
  dplyr::mutate(Period = tsibble::yearmonth(Period))
  

# colocamos la fecha como indice
datostibble = as_tibble(datos,
                        index = 'Period'
                        ,key='variable')


# pasar de tibble a melt (normalizar tabla, anular dinamizacion de columnas)
mtibble = reshape2::melt(datostibble,'Period')

mtibble = as_tsibble(mtibble, 
                     index ='Period'
                     ,key='variable')

mtibble

# verificando cuantas series tenemos
unique(mtibble$variable)

#crear el modelo Values es el nombre de la variable temporal
modelo = mtibble %>% 
  model(arima = ARIMA(value))

accuracy(modelo)

# pronostico usando los modelos #
pronostico = modelo %>%
  forecast(h ='32 month')

pronostico%>%
  autoplot(mtibble)






