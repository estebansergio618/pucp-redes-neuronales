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

link = 'data resumen.txt'
datos = read.csv(link, sep = '\t')

#Condiciones previas
datos = datos %>% select(Anio,PLANIFICADO,EJECUTADO) 

datos = datos %>%
  dplyr::mutate(fecha = tsibble::yearmonth(fecha))

# colocamos la fecha como indice
datostibble = as_tibble(datos,
                        index = 'fecha')

#Pongo fecha como primera columna
datostibble = datostibble[,c(5,1,2,3,4)]

mtibble = reshape2::melt(datostibble,'fecha')

library(fpp3)

mtibble = as_tsibble(mtibble, 
                     index ='fecha'
                     ,key='variable')

modelos = mtibble %>%
  model(
    arima = ARIMA(value)
    ,ets = ETS(value)
    ,snaive = SNAIVE(value)
  )

# ploteamos los modelos
pronostico = modelos %>%
  forecast(h='1 year')

pronostico%>%
  autoplot(mtibble)