
install.packages('feasts')
install.packages('reshape2')
install.packages('fable')
library(feasts)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(reshape2)
library(fable)
library(lubridate)
library(tsibble)
install.packages('zoo')
library(zoo)
install.packages('readxl')
library(readxl)
install.packages('tsibbledata')
library(tsibbledata)

ruta = "Month_Value_1.xlsx"
datos = read_excel(ruta)

head(datos,1)
tail(datos,1)

# generar datos tipo fecha

datos$fecha = seq(as.Date("2015/1/1"), by = "month", length.out = 64)

# fecha a formato a año_mes

ym = as.yearmon(datos$fecha)
ym

# elimino period
datos$Period = NULL
# fecha como año_mes segunda opción de formato fecha

datos = datos %>%
  #dplyr::mutate(fecha = tsibble::yearmonth(fecha))
    mutate(fecha = tsibble::yearmonth(fecha))

head(datos)

# colocamos la fecha como indice

datostibble = as_tibble(datos,
                        index = 'fecha')


# transformando tablas, anular columnas

datostibble = datostibble[,c(5,4,1,2,3)]

# pasar de tibble a melt (normalizar tabla, anular dinamizacion de columnas)

mtibble = reshape2::melt(datostibble,'fecha')

install.packages('fpp3')
library(fpp3)

mtibble = as_tsibble(mtibble, 
                    index ='fecha'
                    ,key='variable')

head(mtibble)
mtibble
class(mtibble)

# verificando cuantas series tenemos
unique(mtibble$variable)

#crear el modelo Values es el nombre de la variable temporal

modelo1 = mtibble %>% 
  model(arima = ARIMA(value))

  
modelo1

report(modelo1)

# pronostico usando los modelos #

pronostico1 = modelo1 %>%
  forecast(h ='2 year')

pronostico1%>%
  autoplot(mtibble)


accuracy(modelo1)

# multiples modelos para multiples variables

modelos2 = mtibble %>%
  model(
    arima = ARIMA(value)
    ,ets = ETS(value)
    ,snaive = SNAIVE(value)
  )

modelos2
report(modelos2)

accuracy(modelos2)

# ploteamos los modelos
pronostico2 = modelos2 %>%
  forecast(h='1 year')

pronostico2%>%
  autoplot(mtibble)


accuracy(modelos2)

# seleccionando la variable y modelo

modelos2 %>%
  filter(variable=='Sales_quantity')%>%
  select(arima)%>%
  report()


# modelo mixto (Arima, ets)

modelo3 = mtibble %>%
  model(arima = ARIMA(value),
        ets = ETS(value)) %>%
  mutate(mixto =(arima+ets)/2)


modelo3

pronostico3 = modelo3%>%
  forecast(h='1 year')
pronostico3
  
pronostico3 %>%
  autoplot(mtibble,level = NULL)

