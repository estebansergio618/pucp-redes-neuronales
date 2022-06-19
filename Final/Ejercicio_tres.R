#------------
# EJERCICIO 3
#------------
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

#LECTURA DEL SHEETS'S
link = 'https://docs.google.com/spreadsheets/d/e/2PACX-1vT9v2BjI1jJ6Q1acHEUk48WQA9IxBDMm-Jv7nezLzF7Bmb2CR4f6IH-UOiL67GcLCERTTP1chx7g_DS/pub?output=csv'
datos = read.csv(link,sep = ',')

#GENERAR LA COLUMNA Fecha QUE EMPIEZA EN ENERO DEL 2000
datos$Fecha = seq(as.Date("2000/1/1"),by = 'month', length.out = 176)

# PASO A FORMATO MES -AÑO
datos = datos %>%
  dplyr::mutate(Fecha = tsibble::yearmonth(Fecha))

# ME ASEGURO QUE SEAN TIBBLE
datostibble = as_tibble(datos,
                        index = 'Fecha'
                        ,key='variable')

# REDIMENSIONO COMO UNA TABLA DINAMICA
mtibble = reshape2::melt(datostibble,'Fecha')


# ME VUELVO ASEGURAR
mtibble = as_tsibble(mtibble, 
                     index ='Fecha'
                     ,key='variable')

# GENERO UN MODELO PARA LOS 2
modelo = mtibble %>% 
  model(arima = ARIMA(value))

accuracy(modelo)

# ELIJO UN MES PORQUE LOS PROXIMOS VALORES SALDRAN EN EL MES SIGUIENTE
pronostico = modelo %>%
  forecast(h ='1 month')

#OBS: POR EL ENUNCIADO ENTIENDO QUE DEBO PREDECIR UN MES PARA LA COLUMNA M2 Y INPC
# PERO EN CASO SEA 2 MESES SOLO MODIFICARIAS A '2 month', TUVE COMPLICACIONES AL
# ENTEDER QUE SE QUERIA DECIR CON (y(t), y(t+1))

#MIRO SUS VALORES DE LA COLUMNA value PARA SABER CUANTO VALE
pronostico

# GRAFICO PARA QUE SE VEA BONITO EL VALOR PREDICHO
pronostico%>%
  autoplot(mtibble)

