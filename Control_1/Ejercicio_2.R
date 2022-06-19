library(lubridate)
library(dplyr)
#install.packages('facet_wrap')
library(facet_wrap)
library(tidyquant)
library(ggplot2)
library(dygraphs)

#LECTURA DE DATOS
link = 'https://docs.google.com/spreadsheets/d/e/2PACX-1vQtp6Pm6kSlFrgFOy5pOrsX95PPNhp2uO0u2g0IxSNesnQkWV950ydewWje2UpoxP2eyxhBm6eEA_gT/pub?output=csv'
BaseDeDatos = read.csv(link, sep = ',')

#CONDICIONES PREVIAS
df = BaseDeDatos %>% 
  filter(Activo_id == 16) %>%
  select(Activo_id,s3,s4,s14,s15) 

#CREA LA SERIE DE TIEMPO DE LOS SENSORES
tsData = ts(df, start = c(2010,1,1), frequency = 12)

#GRAFICA LA SERIE DE TIEMPO DE LOS SENSORES
dygraph(tsData)

#DECORA LA GRAFICA 
dygraph(tsData,main="Evolucion de sensores",
                          xlab ="periodo",
                          ylab = "valor de sensores") %>%
  #
  #SOMBRE PARTE INFERIOR DEL GRÁFICO
  dyOptions(fillGraph = T,fillAlpha = 0.05,
            drawPoints = T,pointSize = 3,gridLineColor = "black")%>%
  #
  # COLOCA UN RANGO PARA SELECCIONAR
  dyRangeSelector(dateWindow = c("2010-01-01","2010-07-28"))  
