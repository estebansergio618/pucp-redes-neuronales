library(lubridate)
library(dplyr)
library(facet_wrap)
library(tidyquant)
library(ggplot2)

link = 'https://docs.google.com/spreadsheets/d/e/2PACX-1vQtp6Pm6kSlFrgFOy5pOrsX95PPNhp2uO0u2g0IxSNesnQkWV950ydewWje2UpoxP2eyxhBm6eEA_gT/pub?output=csv'
BaseDeDatos = read.csv(link, sep = ',')

#PRE CONDICIONES DEL EJERCICIO
df = BaseDeDatos %>% 
  filter(Activo_id == 16) %>%
  select(Activo_id,s3,s4,s14,s15) 

#COLUMNAS QUE INICIAN EL 2010-1-1
df$Fecha = seq(as.Date('2010/1/1'), by = 'day',length.out = 209)

#ELIMINO LA COLUMNA ACTIVO_ID PUES YA LA SELECCIONÉ
df$Activo_id = NULL

#PERMUTO LA FECHA
df = df[,c(5,1,2,3,4)]

# APLICO UN MELT (EQUIV A TABLA DINÁMICA PARA HACER MULTIPLES GRÁFICAS)
df = reshape2::melt(df, id = 'Fecha')

ggplot(data = df,aes(x=Fecha, y=value))+
  geom_line(color = 'skyblue') + 
  #GRAFICA TENDENCIA
  geom_smooth(formula = y ~ x,method = lm) +
  # HACE MULTIPLES GRAFICAS CON TODOS LOS SENSORES
  facet_wrap(~variable, scales='free') + 
  labs(title = "Evolución y Tendencia de sensores",
       subtitle = "Desde 01-Jan - 2010",
       y = "Fecha", x = "Valor emitido")
