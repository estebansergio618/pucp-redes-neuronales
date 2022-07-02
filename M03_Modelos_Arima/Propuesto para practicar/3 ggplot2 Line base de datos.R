#----- Gráfico de barras usando ggplot2 #-----

#el flujo es el siguiente:

#data,estéticas,geometrías,complementos

#Cargando la información

library(openxlsx)
library(ggplot2)
library(reshape2)
library(lubridate)
library(dplyr)


datos <- read.xlsx("BASE DE DATOS.xlsx",
                   sheet = "Hoja1",detectDates = T)

#Estructura de ggplot2 (paso a paso)

#data
#estéticas
#geometrías (x, y, alpha, color, group, linetype, size)
#complementos (titulos, subtitulos, escalas, temas, lineas de intercepción,
#colores, estadísticas, regresiones, etc)


ggplot(data=datos)+
  aes(x=PERIODO)+
  aes(y=EXPORTACIONES)+
  geom_line()+
  geom_point(color = 'red', alpha = 0.5, size = 2)+
  geom_hline(yintercept = mean(datos$EXPORTACIONES),
             col ='grey',size = 1.5)+
  geom_vline(xintercept = as.numeric(as.Date(  # ultima fecha
    tail(datos$PERIODO,n=1))),linetype = 2)+
  geom_vline(xintercept = as.numeric(as.Date(    # barra vertical año pasado
    tail(datos$PERIODO,n=13)[1])),linetype = 4)+
  geom_vline(xintercept = as.numeric(as.Date(    # barra vertical hace 2 años
    tail(datos$PERIODO,n=25)[1])),linetype = 4)+
  annotate(geom ='text',                          # hacemos conmentario
           x=as.Date(tail(datos$PERIODO,n=3)[1]),
           y=2000000,
           label='crisis',angle=90,size=4)+
  labs(title = 'EVOLUCIÓN DE LAS EXPORTACIONES DEL ECUADOR',
       subtitle='En miles de millones',
       caption='Fuente:BCE\n Elaboración :Autor')+
  theme(text=element_text(size=14),
        legend.position='bottom',
        axis.text.x = element_text(angle=90,hjust=1,size =8))+
 # scale_x_date(date_labels='%Y %b',breaks =datos$PERIODO) # '%Y %b'año completo mes abreviado
  scale_x_date(date_labels='%Y %b',
               breaks=scales::pretty_breaks(n=12))

last_plot()
ggsave('exportaciones.png',width=8,height=5)
dev.off()
  
  
  
  
  
  
  
  
  












