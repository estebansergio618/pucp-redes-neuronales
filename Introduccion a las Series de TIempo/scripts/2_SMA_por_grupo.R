library(lubridate)
library(psych)
library(dplyr)
library(ggplot2)

### Cargando el dataframe ##############
ruta = "sensor_cond_fecha_hora.csv"
ds = read.csv(ruta,sep =';')
ds$fechaHora = dmy_hm(ds$fechaHora,tz=Sys.timezone())


# me quedo con los sensores de mayor variabilidad
ds2 = ds %>% 
  select(Activo_id,fechaHora,s9,s14,s4,s3)

# nos da un plot de correlaciones entra las variables num?ricas
pairs.panels(ds2 %>% select(s3,s4,s9,s14))

df = ds2 %>%
  mutate(
    fecha = floor_date(fechaHora, "day"),
    hour = floor_date(fechaHora, "3 hour"),  # cierra en la menor h
    hora = ceiling_date(fechaHora,'3 hour')  # cierra en la mayor h
     )

# agrupamos cada 3 horas los valores pormedio
data_3h = df %>% 
  select(Activo_id,s3,s4,s9,s14,hora) %>% 
  group_by(Activo_id,hora) %>% 
  summarise(s3_3 = mean(s3),
            s4_3 = mean(s4),
            s9_3 = mean(s9),
            s14_3 = mean(s14)
            )

head(data_3h)

library(tidyquant)

ggplot(data = data_3h,aes(x = hora,y = s3_3)) +
  geom_line() +
  facet_wrap(Activo_id ~.,scales='free') +  # se separa por activos
  geom_smooth(aes(color ='promedio'),se =FALSE,
              method ='lm',formula = y ~ 1,show.legend=TRUE)+ # agrega el promedio
  geom_smooth(method = lm,aes(color ='trend'))  +# tendencia de la serie de tiempo
  geom_ma(aes(color='MA(12)'),ma_fun = SMA,
         n = 8,size =1,
        show.legend=TRUE) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90,hjust = 1)) + # ejes rotados
  scale_x_datetime(date_labels ='%b %d',breaks ='1 days')+  # eqtiquetas del eje X
  scale_colour_manual(name='Legenda',
                      values=c('promedio'='red',
                               "MA(12)"='green',
                               'trens'='blue'),
                      labels = c('Promedio',
                                 'Media Movil 12',
                                 'Tendencia'))+ # manipulas manualmente los colores

labs(title = "Evoluci?n hist?rica de los valores",
     subtitle = 'Lectura de sensores',
     caption = 'Elaborado por:OBH')+
  xlab("Fecha de la serie")+
  ylab('Valor de los sensores')

#-----Creamos media y rolling  partition by
df2 = data_3h %>%
  group_by(Activo_id) %>%
  mutate(group_avg = mean(s3_3)) %>% 
  mutate(group_SMA = rollmean(s3_3, 8, fill = NA,  align = "right"))  

# ----Creamos el plot
ggplot(data = df2,aes(x = hora,y = s3_3)) +
  geom_line() +
  facet_wrap(Activo_id ~.,scales='free') +  # se separa por activos
  geom_smooth(method = lm,aes(color ='trend'))  +# tendencia de la serie de tiempo
  geom_line(aes(x = hora,y =group_avg,color ='promedio'))+
  geom_line(aes(x = hora,y =group_SMA,color ='MA(12)'),linetype='dashed')+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90,hjust = 1)) + # ejes rotados
  scale_x_datetime(date_labels ='%b %d',breaks ='1 days')+  # eqtiquetas del eje X
  scale_colour_manual(name='Legenda',
                      values=c('promedio'='red',
                               "MA(12)"='green',
                               'trend'='blue'),
                      labels = c('Promedio',
                                 'Media Movil 12',
                                 'Tendencia'))+ # manipulas manualmente los colores
  
  labs(title = "Evoluci?n hist?rica de los valores",
       subtitle = 'Lectura de sensores',
       caption = 'Elaborado por:OBH')+
  xlab("Fecha de la serie")+
  ylab('Valor de los sensores')


