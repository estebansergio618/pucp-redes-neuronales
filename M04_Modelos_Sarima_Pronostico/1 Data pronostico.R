


#install.packages("forecast")
#install.packages("foreign")
#nstall.packages("dygraph")
#install.packages("highcharter")
#install.packages("urca")

library(forecast)
library(foreign)
library(dygraphs)
library(highcharter)
library(urca)
library(dplyr)
library(feasts)

# borramos data pasada
rm(list = ls())
graphics.off()
cat("\014")  # control + l (limpia consola)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


# importamos librerias de forecasting y data

archivo ='data resumen.txt'
dataset = read.csv(archivo,sep ='\t')

ts.plot(dataset$EJECUTADO/1000000)

# genero una variable fecha
fecha = seq(as.Date('2018/01/01'),as.Date('2021/12/1'), by = 'month')
dataset$Fecha=fecha

# acoplamos fecha al dataset

tsdata = ts(dataset$PLANIFICADO, start=c(2018,1), frequency = 12)

#------ ploteamos la variable PLANIFICADO#-------

par(bg=c('lightblue'))
plot(tsdata/1000000,lwd = 3,
     main ='Costos Ejecutados-MM'
     ,sub='Fuente: Data - Elaboración : OBH'
     ,col='red')

# otro chart
hchart(tsdata)

library(ggplot2)
library(tidyquant)

ggplot(data = dataset,aes(x = Fecha,y = EJECUTADO/1000000)) +
  geom_line() +
  geom_smooth(aes(color ='promedio'),se =FALSE,
              method ='lm',formula = y ~ 1,show.legend=TRUE) + # agrega el promedio
  geom_smooth(method = lm,aes(color='trend')) + # tendencia de la serie de tiempo
   geom_ma(aes(color='MA(3)'),ma_fun = SMA,
          n = 3,size =1,
          show.legend=TRUE) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90,hjust = 1)) + # ejes rotados
  scale_x_date(date_labels ='%b %Y',breaks ='3 months')+  # eqtiquetas del eje X
  scale_colour_manual(name='Legenda',
                      values=c('promedio'='red',
                               "MA(3)"='green',
                               "trend"='blue'),
                      labels = c('Promedio',
                                 'Media Movil 3',
                                 'Tendencia'))+ # manipulas manualmente los colores
  
  labs(title = "Evolución histórica de los costos",
       subtitle = 'Costos planificados',
       caption = 'Elaborado por:OBH')+
  xlab("Fecha de la serie")+
  ylab('Valor de los costos')

#---- Utilizando dygraph#----------

dygraph(tsdata)

# ARIMA(1,0,0)(1,1,0)[12] 

m1 = arima(tsdata,order = c(1,0,0),seasonal = c(1,1,0,12))
summary(m1)

# pronostico

forecast(m1,h = 12)

autoplot(forecast(m1,h=12))


pronostico = forecast(m1,h = 12)

promedio = data.frame(pronostico$mean)
bajo = data.frame(pronostico$lower)
alto = data.frame(pronostico$upper)

write.csv(promedio,'promedio.csv')
write.csv(bajo,'bajo.csv')
write.csv(alto,'alto.csv')
