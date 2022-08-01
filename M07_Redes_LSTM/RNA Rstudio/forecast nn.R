library(quantmod)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)



acciones = read.csv('NTDOY.csv')
acciones <- acciones[,c(1,6)]

tail(acciones,1)

hoy <- today()-1

ts.plot(acciones$Precio)

names(acciones) = c('Fecha','Precio')
acciones$Fecha = as.Date(acciones$Fecha)

rango_fecha = (hoy+1):(hoy+30)
Precio = as.numeric(NA)

rango_fecha = as.data.frame(cbind(Precio,rango_fecha))

rango_fecha$rango_fecha = as.Date(rango_fecha$rango_fecha)
names(rango_fecha)[2] = 'Fecha'

acciones<- rbind(acciones,rango_fecha)

acciones$Fecha_dup=acciones$Fecha

acciones = acciones %>% 
  separate(Fecha,c('Año','Mes','Dia'))

acciones$Año = as.numeric(acciones$Año)
acciones$Mes = as.numeric(acciones$Mes)
acciones$Dia = as.numeric(acciones$Dia)

# escalando fecha

Fecha.sc = scale(acciones[,c('Año','Mes','Dia')])

set.seed(1)

acciones.sc = acciones %>% select(Fecha_dup,Precio)
acciones.sc = cbind(acciones.sc,Fecha.sc)
names(acciones.sc)[1] = 'Fecha'

train = createDataPartition(na.omit(subset(acciones,
                                            acciones$Fecha_dup<today()))$Precio,
                              p=0.7,list = F)


test = rbind(acciones[-train,],subset(acciones,
                                     acciones$Fecha_dup>=today()))                             
                             

# creamos el df test escalado

test.sc = test %>% 
  select(Fecha_dup,Precio) 

names(test.sc)[1]='Fecha'

ftsc = scale(test[,c('Año','Mes','Dia')]) 

test.sc = cbind(test.sc,ftsc)

# cargamos librerias

install.packages('neuralnet')
install.packages('NeuralNetTools')

library(neuralnet)
library(NeuralNetTools)


mod = neuralnet(formula = Precio~Año+ Mes+ Dia, 
                data = acciones.sc[train,],hidden = 2,
                threshold = 0.01,stepmax = 1e+7, 
                rep = 1,linear.output=TRUE)

summary(mod)
pred = compute(mod,test.sc)

pred$neurons
pred$net.result  

plotnet(mod)

datos = cbind(pred$net.result,test.sc)

rmse = RMSE(datos$Precio,datos$`pred$net.result`,na.rm=T)  
rmse  

ggplot()+
  geom_line(data =datos,aes(x=Fecha,y=Precio),color='blue') +
  geom_line(data=datos,aes(x=Fecha,y=pred$net.result),color='red')
  
