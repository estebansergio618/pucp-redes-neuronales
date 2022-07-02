
#ARIMA en Rstudio
#Cargar librerias

library(tseries)
library(astsa)
library(forcast)
library(dplyr)
library(lubridate)
library(foreign)
library(quantmod)

dataset = read.csv('petroleo.csv',sep = ';')
head(dataset)

ts.plot(dataset$Precio)

tsdata = dataset %>% select(Precio)

tsdata =ts(dataset$Precio,start = c(2013,1),frequency = 12)

ts.plot(tsdata)

str(tsdata)

print(tsdata)


# del plot observamos que la serie no es estacionaria (verificamos con test df)
# H0: no es estacionatia
# H1: es estacionaria


adf.test(tsdata)

# atenuamos la varianza con log

serielog = log(tsdata)
serielog
plot(serielog, main = 'log precios')

# test df

adf.test(serielog)

########################################
### Calculando primeras diferencias
########################################

seriedif = diff(tsdata)
seriedif
plot(seriedif, main = 'd precios')

# test df
adf.test(seriedif)


# segun el test df aún no es estacionario, por lo que hacemos una segunda diferencia
# es decir diferenciamos la serie diferenciada


seriedif2 = diff(tsdata, differences = 2)
seriedif2
plot(seriedif2, main = 'd2 precios')

# test df
adf.test(seriedif2)


########################################
### Autocorrelación simple y parcial
##########################################

par(mfrow=c(2,1))
acf(seriedif2)
pacf(seriedif2)
acf(ts(seriedif2, frequency=1))
pacf(ts(seriedif2, frequency=1))


#Modelo Arima

modelo1=arima(tsdata,order=c(1,2,1))
modelo1
tsdiag(modelo1)
Box.test(residuals(modelo1),type="Ljung-Box")


error=residuals(modelo1)
plot(error,main = 'errores del modelo')
hist(error)

#Pronosticos Arima
par(mfrow=c(1,1))
pronostico=forecast::forecast(modelo1,h=10)
pronostico
plot(pronostico)

#Modelo Arima2

modelo2=arima(tsdata,order=c(1,2,2))
modelo2
tsdiag(modelo2)
Box.test(residuals(modelo2),type="Ljung-Box")


error=residuals(modelo2)
plot(error,main = 'errores del modelo')
hist(error)

#Pronosticos Arima2

pronostico2 = forecast::forecast(modelo2,h=10)
pronostico2
plot(pronostico2,main ='modelo2')


