
library(tseries)
library(forecast)
library(astsa)
library(dplyr)
library(lubridate)
library(foreign)
library(quantmod)
library(fpp2)

data(AirPassengers)
AirPassengers
frequency(AirPassengers)

# verificando missing val

sum(is.na(AirPassengers))

plot(AirPassengers, main ='serie original')

# graficando la autocorrelación de la ts

acf(AirPassengers,lag =40)
pacf(AirPassengers,lag =40)

# Verificando estacionariedad
# H0: No es estacionaria

adf.test(AirPassengers, alternative ="stationary", k=12)

# graficando la tendencia

plot(AirPassengers,col=1)+abline(reg=lm(AirPassengers~time(AirPassengers)),col=2) 

# verificando la tendencia general

plot(aggregate(AirPassengers,FUN = mean),col=3)

#Show seasonality
#This boxplot will show seasonality
boxplot(AirPassengers~cycle(AirPassengers))

cycle(AirPassengers)

# descomponiendo la serie
#Decomposition break data into trend, seasonal, regular and random
plot(decompose(AirPassengers)) # time series decomposition


# La serie no es estacionaria porque tiene tendencia en media y varianza 
# atenuamos la varianza aplicando logaritmos

lp = log(AirPassengers)
plot(lp, main ='log pasajeros')
acf(lp,lag =40)
pacf(lp,lag =40)

# atenuamos la varianza pero sigue la tendencia

dlp = diff(log(AirPassengers))
plot(dlp, main ='dif log pass') 
acf(dlp,lag = 40,col=2)
pacf(dlp,lag =40,col=2)

# verificamos que la serie es estacionaria

adf.test(dlp,alternative = 'stationary')

# en el correlograma acf de dlp vemos que existe estacionalidad
# calculamos la primera diferencia estacional de AirPassengers


sd.log.p = diff(lp,lag = 12)
plot(sd.log.p)

# verificamos estacionariedad

adf.test(sd.log.p,alternative = 'stationary')

# con la primera diferencia aún no es estacionaria

s2d.log.p = diff(sd.log.p)
plot(s2d.log.p)
adf.test(s2d.log.p,alternative = 'stationary')
acf(s2d.log.p,lag = 40,col=2)
pacf(s2d.log.p,lag =40,col=2)



### Predicciones en base a correlogramas escogemos parametros de la parte regular

fit = arima(log(AirPassengers),c(0,1,1),seasonal = list(order=c(1,2,1),period = 12))
fit

tsdiag(fit)

## verificamos utilizando autoarima

ARIMAmodel = auto.arima(log(AirPassengers))
ARIMAmodel

# observados vs esperados del modelo

plot(ARIMAmodel$x,col='red',type='o')
lines(fitted(ARIMAmodel),col='blue')

# predicción 10 periodos (valores logaritmicos)

pred <- predict(fit,n.ahead=10*12) #10 years * 12 months 
pred


#2.718 is e value and round them to 0 decimal 

pred1<-round(exp(pred$pred),0)  
pred1  #give op of 1960 to 1970


#line type (lty) can be specified using either text ("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash") or number (0, 1, 2, 3, 4, 5, 6). Note that lty = "solid" is identical to lty=1.

ts.plot(AirPassengers,pred1,log="y",lty=c(1,3))
ts.plot(AirPassengers,pred1,lty=c(1,3))

# comparamos valores reales vs modelo

df = data.frame(y_true = AirPassengers,model = exp(fitted(fit)))
head(df)

# revisamos residuos

qqnorm(residuals(fit))
qqline(residuals(fit))

# revisamos que los residuos no sean estacionarios
# H0: No es estacionaria

adf.test(fit$residuals, alternative ="stationary")
