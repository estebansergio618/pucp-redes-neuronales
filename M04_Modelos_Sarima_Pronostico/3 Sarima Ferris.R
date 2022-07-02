
library(tseries)
library(forecast)
library(astsa)
library(dplyr)
library(lubridate)
library(foreign)
library(quantmod)
library(fpp2)

enlace = 'https://docs.google.com/spreadsheets/d/e/2PACX-1vSaDGKb9OUwYDiiXDTcM3r11YTwllX74DWLNrROxtejbjuFqT-C4EUlEg7U5-2xkw/pub?gid=697222197&single=true&output=csv'

ds = read.csv(enlace)

head(ds)

ferris = ts(ds$venta, start = c(1964,1), frequency = 12)

ferris
frequency(ferris)
cycle(ferris)


# verificando missing val

sum(is.na(ferris))

plot(ferris, main ='serie original')

# descomponiendo la serie
#Decomposition break data into trend, seasonal, regular and random
plot(decompose(ferris)) # time series decomposition

# Verificando estacionariedad
# H0: No es estacionaria

adf.test(ferris, alternative ="stationary", k=12)

# graficando la autocorrelación de la ts

acf(ferris,lag =40)
pacf(ferris,lag =40)


#Show seasonality
#This boxplot will show seasonality

boxplot(ferris ~ cycle(ferris))


# atenuamos la tendencia

dlp = diff(ferris)
plot(dlp, main ='dif serie original') 
acf(dlp,lag = 40,col=2)
pacf(dlp,lag =40,col=2)

# verificamos que la serie es estacionaria
# H0 : no es estacionaria (si p_value < 5%, entonces rechazo H0)

adf.test(dlp,alternative = 'stationary')

# en el correlograma acf de dlp vemos que existe estacionalidad
# calculamos la primera diferencia estacional de la serie


seasonal.1d = diff(ferris,lag = 12)
plot(seasonal.1d,main ='primera diferencia estacional')

# verificamos estacionariedad de la primera diferencia estacional

adf.test(seasonal.1d,alternative = 'stationary')

# con la primera diferencia estacional es estacionaria

acf(seasonal.1d,lag = 40,col=2)
pacf(seasonal.1d,lag =40,col=2)

## verificamos utilizando autoarima

ARIMAmodel = auto.arima(ferris)
ARIMAmodel

# observados vs esperados del modelo

plot(ARIMAmodel$x,col='red',type='o')
lines(fitted(ARIMAmodel),col='blue')

### Predicciones en base a correlogramas escogemos parametros de la parte regular

fit = arima(ferris,c(1,1,1),seasonal = list(order=c(1,1,1),period = 12))
fit

tsdiag(fit)


# predicción 24 periodos 

pred = predict(fit,n.ahead = 2*12) #10 years * 12 months 
pred



#line type (lty) can be specified using either text ("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash") or number (0, 1, 2, 3, 4, 5, 6). Note that lty = "solid" is identical to lty=1.

ts.plot(ferris,pred$pred,lty=c(1,3),col=c(1,2))


# comparamos valores reales vs modelo

df = data.frame(y_true = ferris,model = fitted(fit))
head(df)

# revisamos residuos

qqnorm(residuals(fit))
qqline(residuals(fit))

# revisamos que los residuos no sean estacionarios
# H0: No es estacionaria

adf.test(fit$residuals, alternative ="stationary")
acf(residuals(fit),lag = 40,col=2)
plot(residuals(fit))
hist(residuals(fit))
