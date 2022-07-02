
getwd()

library(tseries)
#install.packages('astsa')
library(astsa)
library(forcast)
library(dplyr)
library(lubridate)
library(foreign)
library(quantmod)
install.packages('fpp2', dependencies = TRUE)
library(fpp2)


#Note que los modelos SARIMA especificados como ARIMA(p, d, q)(P, D, Q) m son modelos ARIMA con un gran numero de restricciones.
#donde (P, D, Q)m denota la parte estacional del modelo.


#Procedimiento general

#Importar los datos de la libreria fpp2
data(euretail)
euretail

#Realiza la grafica de serie de tiempo
plot.ts(euretail, main="Euretail")

# verificamos que la serie no es estacionaria H0 : no es estacionaria
adf.test(euretail)


#Comando para saber cuantas diferenciaciones se requieren para un SARIMA

ndiffs(euretail)    # diferencia regular
nsdiffs(euretail)   # diferencia estacional


acf(euretail)   # vemos que esta serie no es estacionaria por la forma del plot acf
pacf(euretail)

#Solicitar el mejor modelo de ARIMA
#La funcion auto.arima() calcula el mejor modelo ARIMA(p, d, q) de acuerdo a diferentes criterios: AIC, AICC o BIC value.


model = auto.arima(euretail,stepwise=FALSE,approximation=FALSE) 

#Resumen del Modelo SARIMA
summary(model)
residuals=resid(model)
plot(residuals, main="Residuals", col="Blue")
adf.test(residuals)

#SARIMA(0,1,3)(0,1,1)[4] 
#Pronóstico del modelo

pronostico = forecast(model,h=10)
ggseasonplot(euretail, main="Plot SARIMA") 
plot(pronostico)

# ajustar la propuestaautoarima
modelo2 = Arima(euretail,order=c(0,1,3),seasonal =list(order = c(0,1,1),period(4)))
summary(modelo2)


