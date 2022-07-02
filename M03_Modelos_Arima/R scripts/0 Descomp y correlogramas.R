### Plots 
# Nottem dataset
# Serie de tiempo que contiene el promedio de temperaturas en el castillo de Nottingham en grados Fahrenheit durante 20 a???os.

# Otros paquetes
library(forecast)
library(ggplot2)
library(tseries)

nottem
# notten data interna de R 
plot(nottem) 
nottem
plot(decompose(nottem))

# correr el test de dickey fuller
# H0 : serie no es estacionaria

adf.test(nottem)

# si p-value es < 0.05 entonces se rechaza H0
### ACF (simple) y PACF (parcial)

par(mfrow = c(1,1))

plot(nottem) 
acf(nottem,lag.max = 20, plot = T)
pacf(nottem, lag.max = 20, plot =T)


# otra forma de hacer la decomposición con libreria ggplot2

autoplot(decompose(nottem, type ='additive'))


# otra alternativa seasonal lenguaje natural de R
plot(stl(nottem,s.window = 'periodic'))

# extraer los componentes de una serie temporal
mynottem = decompose(nottem,'additive')

class(mynottem)

# graficando la tendencia
plot(mynottem$trend)

plot(mynottem$seasonal)

mynottem$trend











