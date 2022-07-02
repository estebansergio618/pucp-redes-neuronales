# library(stats)

# El argumento ar es el coeficiente ar de la ecuación modelo
# los coeficientes alfa 1 deben ser en valor aboluto menores a 1

# ar = 0.6
# la FACP es positiva
# AFC decrece lentamente a cero

ar1 = arima.sim(list(order = c(2,0,0),
                     ar = c(0.6,-0.4)), n=100)


par(mfrow=c(3,1))
plot(ar1)
acf(ar1,xlim=c(1,24))
pacf(ar1,xlim=c(1,24))

# ar = -0.6 negativo
# la FACP es negativa
# AFC se comporta de forma alternativa

out1 = arima.sim(list(order=c(1,0,0),
                     ar= 0.9), n=100)


par(mfrow=c(1,1))
plot(out1)
acf(out1,xlim=c(1,24))
pacf(out1,xlim=c(1,24))

# modelo ar

ar2 = arima.sim(list(order=c(1,0,0),
                     ar=0.8), n=100)


par(mfrow=c(3,1))
plot(ar2)
acf(ar2)
pacf(ar2)

###

ar3 = arima.sim(list(order=c(1,0,0),
                     ar=0.9), n= 100)

plot(ar3)
acf(ar3)
pacf(ar3)
###

ar4 = arima.sim(list(order=c(1,0,0),
                     ar=0.99), n=100)

plot(ar4)
acf(ar4)
pacf(ar4)

## modelos MA

ma1 = arima.sim(list(order=c(0,0,1),
                     ma = 1.2), n=100)

par(mfrow=c(3,1))
plot(ma1)
acf(ma1,xlim=c(1,24))
pacf(ma1,xlim=c(1,24))


## modelos ARMA (1,1)

arma =arima.sim(list(order=c(1,0,1),
                     ar = 0.6,
                     ma = 1.2), n=100)
par(mfrow=c(3,1))
plot(arma)
acf(arma,xlim=c(1,24))
pacf(arma,xlim=c(1,24))


## modelo ARIMA

arima = arima.sim(list(order = c(1,1,1),
                     ar = 0.45,
                     ma = 1.2), n=100)
par(mfrow=c(3,1))
plot(arima)
acf(arima,xlim=c(1,24))
pacf(arima,xlim=c(1,24))
