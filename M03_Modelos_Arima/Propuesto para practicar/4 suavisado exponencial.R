library(TTR)

x=seq(1,7)
x

SMA(x,n=3)

# lynx # base de número de linces capturados en canada

lynx
plot(lynx)

lynxsmoothed = SMA(lynx,n=3); lynxsmoothed
plot(lynx)
lines(lynxsmoothed,col ='red')

# n=9

lynxsmoothed = SMA(lynx,n=9); lynxsmoothed
plot(lynx)
lines(lynxsmoothed,col ='red')

# suavisado exponencial ets
library(forecast)

# funcion ets

nottem

etsmodel = ets(nottem);etsmodel

# modelo vs original

plot(nottem,lwd =3)
lines(etsmodel$fitted,col ='red')

# forecast

plot(forecast(etsmodel,h=12))

# ci = 95%

plot(forecast(etsmodel,h=12,level=95))

#holt winters multiplicativo

etsmodmul = ets(nottem,model ='MMM')

# comparamos real vs modelo

plot(nottem)
lines(etsmodmul$fitted,col ='red')
