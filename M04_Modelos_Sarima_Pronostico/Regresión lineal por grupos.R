
# cargamos las librerias que utilizaremos en este script

# install.packages('dplyr')

library(dplyr)
library(ggplot2)
library(broom)
library(tidyr)
library(lme4)
require(tigerstats)

# importar nuestra data a analizar

ruta ='https://docs.google.com/spreadsheets/d/e/2PACX-1vQ53-PM7umRSgv5Or44DDnz3QnsctvaM9nx1RQnXrbZgMDJL_W0qjEIYTtr4N_dmv0JkM_rKdn4RYhD/pub?gid=1391463000&single=true&output=csv'
dataset = read.csv(ruta,sep=';')
dataset$Periodo = NULL
head(dataset)

# verificando cuantos lineas tenemos

unique(dataset$Activo)
dataset %>% count(Activo)

# Ajustar el modelo de regresión lineal simple 
# S2 = y  , S1 = X
# alt + 126 : ~ "depende"

modelo1 = lm(S2 ~ S1, data = dataset)
summary(modelo1)


tidy(modelo1)
glance(modelo1)

# y = mx+b
# vamos a plotear las variables

plot(dataset$S1,dataset$S2, pch=19)
abline(modelo1,lwd=3,col='red')

# cambio el tipo de variable de texto a factor
dataset$Activo = as.factor(dataset$Activo)

# agrupamos los datos por activo
plot(dataset$S1,dataset$S2,
     col = dataset$Activo, pch =19)

# Separate regressions por activo de manera gráfica
qplot(S1, S2, data = dataset,
      geom = c("point", "smooth"), 
      method = "lm", formula = y ~ x, color = as.factor(Activo), 
      main="Regression Lineal S1 vs S2", 
      xlab="S1", ylab = "S2")

# otra gráfica 3 subplots por grupo de activo

dataset %>%
  ggplot()+
  aes(x = S1,y = S2,group=1)+
  geom_point(size = 1L,color ='red') +
  xlab('s1') + 
  ylab('s2') +
  ggtitle('Regresión lineal') +
  facet_wrap(vars(Activo),scales='free_y')+
  geom_smooth(method='lm',color = 'black')



# modelo lineal múltiple con factor

fitted_models = dataset %>%
  group_by(Activo) %>% 
  do(model = lm(S2 ~ S1, data = .))

summary(fitted_models)
fitted_models$model


### ---- otra libreria

#install.packages('tigerstats')

#xyplot(S2 ~ S1, groups = Activo, data=dataset, type='l')
xyplot(S2 ~ S1, groups = Activo, data = dataset)

fits <- lmList(S2 ~ S1 | Activo, data = dataset)
fits

