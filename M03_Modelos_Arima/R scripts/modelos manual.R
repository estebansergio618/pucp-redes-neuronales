
# importamos la data

link = 'https://docs.google.com/spreadsheets/d/e/2PACX-1vRTlQpgN-Z-KDhwRjOk0-NZPIeKYvtFcigVtNDFPX8s_vZt9GtfHne6l9TDov9dhQ/pub?gid=1369788109&single=true&output=csv'
df = read.csv(link)

# cargamos librerias
# cuantos activos hay
unique(df$Activo_id)

library(dplyr)

df1 = df %>% 
  select(Activo_id,ciclo,s14) %>% 
  filter(Activo_id == 1)

plot(df1$s14, type='l')


# creamos un lag

df1$lag1 = lag(df1$s14, n = 1)
head(df1)

# creamos el VF valor futuro(t+1)

df1$VF = lead(df1$s14, n = 1)
head(df1)

# creamos una variable de primera diferencia

df1$diff1 = df1$s14 - df1$lag1

library(TTR)
# ahora creamos un SMA w=4

df1$MA4 = SMA(df1$s14,n=4)
head(df1)

tail(df1)

# punto para data entrenamiento y test

split_point = round(192*.98,0)
split_point

data_train = df1[df1$ciclo<split_point,]
data_test = df1[df1$ciclo>=split_point,]

head(data_train)

library(tidyr)
# imputamos datos en data_train
data_train = data_train %>% fill(lag1,diff1,MA4,
                               .direction = c('up'))

head(data_train)

library(xgboost)
modelo = xgboost(data = as.matrix(data_train[,c('s14',
                                           'lag1',
                                           'diff1',
                                           'MA4')]),
                     label = data_train$VF, 
                     nrounds = 30)

## dataset para predecir

d_pred = data_test %>% 
  select(s14,lag1,diff1,MA4)

# predicciones

y_pred = predict(modelo,newdata = as.matrix(d_pred))
y_pred
length(y_pred)
y_pred[5]

# ploteamos lo predico vs real

plot(data_test$ciclo,data_test$VF,type='l')
lines(data_test$ciclo,y_pred,col='red')
