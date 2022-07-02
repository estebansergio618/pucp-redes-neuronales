
raw_csv_data= read.csv('Index2018.txt')

raw_csv_data$date = as.Date(raw_csv_data$date
                            ,"%d/%m/%Y")

head(raw_csv_data)

tail(raw_csv_data)

# generamos fechas completas
fechas = seq(as.Date("1994-01-07"),
             as.Date("2018-01-29"),
             by ='day')
Fecha_df = as.data.frame(fechas)
colnames(Fecha_df)='date'

# cruzamos datos
library(dplyr)

data_left <- left_join(Fecha_df,
                       raw_csv_data,
                       by='date') # cruce de tablas

head(data_left)

# llenamos valores perdidos
library(tidyr)
# opciones ('up','down')
data_fill = data_left %>% fill(spx,dax,ftse,nikkei,
                               .direction = c('down'))

head(data_fill)

## elijo la variable de interes

df_comp = data_fill %>% 
  select(date,ftse) %>% 
  rename(market_value = ftse)

plot(df_comp$market_value,type ='l')

acf(df_comp$market_value,xlim=c(1,40))
pacf(df_comp$market_value,xlim=c(1,40))

# ajustando el modelo a AR = 1

modelo_ar = arima(df_comp$market_value,order = c(1,0,0))
modelo_ar

# verifico AR1

df_comp$lag1 = lag(df_comp$market_value,n=1)

# llenamos valor perdido

data_fill2 = df_comp %>% fill(lag1,
                               .direction = c('up'))

# creamos modelo lineal

modelo_lm = lm(market_value ~ lag1, data = data_fill2)
summary(modelo_lm)

# Separate regressions por activo de manera gráfica
library(ggplot2)
qplot(market_value, lag1, data = data_fill2,
      geom = c("point", "smooth"), 
      method = "lm", formula = y ~ x, color = 'red', 
      main="Regression Lineal mv vs lag1", 
      xlab="MV", ylab = "lag1"
      ,xlim=c(0,8000)
      ,ylim=c(0,8000))
