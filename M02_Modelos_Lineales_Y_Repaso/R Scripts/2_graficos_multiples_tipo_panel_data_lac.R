#Cargando las librerias

library(openxlsx)
library(lubridate)
library(dplyr)
library(dygraphs)
library(reshape2)
library(ggplot2)
library(readxl)

url = 'data lac.xlsx'
data_lac <- read_excel(url)
head(data_lac)

# cuantas fabricas hay en la columna fabrica

unique(data_lac$Fabrica)
unique(data_lac$Seccion)
unique(data_lac$Linea)

# remplazar un valor por otro en una columna

data_lac$Linea[data_lac$Linea=='Linea Baby 7'] = 'Linea Baby 5'

unique(data_lac$Linea)

#selecciono las variables de interés costo por linea

data_lac$año = year(data_lac$FechaProgramacion)
data_lac$mes = month(data_lac$FechaProgramacion)

# escogemos las variables de interés

datos = data_lac %>% 
  select(año,mes,Linea,Costo) %>% 
  group_by(fec = paste(año,mes),Linea) %>% 
  summarise(Costo_mes = sum(Costo))

# dinamizamos la columna Linea para que se vuelvan columnas

ds = dcast(datos, fec ~ Linea)

# llenamos con 0 los NA
ds[is.na(ds)] = 0

install.packages('tidyr')
library(tidyr)  # para separar añomes de fec

ds2 = ds %>%
  separate(fec, c("año", "mes"), " ") %>% 
  arrange(as.numeric(año),as.numeric(mes)) %>% 
mutate(periodo = seq(as.Date("2018/05/01"),
                     as.Date("2020/01/01"),
                  by='month'))

# el periodo anterior equivale a 

ds2$periodo = seq(as.Date("2018/05/01"),
                  as.Date("2020/01/01"),
                  by='month')

#(usando las funciones naturales de R)

ds2$año=NULL
ds2$mes=NULL

# la función inversa de la tabla dinámica
melt1 = melt(ds2,id.vars = 'periodo')


# ploteamos

ggplot(data = melt1, aes(x=periodo,y=value))+
  geom_line()+
  facet_grid(.~variable)+
  theme(legend.position='bottom',
        axis.text.x=element_text(angle=90,hjust=1))


ggplot(data = melt1, aes(x=periodo,y=value))+
  geom_line()+
  facet_grid(variable~.,scales = 'free_y')+
  theme(legend.position='bottom',
        axis.text.x=element_text(angle=90,hjust=1))

ggplot(data = melt1, aes(x=periodo,y=value))+
  geom_line()+
  facet_wrap(.~variable,scales = 'free_y',ncol = 3)+
  theme(legend.position='bottom',
        axis.text.x=element_text(angle=90,hjust=1))+
  scale_x_date(date_labels ='%Y %b',breaks ='1 months')+
  labs(title = "Series de tiempo de marcas")+
  geom_hline(yintercept = 90000, col ='red')



