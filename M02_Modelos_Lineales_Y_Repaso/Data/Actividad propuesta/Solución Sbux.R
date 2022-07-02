
# importamos la data

link ='https://docs.google.com/spreadsheets/d/e/2PACX-1vSYyr2OdmQAqispfFuAmRHXpKnP2NxmZw3Mzs3Pi3YkBWQ0UouNG-fn2BJjmUIOYDN0mMQSx6HDyWyM/pub?gid=1385766601&single=true&output=csv'

dataset = read.csv(link, sep = ';')

# primera forma de convertir a fecha
dataset$Date = as.Date(dataset$Date,"%m/%d/%Y" )

library(dplyr)
library(dygraphs)
library(lubridate)

data = dataset %>% 
  select(Adj.Close)

tsdata = ts(data, start = c(1993,3), frequency = 12)

dygraph(tsdata)

# agregamos etiquetas

dygraph(tsdata,main="Adj. Close",
        xlab ="periodo",
        ylab = "millones de soles") %>%
  dyOptions(fillGraph = T,fillAlpha = 0.1,
            drawPoints = T,pointSize = 3,
            pointShape = "triangle",gridLineColor = "blue")

# el selector de posición se resalta

dygraph(tsdata,main="Adj. Close",
        xlab ="periodo",
        ylab = "millones de soles") %>%
  dyOptions(fillGraph = T,fillAlpha = 0.04,
            drawPoints = T,pointSize = 3,
            pointShape = "star",gridLineColor = "blue")%>%
  dyHighlight(highlightCircleSize = 8, # tamaño del indicador
              highlightSeriesBackgroundAlpha = 1, # transparencia
              hideOnMouseOut = F, # según el mouse
              highlightSeriesOpts = list(strokeWidth = 3))

# selector de fecha

dygraph(tsdata,main="Evolución de las Exportacioes",
        xlab ="periodo",
        ylab = "millones de soles") %>%
  dyOptions(fillGraph = T,fillAlpha = 0.04,
            drawPoints = T,pointSize = 3,
            pointShape = "star",gridLineColor = "blue")%>%
  dyHighlight(highlightCircleSize = 8,
              highlightSeriesBackgroundAlpha = 1,
              hideOnMouseOut = F,
              highlightSeriesOpts = list(strokeWidth = 3))%>%
  dyRangeSelector(dateWindow = c("2000-01-01","2008-01-01"))

# sombreamos periodos de tiempo

graficodinamico = dygraph(tsdata,main="Evolución de las Exportaciones",
                          xlab ="periodo",
                          ylab = "millones de soles") %>%
  dyOptions(fillGraph = T,fillAlpha = 0.04,
            drawPoints = T,pointSize = 3,
            pointShape = "star",gridLineColor = "blue")%>%
  dyHighlight(highlightCircleSize = 8,
              highlightSeriesBackgroundAlpha = 1,hideOnMouseOut = F,
              highlightSeriesOpts = list(strokeWidth = 3))%>%
  dyRangeSelector ()%>%
  dyAnnotation("2006-01-01",text = "IE",tooltip = "Max valores")%>%
  dyShading(from = "2006-02-01", to = "2006-12-01",color = "pink")%>%
  dyShading(from = "1998-02-01", to = "1998-12-01",color = "lightblue")%>%
  dyEvent("2016-01-01","inicio recesivo",labelLoc = "top")%>%
  dyLegend(show="follow")



